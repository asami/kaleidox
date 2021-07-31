package org.goldenport.kaleidox.lisp

import scalaz._, Scalaz._
import org.simplemodeling.model._
import org.goldenport.event._
import org.goldenport.statemachine._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import LispFunction.CursorResult
import org.goldenport.kaleidox.model.sexpr._
import org.goldenport.kaleidox.model.diagram._

/*
 * @since   May.  5, 2021
 *  version May. 22, 2021
 *  version Jun. 30, 2021
 * @version Jul. 11, 2021
 * @author  ASAMI, Tomoharu
 */
object KaleidoxFunction {
  trait KaleidoxEvalFunction extends CursorEvalFunction {
    def eval(c: LispContext): CursorResult = eval(c.asInstanceOf[Context])

    def eval(c: Context): CursorResult

    protected final def issue_event(c: Context)(p: Event) = {
      c.statemachineSpace.issueEvent(p)
      SEvent(p)
    }
  }

  val functions = Vector(
    Event.Issue,
    Event.Call,
    StateMachine.New,
    StateMachine.Diagram
  )

  object Event {
    case object Issue extends KaleidoxEvalFunction {
      val specification = FunctionSpecification("event-issue",
        param_argument("name")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('name)
      } yield name.map(_issue(c))

      private def _issue(c: Context)(name: String): SExpr = {
        c.universe.model.eventModel.createOption(name).
          map(issue_event(c)).
          getOrElse(SError.notFound("Event", name))
      }
    }

    case object Call extends KaleidoxEvalFunction {
      val specification = FunctionSpecification("event-call",
        param_argument("name"),
        param_argument("to")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('name)
        to <- c.param.takeString('to)
      } yield (name |@| to)(_call(c))

      private def _call(c: Context)(name: String, to: ObjectId): SExpr = {
        c.universe.model.eventModel.createCallOption(name, to).
          map(issue_event(c)).
          getOrElse(SError.notFound("Event", name))
      }
    }
  }

  object StateMachine {
    case object New extends KaleidoxEvalFunction {
      override protected def is_defined_at(p: LispContext): Boolean = super.is_defined_at(p)

      val specification = FunctionSpecification("statemachine-new",
        param_argument("statemachine"),
        param_argument_option("resource-id")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('statemachine)
        resourceid <- c.param.getString(Symbol("resource-id"))
      } yield (name |@| resourceid)(_statemachine_new(c))

      private def _statemachine_new(c: Context)(
        name: String,
        resourceid: Option[ObjectId]
      ): SExpr = {
        _spawn(c, name, resourceid).
          map(_statemachine_new).
          getOrElse(SError.notFound("StateMachine", name))
      }

      private def _spawn(
        c: Context,
        name: String,
        resourceid: Option[ObjectId]
      ) =
        resourceid.
          map(c.statemachineSpace.spawnOption(name, _)).
          getOrElse(c.statemachineSpace.spawnOption(name))

      private def _statemachine_new(p: StateMachine) = SStateMachine(p)
    }

    case object Diagram extends KaleidoxEvalFunction {
      override protected def is_defined_at(p: LispContext): Boolean = super.is_defined_at(p)

      val specification = FunctionSpecification("statemachine-diagram",
        param_argument("statemachine")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('statemachine)
        resourceid <- c.param.getString(Symbol("resource-id")) // dummy
      } yield (name |@| resourceid)(_statemachine_diagram(c))

      private def _statemachine_diagram(c: Context)(
        name: String,
        resourceid: Option[ObjectId]
      ): SExpr = {
        _make_sm(c, name).
          map(_make_diagram(c, _)).
          getOrElse(SError.notFound("statemachine", name))
      }

      import org.simplemodeling.model._
      import org.simplemodeling.model.domain._
      import org.goldenport.collection.VectorMap

      class StateHanger(val states: VectorMap[String, MState]) {
        def get(name: String): Option[MState] = states.get(name) orElse _get_substate(name)

        private def _get_substate(name: String) = 
          states.values.toStream.flatMap(_.getSubStateRecursive(name)).headOption
      }
      object StateHanger {
        def create(ps: Seq[(String, MState)]): StateHanger = new StateHanger(VectorMap(ps))
      }

      private def _make_sm(c: Context, name: String) =
        c.universe.model.stateMachineModel.getClass(name).map { c =>
          val sm = MDomainStateMachine.create(name)
          val states = _states(sm, c)
          // val sms = VectorMap.empty[String, MDomainStateMachine]
          sm.setStates(states)
          sm
        }

      private def _states(
        sm: MDomainStateMachine,
        p: StateMachineClass
      ): VectorMap[String, MState] = {
        def initState = MState.initState(sm)

        def _state_(p: StateClass): MState = {
          val s = MState.create(sm, p.name)
          s
        }

        def _statemachine_state_(p: StateMachineRule): MState = {
          val s = MState.create(sm, p.name.get) // TODO
          s.subStateMap = _sub_states_map_(p)
          s
        }

        def _sub_states_map_(p: StateMachineRule): VectorMap[String, MState] = {
          val a1 = p.states.map(_state_)
          val a2 = p.statemachines.map(_statemachine_state_)
          val a = (a1 ++ a2).map(x => x.name -> x)
          VectorMap(a)
        }

        def _build_transitions(statemap: StateHanger) = {
          def _build_state(s: StateClass): Unit = {
            def _transition_(t: Transition): Option[MTransition] = {
              val g = _guard_(t.guard)
              val event = t.getEventName.map(x => MEvent(x)) // TODO share
              val action = None // MAction(sm, "???")

              def _name_transition_(p: NameTransitionTo) =
                (statemap.get(s.name), statemap.get(p.name)) match {
                  case (Some(from), Some(to)) => MTransition(sm, event, g, from, to, action)
                  case (Some(from), None) => ???
                  case (None, Some(to)) => MTransition(sm, event, g, initState, to, action)
                  case (None, None) => ???
                }

              def _history_transition_(p: HistoryTransitionTo) = {
                MTransition(sm, event, g, ???, ???, ???)
              }

              t.to match {
                case NoneTransitionTo => None
                case FinalTransitionTo => None
                case m: HistoryTransitionTo => Some(_history_transition_(m))
                case m: NameTransitionTo =>
                  if (m.name.equalsIgnoreCase(PROP_STATE_INIT))
                    None
                  else
                    Some(_name_transition_(m))
              }
            }

            val ts = s.transitions.transitions.flatMap(_transition_)
            statemap.get(s.name).foreach(_.transitions = ts.toList)
          }

          def _build_statemachine_(smr: StateMachineRule): Unit = {
            def _transition_(t: Transition): Option[MTransition] = { // TODO unify
              val g = _guard_(t.guard)
              val event = t.getEventName.map(x => MEvent(x)) // TODO share
              val action = None // MAction(sm, "???")

              def _name_transition_(p: NameTransitionTo) =
                (statemap.get(smr.name getOrElse ""), statemap.get(p.name)) match {
                  case (Some(from), Some(to)) => MTransition(sm, event, g, from, to, action)
                  case (Some(from), None) => ???
                  case (None, Some(to)) => MTransition(sm, event, g, initState, to, action)
                  case (None, None) => ???
                }

              def _history_transition_(p: HistoryTransitionTo) = {
                MTransition(sm, event, g, ???, ???, ???)
              }

              t.to match {
                case NoneTransitionTo => None
                case FinalTransitionTo => None
                case m: HistoryTransitionTo => Some(_history_transition_(m))
                case m: NameTransitionTo =>
                  if (m.name.equalsIgnoreCase(PROP_STATE_INIT))
                    None
                  else
                    Some(_name_transition_(m))
              }
            }

            smr.states.foreach(_build_state)
            smr.statemachines.foreach(_build_statemachine_)
            val ts = smr.transitions.transitions.flatMap(_transition_)
            statemap.get(smr.name getOrElse "").foreach(_.transitions = ts.toList)
          }

          p.states.foreach(_build_state)
          p.statemachines.foreach(_build_statemachine_)
        }

        def _guard_(g: SmGuard): Option[MGuard] = None // MGuard(sm)

        val (a0, initstatename) = _normalize_init(p.states)
        val a1 = a0.map(_state_)
        val a2 = p.statemachines.map(_statemachine_state_)
//        val c = Vector(MState.initState(sm), MState.finalState(sm)).map(x => x.name -> x)
//        val a3 = Vector()
//        val a4 = _resolve_init(a1 ++ a2 ++ a3)
//        val a4 = _normalize(a1 ++ a2 ++ a3)
        val a4 = _normalize_init(initstatename, a1, a2)
        val a = a4.map(x => x.name -> x)
        val states = StateHanger.create(a)
        _build_transitions(states)
        states.states
      }

      private def _normalize_init(ps: Seq[StateClass]): (Vector[StateClass], Option[String]) = {
        case class Z(
          ss: Vector[StateClass] = Vector.empty,
          initStateName: Option[String] = None
        ) {
          def r = initStateName.
            map(_explicit_init).
            getOrElse((ss, None))

          private def _explicit_init(name: String): (Vector[StateClass], Option[String]) = {
            val (ls, rs) = ss.span(_.name != name)
            rs.headOption.map { x =>
              (x +: (ls ++ rs.tail), None)
            }.getOrElse((ss, initStateName))
          }

          def +(rhs: StateClass) = {
            if (rhs.name.equalsIgnoreCase(PROP_STATE_INIT))
              copy(initStateName = _init_state_name(rhs))
            else
              copy(ss = ss :+ rhs)
          }

          private def _init_state_name(p: StateClass) =
            p.transitions.transitions.map(_.to).collect {
              case NameTransitionTo(to) => to
            }.headOption
        }
        ps./:(Z())(_+_).r
      }

      private def _normalize_init(initstatename: Option[String], states: Seq[MState], sms: Seq[MState]): Seq[MState] = {
        val ss = states ++ sms
        initstatename.map { name =>
          val (ls, rs) = ss.span(_.name != name)
          rs.headOption.map(x => x +: (ls ++ rs.tail)).getOrElse(ss)
        }.getOrElse(ss)
      }

      // private def _normalize(ps: Seq[StateClass]) = {
      //   case class Z(
      //     ss: Vector[StateClass] = Vector.empty,
      //     initStateName: Option[String] = None
      //   ) {
      //     def r = initStateName.
      //       map(_explicit_init).
      //       getOrElse(ss)

      //     private def _explicit_init(name: String) = {
      //       val (ls, rs) = ss.span(_.name != name)
      //       rs.headOption.map(x => x +: (ls ++ rs.tail)).getOrElse(ss)
      //     }

      //     def +(rhs: StateClass) = {
      //       if (rhs.name.equalsIgnoreCase(PROP_STATE_INIT))
      //         copy(initStateName = _init_state_name(rhs))
      //       else
      //         copy(ss = ss :+ rhs)
      //     }

      //     private def _init_state_name(p: StateClass) =
      //       p.transitions.transitions.map(_.to).collect {
      //         case NameTransitionTo(to) => to
      //       }.headOption
      //   }
      //   ps./:(Z())(_+_).r
      // }

      // private def _init_state_name(ps: Seq[StateClass]) =
      //   ps.find(_.name.equalsIgnoreCase(PROP_STATE_INIT)).
      //     flatMap(_.transitions.transitions.map(_.to).collect {
      //       case NameTransitionTo(to) => to
      //     }.headOption)

      // private def _normalize(
      //   ps: Seq[StateClass],
      //   initstatename: Option[String]
      // ) = initstatename.map { name =>
      //   val (ls, rs) = ps.span(_.name != name)
      //   rs.headOption.map(x => x +: (ls ++ rs.tail)).getOrElse(ps)
      // }.getOrElse(ps)

      // // unused
      // private def _resolve_init(ps: Seq[MState]): Seq[MState] = {
      //   case class Z(
      //     ss: Vector[MState] = Vector.empty,
      //     initStateName: Option[String] = None
      //   ) {
      //     def r = initStateName.
      //       map(_explicit_init).
      //       getOrElse(ss)

      //     private def _explicit_init(name: String) = {
      //       val (ls, rs) = ss.span(_.name != name)
      //       rs.headOption.map(x => x +: (ls ++ rs.tail)).getOrElse(ss)
      //     }

      //     def +(rhs: MState) = {
      //       if (rhs.name.equalsIgnoreCase(PROP_STATE_INIT))
      //         copy(initStateName = _init_state_name(rhs))
      //       else
      //         copy(ss = ss :+ rhs)
      //     }

      //     private def _init_state_name(p: MState) =
      //       p.transitions.headOption.map(_.postState.name)
      //   }
      //   ps./:(Z())(_+_).r
      // }

      private def _make_diagram(c: Context, p: MStateMachine): SExpr = {
        val env = c.executionContext.environment
        val model = _make_model(p)
        val g = new StateMachineDiagramGenerator(env, model)
        g.generate(p)
      }

      private def _make_model(sm: MStateMachine): SimpleModel = {
        SimpleModel(Vector(sm))
      }
    }
  }
}
