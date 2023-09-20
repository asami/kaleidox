package org.goldenport.kaleidox.lisp

import scalaz._, Scalaz._
import Validation.FlatMap._
// import org.simplemodeling.model._
import java.nio.charset.Charset
import org.goldenport.RAISE
import org.goldenport.io.InputSource
import org.goldenport.parser._
import org.goldenport.event._
import org.goldenport.statemachine._
import org.goldenport.statemachine.{ExecutionContext => StateMachineContext}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import org.goldenport.sexpr.eval.entity.EntityId
import LispFunction.CursorResult
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.model.sexpr._
import org.goldenport.kaleidox.model.diagram._

/*
 * @since   May.  5, 2021
 *  version May. 22, 2021
 *  version Jun. 30, 2021
 *  version Jul. 11, 2021
 *  version Aug.  2, 2021
 *  version Sep. 26, 2021
 *  version Oct. 31, 2021
 *  version Nov. 29, 2021
 *  version Dec. 18, 2021
 *  version Jul. 31, 2023
 *  version Aug. 21, 2023
 * @version Sep. 16, 2023
 * @author  ASAMI, Tomoharu
 */
object KaleidoxFunction {
  trait KaleidoxEvalFunction extends CursorEvalFunction {
    def eval(c: LispContext): CursorResult = eval(c.asInstanceOf[Context])

    def eval(c: Context): CursorResult

    protected final def issue_event(c: Context)(p: Event) = {
      implicit val lc = c
      val eid = p match {
        case m: CallEvent => m.to.entity.map(x => EntityId(x, m.to.id))
        case _ => None
      }
      val entity = eid.flatMap(x =>
        c.feature.entity.get(x) match {
          case SEntity(y) =>
            for ((_, sm) <- y.statemachines)
              c.statemachineSpace.register(sm)
            Some(y)
          case _ => None
        }
      )
      val parcel = Parcel(StateMachineContext.createPreparation(c.traceContext, c), p)
      c.statemachineSpace.issueEvent(parcel)
      entity.map(c.feature.entity.update).flatMap {
        case m: SError => Some(m)
        case _ => None
      }.getOrElse(
        SEvent(p)
      )
    }
  }

  val functions = Vector(
    Event.Issue,
    Event.Call,
    StateMachine.New,
    StateMachine.Diagram,
    SmartDox.Load,
    SmartDox.Html,
    Modeler.Current,
    Modeler.Load,
    Modeler.Diagram
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
        param_argument("to"),
        param_argument_option("entity")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('name)
        to <- c.param.takeString('to)
        entity <- c.param.getString('entity)
      } yield (name |@| to |@| entity)(_call(c))

      private def _call(c: Context)(name: String, to: String, entity: Option[String]): SExpr = {
        c.universe.model.eventModel.createCallOption(name, ObjectId(to, entity)).
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
        resourceid: Option[String]
      ): SExpr = {
        _spawn(c, name, resourceid).
          map(_statemachine_new).
          getOrElse(SError.notFound("StateMachine", name))
      }

      private def _spawn(
        c: Context,
        name: String,
        resourceid: Option[String]
      ) = {
        implicit val sc = StateMachineContext.createPreparation(c.traceContext, c)
        resourceid.
          map(x => c.statemachineSpace.spawnOption(name, ObjectId(x))).
          getOrElse(c.statemachineSpace.spawnOption(name))
      }

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
        resourceid: Option[String]
      ): SExpr = {
        // _make_sm(c, name).
        //   map(_make_diagram(c, _)).
        //   getOrElse(SError.notFound("statemachine", name))
        c.extension.modeler.generateStateMachineDiagram(c, name, resourceid)
      }

      // import org.simplemodeling.model._
      // import org.simplemodeling.model.domain._
      import org.goldenport.collection.VectorMap

      // class StateHanger(val states: VectorMap[String, MState]) {
      //   def get(name: String): Option[MState] = states.get(name) orElse _get_substate(name)

      //   private def _get_substate(name: String) = 
      //     states.values.toStream.flatMap(_.getSubStateRecursive(name)).headOption
          
      //   def historyStates(name: String): Vector[MState] =
      //     states.values.flatMap(s =>
      //       if (s.transitions.exists(t => t.postState.name == name))
      //         Some(s.createHistoryState)
      //       else
      //         None
      //     ).toVector
      // }
      // object StateHanger {
      //   def create(ps: Seq[(String, MState)]): StateHanger = new StateHanger(VectorMap(ps))
      // }

//       private def _make_sm(c: Context, name: String) =
//         c.universe.model.stateMachineModel.getClass(name).map { c =>
//           val sm = MDomainStateMachine.create(name)
//           val states = _states(sm, c)
//           // val sms = VectorMap.empty[String, MDomainStateMachine]
//           sm.setStates(states)
//           sm
//         }

//       private def _states(
//         sm: MDomainStateMachine,
//         p: StateMachineClass
//       ): VectorMap[String, MState] = {
//         def initState = MState.initState(sm)
//         // def historyState = MState.historyState(sm)

//         def _state_(p: StateClass): MState = {
//           val s = MState.create(sm, p.name)
//           s
//         }

//         def _statemachine_state_(p: StateMachineRule): MState = {
//           val s = MState.create(sm, p.name.get) // TODO
//           s.subStateMap = _sub_states_map_(p)
//           s
//         }

//         def _sub_states_map_(p: StateMachineRule): VectorMap[String, MState] = {
//           val a1 = p.states.map(_state_)
//           val a2 = p.statemachines.map(_statemachine_state_)
//           val a = (a1 ++ a2).map(x => x.name -> x)
//           VectorMap(a)
//         }

//         def _build_transitions(statemap: StateHanger) = {
//           def _build_state(s: StateClass): Unit = {
//             def _transition_(t: Transition): Option[MTransition] = {
//               val g = _guard_(t.guard)
//               val event = t.getEventName.map(x => MEvent(x)) // TODO share
//               val action = None // MAction(sm, "???")

//               def _name_transition_(p: NameTransitionTo) =
//                 (statemap.get(s.name), statemap.get(p.name)) match {
//                   case (Some(from), Some(to)) => MTransition(sm, event, g, from, to, action)
//                   case (Some(from), None) => RAISE.noReachDefect
//                   case (None, Some(to)) => MTransition(sm, event, g, initState, to, action)
//                   case (None, None) => RAISE.noReachDefect
//                 }

//               def _history_transition_(p: HistoryTransitionTo) = {
//                 val historystate = statemap.historyStates(s.name).head // TODO
//                 MTransition(sm, event, g, statemap.get(s.name).get, historystate, action)
//               }

//               t.to match {
//                 case NoneTransitionTo => None
//                 case FinalTransitionTo => None
//                 case m: HistoryTransitionTo => Some(_history_transition_(m))
//                 case m: NameTransitionTo =>
//                   if (m.name.equalsIgnoreCase(PROP_STATE_INIT))
//                     None
//                   else
//                     Some(_name_transition_(m))
//               }
//             }

//             val ts = s.transitions.call.flatMap(_transition_) ++ s.transitions.global.flatMap(_transition_)
//             statemap.get(s.name).foreach(_.transitions = ts.toList)
//           }

//           def _build_statemachine_(smr: StateMachineRule): Unit = {
//             def _transition_(t: Transition): Option[MTransition] = { // TODO unify
//               val g = _guard_(t.guard)
//               val event = t.getEventName.map(x => MEvent(x)) // TODO share
//               val action = None // MAction(sm, "???")

//               def _name_transition_(p: NameTransitionTo) =
//                 (statemap.get(smr.name getOrElse ""), statemap.get(p.name)) match {
//                   case (Some(from), Some(to)) => MTransition(sm, event, g, from, to, action)
//                   case (Some(from), None) => RAISE.noReachDefect
//                   case (None, Some(to)) => MTransition(sm, event, g, initState, to, action)
//                   case (None, None) => RAISE.noReachDefect
//                 }

//               def _history_transition_(p: HistoryTransitionTo) = {
//                 RAISE.notImplementedYetDefect
//               }

//               t.to match {
//                 case NoneTransitionTo => None
//                 case FinalTransitionTo => None
//                 case m: HistoryTransitionTo => Some(_history_transition_(m))
//                 case m: NameTransitionTo =>
//                   if (m.name.equalsIgnoreCase(PROP_STATE_INIT))
//                     None
//                   else
//                     Some(_name_transition_(m))
//               }
//             }

//             smr.states.foreach(_build_state)
//             smr.statemachines.foreach(_build_statemachine_)
//             val ts = smr.transitions.call.flatMap(_transition_) ++ smr.transitions.global.flatMap(_transition_)
//             statemap.get(smr.name getOrElse "").foreach(_.transitions = ts.toList)
//           }

//           p.statemachines.foreach(_build_statemachine_)
//           p.states.foreach(_build_state)
//         }

//         def _guard_(g: SmGuard): Option[MGuard] = None // MGuard(sm)

//         val (a0, initstatename) = _normalize_init(p.states)
//         val a1 = a0.map(_state_)
//         val a2 = p.statemachines.map(_statemachine_state_)
// //        val c = Vector(MState.initState(sm), MState.finalState(sm)).map(x => x.name -> x)
// //        val a3 = Vector()
// //        val a4 = _resolve_init(a1 ++ a2 ++ a3)
// //        val a4 = _normalize(a1 ++ a2 ++ a3)
//         val a4 = _normalize_init(initstatename, a1, a2)
//         val a = a4.map(x => x.name -> x)
//         val states = StateHanger.create(a)
//         _build_transitions(states)
//         states.states
//       }

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
            (p.transitions.call.map(_.to) ++ p.transitions.global.map(_.to)).collect {
              case NameTransitionTo(to) => to
            }.headOption
        }
        ps./:(Z())(_+_).r
      }

      // private def _normalize_init(initstatename: Option[String], states: Seq[MState], sms: Seq[MState]): Seq[MState] = {
      //   val ss = states ++ sms
      //   initstatename.map { name =>
      //     val (ls, rs) = ss.span(_.name != name)
      //     rs.headOption.map(x => x +: (ls ++ rs.tail)).getOrElse(ss)
      //   }.getOrElse(ss)
      // }

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

      // private def _make_diagram(c: Context, p: MStateMachine): SExpr = {
      //   val env = c.executionContext.environment
      //   val model = _make_model(p)
      //   val g = new StateMachineDiagramGenerator(env, model)
      //   g.generate(p)
      // }

      // private def _make_model(sm: MStateMachine): SimpleModel = {
      //   SimpleModel(Vector(sm))
      // }
    }
  }

  object SmartDox {
    import org.goldenport.kaleidox.Script

    private val _config = Script.DoxLiteralTokenizer.config

    case object Load extends KaleidoxEvalFunction {
      val specification = FunctionSpecification(
        "dox-load",
        param_argument("file"),
        param_argument_option("charset")
      )

      def eval(c: Context): CursorResult = for {
        text <- c.param.textInFile(c)
      } yield text.flatMap(_load(c))

      private def _load(c: Context)(text: String): ValidationNel[SError, SExpr] =
        SExpr.executeValidationNel(load(c, text))

      def load(c: Context, text: String): SExpr = {
        Script.DoxLiteralTokenizer.parse(_config, text)
      }
    }

    case object Html extends KaleidoxEvalFunction {
      import org.goldenport.cli.Environment
      import org.goldenport.realm.Realm
      import org.smartdox.Dox
      import org.smartdox.parser.Dox2Parser
      import org.smartdox.generator.{Context => DoxContext}
      import org.smartdox.generators.Dox2HtmlGenerator

      val specification = FunctionSpecification(
        "dox-html",
        param_argument("dox"),
        param_argument_option("charset")
      )

      def eval(c: Context): CursorResult = for {
        text <- c.param.textInFile(c)
      } yield text.flatMap(_html(c))

      private def _html(c: Context)(p: String): ValidationNel[SError, SExpr] = {
        for {
          dox <- _parse(p)
          r <- SExpr.executeValidationNel(html(c, dox))
        } yield r
      }

      private def _parse(p: String): ValidationNel[SError, Dox] = {
        val c = Script.DoxLiteralTokenizer.config
        val parser = new Dox2Parser(c)
        parser.apply(p) match {
          case ParseSuccess(dox, _) => Success(dox)
          case m: ParseFailure[_] => Failure(SError.syntaxError(m)).toValidationNel
          case EmptyParseResult() => Success(Dox.empty)
        }
      }

      def html(c: Context, text: String): SExpr = 
        _parse(text) match {
          case Success(s) => html(c, s)
          case Failure(e) => e.list.head.RAISE
        }

      def html(c: Context, dox: Dox): SExpr = {
        val env = Environment.create()
        val ctx = DoxContext.create(env)
        val b = Realm.Builder()
        b.setObject("article.dox", dox)
        val in = b.build()
        val htmltx = new Dox2HtmlGenerator(ctx)
        val r = htmltx.generate(in)
        r.get("html.d/article.html") match {
          case Some(s) => STree.sexpr(s) match {
            case SString(s) => SHtml(s)
            case m => m
          }
          case None => STree(r)
        }
      }
    }
  }

  object Modeler {
    case object Current extends KaleidoxEvalFunction {
      val specification = FunctionSpecification(
        "modeler-current"
      )

      def eval(c: Context): CursorResult = for {
        r <- c.param.lift(SModel(c.universe.model))
      } yield r
    }

    case object Load extends KaleidoxEvalFunction {
      val specification = FunctionSpecification(
        "modeler-load",
        param_argument("model"),
        param_argument_option("charset")
      )

      def eval(c: Context): CursorResult = for {
        text <- c.param.textInFile(c)
      } yield text.flatMap(_parse(c))

      private def _parse(c: Context)(text: String): ValidationNel[SError, SExpr] = {
        val m = Model.parse(c.config, text)
        Success(SModel(m))
      }
    }

    case object Diagram extends KaleidoxEvalFunction {
      val specification = FunctionSpecification(
        "modeler-diagram",
        param_argument("model"),
        param_argument_option("charset")
      )

      def eval(c: Context): CursorResult = for {
        textormodel <- c.param.textInFileOr(c) {
          case m: Model => m
          case m: SModel => m.model
        }
      } yield textormodel.flatMap(_diagram(c))

      private def _diagram(c: Context)(p: Either[String, IModel]): ValidationNel[SError, SExpr] = for {
        model <- make_model(c, p)
      } yield c.config.extension.modeler.generateDiagram(c, model)
    }

    protected def make_model(c: Context, p: Either[String, IModel]): ValidationNel[SError, SModel] = {
      val a = p match {
        case Right(r) => r
        case Left(l) => Model.parse(c.config, l)
      }
      val model = SModel(a)
      Success(model)
    }

    case object Vocabulary extends KaleidoxEvalFunction {
      val specification = FunctionSpecification(
        "modeler-vocabulary",
        param_argument("model"),
        param_argument_option("charset")
      )

      def eval(c: Context): CursorResult = for {
        text <- c.param.textInFile(c)
      } yield text.flatMap(_vocabulary(c))

      private def _vocabulary(c: Context)(text: String): ValidationNel[SError, SExpr] =
        RAISE.notImplementedYetDefect
    }

  }
}
