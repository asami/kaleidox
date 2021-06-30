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
 * @version Jun. 28, 2021
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
      import org.smartdox.Description

      private def _make_sm(c: Context, name: String) =
        c.universe.model.stateMachineModel.getClass(name).map { c =>
          val pkg = MPackageRef.default // TODO
          val desc = Description.name(name)
          MDomainStateMachine(desc, pkg)
        }

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
