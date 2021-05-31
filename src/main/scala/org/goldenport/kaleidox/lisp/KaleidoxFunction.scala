package org.goldenport.kaleidox.lisp

import scalaz._, Scalaz._
import org.goldenport.event._
import org.goldenport.statemachine._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import LispFunction.CursorResult
import org.goldenport.kaleidox.model.sexpr._

/*
 * @since   May.  5, 2021
 * @version May. 22, 2021
 * @author  ASAMI, Tomoharu
 */
object KaleidoxFunction {
  trait KaleidoxEvalFunction extends CursorEvalFunction {
    def eval(c: LispContext): CursorResult = eval(c.asInstanceOf[Context])

    def eval(c: Context): CursorResult
  }

  val functions = Vector(
    Event.Issue,
    StateMachine.New
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
          map(_issue_event(c)).
          getOrElse(SError.notFound("Event", name))
      }

      private def _issue_event(c: Context)(p: Event) = {
        c.statemachineSpace.issueEvent(p)
        SEvent(p)
      }
    }
  }

  object StateMachine {
    case object New extends KaleidoxEvalFunction {
      val specification = FunctionSpecification("statemachine-new",
        param_argument("statemachine")
      )

      def eval(c: Context): CursorResult = for {
        name <- c.param.takeString('statemachine)
      } yield name.map(_statemachine_new(c))

      private def _statemachine_new(c: Context)(name: String): SExpr = {
        c.statemachineSpace.spawnOption(name).
          map(_statemachine_new).
          getOrElse(SError.notFound("StateMachine", name))
      }

      private def _statemachine_new(p: StateMachine) = SStateMachine(p)
    }
  }
}
