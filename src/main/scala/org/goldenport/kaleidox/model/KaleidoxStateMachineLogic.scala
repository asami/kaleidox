package org.goldenport.kaleidox.model

import scalaz.{State => _, _}, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.statemachine._
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.lisp.Context

/*
 * @since   May. 23, 2021
 *  version May. 29, 2021
 * @version Nov. 29, 2021
 * @author  ASAMI, Tomoharu
 */
trait KaleidoxStateMachineLogic extends StateMachineModel {
}

object KaleidoxStateMachineLogic {
  object Factory extends StateMachineLogic.Factory {
    def create(rule: StateMachineRule): StateMachineLogic = SimpleLogic(rule)
  }

  case class SimpleLogic(rule: StateMachineRule) extends StateMachineLogic {
//    def accept(state: State, p: Parcel): Consequence[Boolean] = Consequence(true) // TODO
    def execute(sm: StateMachine, activity: Activity, p: Parcel): Parcel = {
      // val a: org.goldenport.kaleidox.ExecutionContext = p.context
      activity match {
        case Activity.Empty => p
        case Activity.Opaque(s) =>
          val ctx = p.context.opaque.get.asInstanceOf[Context]
          val r = ctx.kaleidoxEvaluator.applyScript(s)
          p
      }
    }
  }
}
