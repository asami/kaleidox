package org.goldenport.kaleidox.model

import scalaz.{State => _, _}, Scalaz._
import org.goldenport.context.Consequence
import org.goldenport.statemachine._
import org.goldenport.kaleidox._

/*
 * @since   May. 23, 2021
 * @version May. 29, 2021
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
  }
}
