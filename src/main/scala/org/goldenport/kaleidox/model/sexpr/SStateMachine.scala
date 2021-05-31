package org.goldenport.kaleidox.model.sexpr

import org.goldenport.sexpr._
import org.goldenport.statemachine._

/*
 * @since   May.  4, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
case class SStateMachine(statemachine: StateMachine) extends SExtension with Mutable {
  override def titleInfo = s"${statemachine.name}:${statemachine.state.status}"
  def state = SState(statemachine.state)
  def status = statemachine.status
  def history = SList.create(
    statemachine.history.map(x => SCell(SEvent(x.event), SState(x.state)))
  )
}
