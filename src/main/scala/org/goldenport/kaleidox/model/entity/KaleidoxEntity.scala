package org.goldenport.kaleidox.model.entity

import org.goldenport.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.entity._
import org.goldenport.statemachine.StateMachine
import org.goldenport.kaleidox.model.EntityModel

/*
 * @since   Sep. 22, 2021
 * @version Sep. 24, 2021
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntity(
  val id: EntityId,
  val attributes: Record,
  val statemachines: Map[Symbol, StateMachine]
) extends Entity {
  def show = {
    val sms = if (statemachines.isEmpty)
      ""
    else
      statemachines.map {
        case (k, v) => s"${k.name}:${v.status}"
      }.mkString(";", ";", "")
    s"""${id.text}${sms}"""
  }
}
