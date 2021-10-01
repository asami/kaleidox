package org.goldenport.kaleidox.model.entity

import org.goldenport.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.record.store.Query
import org.goldenport.statemachine._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.entity._
import org.goldenport.kaleidox.model.EntityModel

/*
 * @since   Sep. 20, 2021
 * @version Sep. 24, 2021
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntityCollection(
  val factory: KaleidoxEntityFactory,
  val name: Symbol,
  val kaleidoxEntityClass: EntityModel.EntityClass
) extends EntityCollection {
  def entityClass = kaleidoxEntityClass

  def get(id: EntityId): SExpr = RAISE.notImplementedYetDefect

  def query(q: Query): SExpr = RAISE.notImplementedYetDefect

  def select(q: Query, header: Option[HeaderStrategy]): SExpr = RAISE.notImplementedYetDefect

  def create(rec: Record): SExpr = {
    // TODO statemachine
    val id = EntityId.generate()
    val sms = kaleidoxEntityClass.stateMachines.map(_spawn(_, id)).
      map(x => Symbol(x.name) -> x).toMap
    val entity = new KaleidoxEntity(id, rec, sms)
    SEntity(entity)
  }

  private def _spawn(p: StateMachineClass, id: EntityId): StateMachine =
    factory.spawn(p, id)

  def update(id: EntityId, rec: Record): SExpr = RAISE.notImplementedYetDefect

  def delete(id: EntityId): SExpr = RAISE.notImplementedYetDefect
}
