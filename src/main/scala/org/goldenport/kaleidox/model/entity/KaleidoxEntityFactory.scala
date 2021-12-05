package org.goldenport.kaleidox.model.entity

import org.goldenport.RAISE
import org.goldenport.i18n.I18NContext
import org.goldenport.record.store._
import org.goldenport.event.ObjectId
import org.goldenport.statemachine._
import org.goldenport.statemachine.{ExecutionContext => StateMachineContext}
import org.goldenport.sexpr.eval.entity._
import org.goldenport.kaleidox.lisp.Context
import org.goldenport.kaleidox.model.EntityModel.{EntityClass => KaleidoxEntityClass}
import org.goldenport.kaleidox.model.EntityModel.{EntityClass => KaleidoxEntityClass}

/*
 * @since   Sep. 20, 2021
 *  version Sep. 24, 2021
 *  version Oct. 31, 2021
 * @version Nov. 29, 2021
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntityFactory(
  val i18nContext: I18NContext,
  val store: StoreFactory,
  val stateMachineSpace: StateMachineSpace
) extends EntityFactory {
  private var _collections: Map[Symbol, EntityCollection] = Map.empty

  def createId(id: String): EntityId = EntityId.create(id)

  def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection] =
    store.map(x => RAISE.notImplementedYetDefect).getOrElse(getCollection(collection))

  def getCollection(collection: Symbol): Option[EntityCollection] = _collections.get(collection)

  def defineCollection(name: Symbol, entity: EntityClass): EntityCollection =
    entity match {
      case m: KaleidoxEntityClass =>
        val sc = store.defineCollection(name, m.schemaClass.schema)
        val x = new KaleidoxEntityCollection(this, name, m, sc)
        _collections = _collections + (name -> x)
        x
      case m => RAISE.notImplementedYetDefect
    }

  def spawn(
    smc: StateMachineClass,
    id: EntityId
  )(implicit ctx: Context): StateMachine = {
    implicit val sc = StateMachineContext.create(ctx.traceContext, smc.logic, ctx)
    val r = smc.spawn(ObjectId(id.string))
    stateMachineSpace.register(r)
    r
  }
}
