package org.goldenport.kaleidox.model.entity

import org.goldenport.RAISE
import org.goldenport.statemachine._
import org.goldenport.sexpr.eval.entity._
import org.goldenport.kaleidox.model.EntityModel.{EntityClass => KaleidoxEntityClass}

/*
 * @since   Sep. 20, 2021
 * @version Sep. 24, 2021
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntityFactory(
  val stateMachineSpace: StateMachineSpace
) extends EntityFactory {
  private var _collections: Map[Symbol, EntityCollection] = Map.empty

  def createId(id: String): EntityId = EntityId.create(id)
  def getCollection(store: Option[Symbol], collection: Symbol): Option[EntityCollection] =
    store.map(x => RAISE.notImplementedYetDefect).getOrElse(getCollection(collection))

  def getCollection(collection: Symbol): Option[EntityCollection] = _collections.get(collection)

  def defineCollection(name: Symbol, entity: EntityClass): EntityCollection = {
    entity match {
      case m: KaleidoxEntityClass => 
        val x = new KaleidoxEntityCollection(this, name, m)
        _collections = _collections + (name -> x)
        x
      case m => RAISE.notImplementedYetDefect
    }
  }

  def spawn(smc: StateMachineClass, id: EntityId): StateMachine = {
    val r = smc.spawn(id.text)
    stateMachineSpace.register(r)
    r
  }
}
