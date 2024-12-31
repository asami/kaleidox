package org.goldenport.kaleidox.model.entity

import scala.collection.mutable
import org.goldenport.RAISE
import org.goldenport.collection.VectorMap
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Field
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.entity._
import org.goldenport.sm.StateMachine
import org.goldenport.kaleidox.ExecutionContext
import org.goldenport.kaleidox.model.EntityModel
// import org.goldenport.kaleidox.model.SchemaModel.StateMachine

/*
 * @since   Sep. 22, 2021
 *  version Sep. 24, 2021
 *  version Oct. 31, 2021
 *  version Nov. 28, 2021
 *  version Feb. 28, 2023
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntity(
  val klass: EntityModel.EntityClass,
  val id: EntityId,
  val attributes: Record,
  val statemachines: VectorMap[Symbol, StateMachine]
) extends Entity {
  private val _journal = new mutable.ArrayBuffer[KaleidoxEntity.Journal]()

  def show = {
    val sms = if (statemachines.isEmpty)
      ""
    else
      statemachines.map {
        case (k, v) => s"${k.name}:${v.status}"
      }.mkString(";", ";", "")
    s"""${id.string}${sms}"""
  }

  def prepare(p: ExecutionContext): Unit = {
    RAISE.notImplementedYetDefect
  }

  def commit(p: ExecutionContext): Unit = {
    RAISE.notImplementedYetDefect
  }

  def record: IRecord = Record.data(
    klass.idName -> id
  ) + attributes + _statemachine_record

  def recordWithShortId: IRecord = Record.data(
    klass.idName -> id.objectId.string
  ) + attributes + _statemachine_record

  private def _statemachine_record = Record(statemachines)

  def persistentRecord: IRecord = {
    val ids = Record.data(
      klass.store.idColumnName -> klass.store.idValue(id)
    )
    val attrs = attributes.toRecord.mapField(klass.store.toField)
    val sms = Record(statemachines.values.toVector.map(klass.store.stateMachineToFieldValue))
    ids + attrs + sms
  }
}

object KaleidoxEntity {
//  def create(p: IRecord): KaleidoxEntity = ???
  sealed trait Journal {
  }
  object Journal {
    case class Create(record: IRecord) extends Journal {
    }
    case class Update(record: IRecord) extends Journal {
    }
  }

  def create(
    klass: EntityModel.EntityClass,
    id: EntityId,
    attributes: Record,
    statemachines: VectorMap[Symbol, StateMachine]
  ): KaleidoxEntity = {
    // val sms = statemachines.mapValues(_.setObjectId(id.objectId))
    val sms = statemachines
    new KaleidoxEntity(klass, id, attributes, sms)
  }
}
