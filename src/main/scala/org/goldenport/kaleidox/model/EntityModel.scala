package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.goldenport.parser._
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Record
import org.goldenport.record.store.Query
import org.goldenport.sexpr.SSchema
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.sexpr.eval.entity.{EntityClass => IEntityClass}
import org.goldenport.sexpr.eval.entity.Entity
import org.goldenport.sexpr.eval.entity.EntityId
import org.goldenport.context.Consequence
import org.goldenport.collection.VectorMap
import org.goldenport.statemachine.StateMachineClass
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.lisp.Context
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass
import org.goldenport.kaleidox.model.entity.KaleidoxEntity
import org.goldenport.kaleidox.model.entity.KaleidoxEntityFactory

/*
 * @since   Jun. 25, 2021
 *  version Jun. 26, 2021
 *  version Sep. 24, 2021
 *  version Oct. 31, 2021
 * @version Nov. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class EntityModel(
  factory: KaleidoxEntityFactory,
  classes: VectorMap[String, EntityModel.EntityClass]
) extends ISchemaModel {
  import EntityModel._

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[EntityModel] = if (isEmpty) None else Some(this)

  def get(name: String): Option[EntityClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = get(name).map(_.schema)
  def getSchemaClass(name: String): Option[ISchemaClass] = get(name).map(_.schemaClass)

  def +(rhs: EntityModel): EntityModel = copy(classes = classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.schema.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }
}

object EntityModel {
//  val empty = EntityModel(VectorMap.empty[String, EntityClass])
  def empty(f: KaleidoxEntityFactory) = EntityModel(f, VectorMap.empty[String, EntityModel.EntityClass])

  implicit object EntityModelSemigroup extends Semigroup[EntityModel] {
    def append(lhs: EntityModel, rhs: => EntityModel) = lhs + rhs
  }

  case class EntityClass(
    factory: KaleidoxEntityFactory,
    schemaClass: SchemaClass,
    store: IEntityClass.Store = IEntityClass.Store()
  ) extends IEntityClass {
    def name = schemaClass.name
    def schema = schemaClass.schema
    def stateMachines: Vector[StateMachineClass] = schemaClass.stateMachines

    def create(p: IRecord)(implicit ctx: LispContext): Consequence[Entity] = {
//      implicit val sc = ctx.statemachineContext
      implicit val c = ctx.asInstanceOf[Context]
      for {
        attrs <- schemaClass.attributeRecordForCreate(p)
      } yield {
        val id = EntityId.generate(name)
        val sms = VectorMap(stateMachines.map(factory.spawn(_, id)).
          map(x => Symbol(x.name) -> x))
        KaleidoxEntity.create(this, id, attrs, sms)
      }
    }

    def reconstitute(p: IRecord): Consequence[Entity] = for {
      id <- schemaClass.idForReconstitute(p)
      attrs <- schemaClass.attributeRecordForReconstitute(p)
      sms <- schemaClass.stateMachineRecordForReconstitute(p, id)
    } yield {
      KaleidoxEntity.create(this, id, attrs, sms)
    }

    def unmarshallProperties(p: IRecord): Consequence[IRecord] =
      schemaClass.unmarshallProperties(p)

    def persistentQuery(p: Query): Query = ???
  }
  object EntityClass {
  }

  def apply(f: KaleidoxEntityFactory, p: EntityClass): EntityModel = EntityModel(f, VectorMap(p.name -> p))

  def create(f: KaleidoxEntityFactory, p: LogicalSection): EntityModel = createOption(f, p) getOrElse EntityModel.empty(f)

  def createOption(f: KaleidoxEntityFactory, p: LogicalSection): Option[EntityModel] = {
    SchemaClass.createOption(p).map(_to_model(f, _))
    // TODO association
    // TODO statemachine
  }

  def create(f: KaleidoxEntityFactory, p: Section): EntityModel = createOption(f, p) getOrElse EntityModel.empty(f)

  def createOption(f: KaleidoxEntityFactory, p: Section): Option[EntityModel] = {
    SchemaClass.createOption(p).map(_to_model(f, _))
    // TODO association
    // TODO statemachine
  }

  private def _to_model(f: KaleidoxEntityFactory, p: SchemaClass) = EntityModel(f, EntityClass(f, p))
}
