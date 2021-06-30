package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass

/*
 * @since   Jun. 25, 2021
 * @version Jun. 26, 2021
 * @author  ASAMI, Tomoharu
 */
case class EntityModel(
  classes: VectorMap[String, EntityModel.EntityClass]
) extends ISchemaModel {
  import EntityModel._

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[EntityModel] = if (isEmpty) None else Some(this)

  def get(name: String): Option[EntityClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = get(name).map(_.schema)
  def getSchemaClass(name: String): Option[ISchemaClass] = get(name).map(_.schemaClass)

  def +(rhs: EntityModel): EntityModel = copy(classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.schema.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }
}

object EntityModel {
  val empty = EntityModel(VectorMap.empty[String, EntityClass])

  implicit object EntityModelMonoid extends Monoid[EntityModel] {
    def zero = EntityModel.empty
    def append(lhs: EntityModel, rhs: => EntityModel) = lhs + rhs
  }

  case class EntityClass(
    schemaClass: SchemaClass
  ) {
    def name = schemaClass.name
    def schema = schemaClass.schema
  }
  object EntityClass {
  }

  def apply(p: EntityClass): EntityModel = EntityModel(VectorMap(p.name -> p))

  def create(p: Section): EntityModel = createOption(p) getOrElse empty

  def createOption(p: Section): Option[EntityModel] = {
    SchemaClass.createOption(p).map(_to_model)
    // TODO association
    // TODO statemachine
  }

  private def _to_model(p: SchemaClass) = EntityModel(EntityClass(p))
}
