package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.goldenport.context.Showable
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass

/*
 * @since   Jun. 25, 2021
 *  version Jun. 27, 2021
 *  version Aug. 21, 2023
 * @version Oct. 15, 2023
 * @author  ASAMI, Tomoharu
 */
case class ValueModel(
  classes: VectorMap[String, ValueModel.ValueClass],
  description: Description = Description.name("value")
) extends Model.ISubModel with ISchemaModel {
  import ValueModel._

  protected def display_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def print_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def show_String: String = classes.values.map(x => x.name).mkString("\n")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[ValueModel] = if (isEmpty) None else Some(this)

  def get(name: String): Option[ValueClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = get(name).map(_.schema)
  def getSchemaClass(name: String): Option[ISchemaClass] = get(name).map(_.schemaClass)

  def +(rhs: ValueModel): ValueModel = copy(classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.value.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }
}

object ValueModel {
  val empty = ValueModel(VectorMap.empty[String, ValueClass])

  implicit object ValueModelMonoid extends Monoid[ValueModel] {
    def zero = ValueModel.empty
    def append(lhs: ValueModel, rhs: => ValueModel) = lhs + rhs
  }

  case class ValueClass(
    schemaClass: SchemaClass
  ) extends SchemaClass.SchemaClassContainer {
    def name = schemaClass.name
    def schema = schemaClass.schema
  }
  object ValueClass {
  }

  def apply(p: ValueClass): ValueModel = ValueModel(VectorMap(p.name -> p))

  def create(p: Section): ValueModel = createOption(p) getOrElse empty

  def createOption(p: Section): Option[ValueModel] =
    SchemaClass.createOption(p).map(_to_model)

  private def _to_model(p: SchemaClass) = ValueModel(ValueClass(p))
}
