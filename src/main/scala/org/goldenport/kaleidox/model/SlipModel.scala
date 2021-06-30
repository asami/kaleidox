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
 * @since   Jun. 27, 2021
 * @version Jun. 27, 2021
 * @author  ASAMI, Tomoharu
 */
case class SlipModel(
  classes: VectorMap[String, SlipModel.SlipClass]
) extends ISchemaModel {
  import SlipModel._

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[SlipModel] = if (isEmpty) None else Some(this)

  def get(name: String): Option[SlipClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = get(name).map(_.schema)
  def getSchemaClass(name: String): Option[ISchemaClass] = get(name).map(_.schemaClass)

  def +(rhs: SlipModel): SlipModel = copy(classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.slip.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }
}

object SlipModel {
  val empty = SlipModel(VectorMap.empty[String, SlipClass])

  implicit object SlipModelMonoid extends Monoid[SlipModel] {
    def zero = SlipModel.empty
    def append(lhs: SlipModel, rhs: => SlipModel) = lhs + rhs
  }

  case class SlipClass(
    schemaClass: SchemaClass
  ) {
    def name = schemaClass.name
    def schema = schemaClass.schema
  }
  object SlipClass {
  }

  def apply(p: SlipClass): SlipModel = SlipModel(VectorMap(p.name -> p))

  def create(p: Section): SlipModel = createOption(p) getOrElse empty

  def createOption(p: Section): Option[SlipModel] =
    SchemaClass.createOption(p).map(_to_model)

  private def _to_model(p: SchemaClass) = SlipModel(SlipClass(p))
}
