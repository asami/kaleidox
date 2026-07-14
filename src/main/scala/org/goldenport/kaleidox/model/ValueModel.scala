package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.goldenport.context.Showable
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.parser.ParseLocation
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass

/*
 * @since   Jun. 25, 2021
 *  version Jun. 27, 2021
 *  version Aug. 21, 2023
 *  version Oct. 15, 2023
 *  version May. 24, 2026
 * @version Jul. 15, 2026
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
    schemaClass: SchemaClass,
    packageName: Option[String] = None,
    properties: Map[String, String] = Map.empty,
    sourceLocation: Option[ParseLocation] = None
  ) extends SchemaClass.SchemaClassContainer {
    def name = schemaClass.name
    def schema = schemaClass.schema

    def getProperty(name: String): Option[String] =
      properties.get(_normalize_property_name(name))
  }
  object ValueClass {
    def create(p: Section): Option[ValueClass] =
      ValueModel.createOption(p).flatMap(_.classes.values.headOption)

    def create(name: String, p: Section): Option[ValueClass] = {
      val properties = _properties(p)
      SchemaClass.createOption(name, p).map(ValueClass(_, properties = properties, sourceLocation = p.location))
    }
  }

  def apply(p: ValueClass): ValueModel = ValueModel(VectorMap(p.name -> p))

  def create(p: Section): ValueModel = createOption(p) getOrElse empty

  def createOption(p: Section): Option[ValueModel] =
    _to_model(p).toOption

  private def _to_model(p: Section) = {
    val properties = _properties(p)
    val pkg = properties.get("package").orElse(properties.get("package-name"))
    SchemaClass.createOption(p).map(x => ValueModel(ValueClass(x, pkg, properties, p.location))).getOrElse(empty)
  }

  private def _properties(p: Section): Map[String, String] =
    CmlSectionFormat.keyValues(p).map { case (key, value) =>
      _normalize_property_name(key) -> value.trim.stripPrefix("\"").stripSuffix("\"")
    }.filterNot(_._2.isEmpty).toMap

  private def _normalize_property_name(p: String): String =
    Option(p).getOrElse("").trim.toLowerCase(java.util.Locale.ROOT).replace('_', '-')
}
