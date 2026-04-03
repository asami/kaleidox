package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import com.typesafe.config.ConfigFactory
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.smartdox.Table
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.record.v3.IRecord
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model

/*
 * @since   Oct. 12, 2023
 *  version Mar. 24, 2026
 * @version Apr.  3, 2026
 * @author  ASAMI, Tomoharu
 */
case class PowertypeModel(
  classes: VectorMap[String, PowertypeModel.PowertypeClass] = VectorMap.empty,
  description: Description = Description.name("powertype")
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def print_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def show_String: String = classes.values.map(x => x.name).mkString("\n")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[PowertypeModel] = if (isEmpty) None else Some(this)

  def +(rhs: PowertypeModel): PowertypeModel = copy(classes ++ rhs.classes)
}

object PowertypeModel {
  val empty = PowertypeModel()

  implicit object PowertypeModelMonoid extends Monoid[PowertypeModel] {
    def zero = PowertypeModel.empty
    def append(lhs: PowertypeModel, rhs: => PowertypeModel) = lhs + rhs
  }

  case class PowertypeClass(
    description: Description,
    packageName: String = "domain", // TODO
    kinds: Vector[PowertypeKind] = Vector.empty
  ) extends Description.Holder {
  }
  case class PowertypeKind(
    name: String,
    value: Option[Int] = None,
    label: Option[String] = None
  )
  object PowertypeClass {
    def createOption(config: Config, p: LogicalSection): Option[PowertypeClass] =
      Builder(config).createOption(p)

    case class Builder(
      config: Config
    ) extends Model.ModelBuilderBase {
      type T = PowertypeClass

      protected def is_Accept(p: LogicalSection): Boolean = true

      protected def create_Model(
        p: LogicalSection,
        ps: Vector[LogicalSection],
        desc: Description
      ): T = {
        val props = dox_properties(p)
        val pkg = props.
          getString("package").
          orElse(props.getString("package_name")).
          map(_.trim).
          filterNot(_.isEmpty).
          getOrElse("domain")
        val kinds = ps.filterNot(x => _narrative_keys.contains(x.keyForModel.toLowerCase)).zipWithIndex.map {
          case (s, i) => _kind(s, i)
        }
        PowertypeClass(desc, pkg, kinds)
      }

      protected def create_Model(
        p: LogicalSection,
        desc: Description,
        properties: IRecord
      ): T = {
        val pkg = properties.
          getString("package").
          orElse(properties.getString("package_name")).
          map(_.trim).
          filterNot(_.isEmpty).
          getOrElse("domain")
        PowertypeClass(desc, pkg, _kinds(p))
      }

      protected def create_Model(
        p: LogicalSection,
        desc: Description,
        tables: List[Table]
      ): T = {
        PowertypeClass(desc, kinds = _kinds(p))
      }

      private def _kinds(p: LogicalSection): Vector[PowertypeKind] = {
        val hocon = ConfigFactory.parseString(p.text)
        val configs = hocon.takeConfigList("kinds") ::: hocon.takeConfigList("kind")
        configs.toVector.zipWithIndex.map { case (c, i) =>
          val name = c.getString("name")
          val value = if (c.hasPath("value")) Some(c.getInt("value")) else Some(i + 1)
          val label = if (c.hasPath("label")) Some(c.getString("label").trim).filterNot(_.isEmpty) else None
          PowertypeKind(name, value, label)
        }
      }

      private def _kind(p: LogicalSection, index: Int): PowertypeKind = {
        val props = dox_properties(p)
        val value = props.getString("value").flatMap(x => scala.util.Try(x.trim.toInt).toOption).orElse(Some(index + 1))
        val label = props.getString("label").map(_.trim).filterNot(_.isEmpty)
        PowertypeKind(p.nameForModel, value, label)
      }

      private val _narrative_keys = Set(
        "headline",
        "brief",
        "summary",
        "description",
        "lead",
        "content",
        "abstract",
        "remarks",
        "tooltip",
        "overview",
        "background",
        "mapping",
        "note",
        "notes",
        "narrative",
        "example",
        "validation",
        "rationale"
      )
    }
  }

  def apply(p: PowertypeClass): PowertypeModel =
    PowertypeModel(classes = VectorMap(p.name -> p))

  def create(config: Config, p: LogicalSection): PowertypeModel =
    Builder(config).createOption(p).getOrElse(PowertypeModel.empty)

  case class Builder(config: Config) extends Model.ModelBuilderBase {
    type T = PowertypeModel

    protected def is_Accept(p: LogicalSection): Boolean =
      p.nameForModel.equalsIgnoreCase("powertype")

    protected def create_Model(
      p: LogicalSection,
      ps: Vector[LogicalSection],
      desc: Description
    ): T = {
      val classes = ps.filter(_is_powertype_class_section).flatMap(PowertypeClass.createOption(config, _))
      PowertypeModel(classes = VectorMap(classes.map(x => x.name -> x)))
    }

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      properties: IRecord
    ): T = PowertypeModel.empty

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      tables: List[Table]
    ): T = PowertypeModel.empty

    private val _narrative_keys = Set(
      "headline",
      "brief",
      "summary",
      "description",
      "lead",
      "content",
      "abstract",
      "remarks",
      "tooltip",
      "overview",
      "background",
      "mapping",
      "note",
      "notes",
      "narrative",
      "example",
      "validation",
      "rationale"
    )

    private def _is_powertype_class_section(p: LogicalSection): Boolean =
      !_narrative_keys.contains(p.keyForModel.toLowerCase)
  }
}
