package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import com.typesafe.config.ConfigFactory
import org.goldenport.RAISE
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
 *  version Apr.  3, 2026
 * @version May. 24, 2026
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

      protected override def create_model_flat(p: LogicalSection): T =
        _kinds_from_text_table(p.text).map { kinds =>
          PowertypeClass(dox_description_name(p), _package_from_text(p.text), kinds)
        }.getOrElse(super.create_model_flat(p))

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
        val pkg = _package_from_text(p.text)
        val kinds = _kinds_from_tables(tables).
          orElse(_kinds_from_text_table(p.text)).
          getOrElse(_kinds(p))
        PowertypeClass(desc, pkg, kinds)
      }

      private def _kinds(p: LogicalSection): Vector[PowertypeKind] = {
        _kinds_from_text_table(p.text).getOrElse {
          val hocon = ConfigFactory.parseString(p.text)
          val configs = hocon.takeConfigList("kinds") ::: hocon.takeConfigList("kind")
          configs.toVector.zipWithIndex.map { case (c, i) =>
            val name = c.getString("name")
            val value = if (c.hasPath("value")) Some(c.getInt("value")) else Some(i + 1)
            val label = if (c.hasPath("label")) Some(c.getString("label").trim).filterNot(_.isEmpty) else None
            PowertypeKind(name, value, label)
          }
        }
      }

      private def _kind(p: LogicalSection, index: Int): PowertypeKind = {
        val props = dox_properties(p)
        val value = props.getString("value").flatMap(x => scala.util.Try(x.trim.toInt).toOption).orElse(Some(index + 1))
        val label = props.getString("label").map(_.trim).filterNot(_.isEmpty)
        PowertypeKind(p.nameForModel, value, label)
      }

      private def _package_from_text(text: String): String =
        text.linesIterator.collectFirst {
          case _package_line(key, value) if key.equalsIgnoreCase("package") || key.equalsIgnoreCase("package_name") =>
            value.trim.stripPrefix("\"").stripSuffix("\"")
        }.map(_.trim).filterNot(_.isEmpty).getOrElse("domain")

      private val _package_line = """\s*([A-Za-z_][A-Za-z0-9_\-]*)\s*[=:]\s*(.+)\s*""".r

      private def _without_table_text(text: String): String =
        text.linesIterator.filterNot(x => _is_table_line(x.trim)).mkString("\n")

      private def _kinds_from_tables(ps: List[Table]): Option[Vector[PowertypeKind]] = {
        val rows = ps.toVector.flatMap(_.toVectorMapStringVector)
        if (rows.isEmpty)
          None
        else
          Some(rows.zipWithIndex.map {
            case (row, index) => _kind_from_table(row, index)
          })
      }

      private def _kinds_from_text_table(text: String): Option[Vector[PowertypeKind]] = {
        val rows = text.linesIterator.toVector.
          map(_.trim).
          filter(_is_table_line).
          map(_split_table_row).
          filterNot(_is_separator_row)
        rows.headOption.flatMap { header =>
          val body = rows.drop(1)
          if (body.isEmpty)
            None
          else
            Some(body.zipWithIndex.map {
              case (row, index) => _kind_from_table(_to_row_map(header, row), index)
            })
        }
      }

      private def _split_table_row(line: String): Vector[String] =
        line.split("\\|", -1).toVector.drop(1).dropRight(1).map(_.trim)

      private def _is_table_line(line: String): Boolean =
        line.startsWith("|") && line.endsWith("|")

      private def _is_separator_row(row: Vector[String]): Boolean =
        row.nonEmpty && row.forall(_.matches("[-: ]+"))

      private def _to_row_map(
        header: Vector[String],
        row: Vector[String]
      ): VectorMap[String, String] =
        VectorMap(header.zip(row.padTo(header.length, "")).map {
          case (key, value) => key -> value
        })

      private def _kind_from_table(
        row: VectorMap[String, String],
        index: Int
      ): PowertypeKind = {
        val name = _row_get(row, "name").
          getOrElse(RAISE.syntaxErrorFault("POWERTYPE kind table row requires name."))
        val value = _row_get(row, "value").flatMap(x => scala.util.Try(x.toInt).toOption).orElse(Some(index + 1))
        val label = _row_get(row, "label")
        PowertypeKind(name, value, label)
      }

      private def _row_get(
        row: VectorMap[String, String],
        names: String*
      ): Option[String] =
        names.toStream.flatMap { name =>
          row.get(name).orElse(row.find(_._1.equalsIgnoreCase(name)).map(_._2))
        }.headOption.map(_.trim).filterNot(_.isEmpty)

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
