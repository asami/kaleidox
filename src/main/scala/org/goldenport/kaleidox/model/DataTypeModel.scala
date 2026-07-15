package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.goldenport.RAISE
import org.smartdox.Description
import org.smartdox.Table
import org.goldenport.values.Designation
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.{CFormat, CMaxLength, CMinLength, CRegex, Constraint, DataType}
import org.goldenport.record.v3.IRecord
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model

/*
 * @since   Oct. 12, 2023
 *  version Oct. 22, 2023
 * @version Jul. 15, 2026
 * @author  ASAMI, Tomoharu
 */
case class DataTypeModel(
  description: Description = Description.name("datatype"),
  classes: VectorMap[String, DataTypeModel.DataTypeClass] = VectorMap.empty
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def print_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def show_String: String = classes.values.map(x => x.name).mkString("\n")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[DataTypeModel] = if (isEmpty) None else Some(this)

  def +(rhs: DataTypeModel): DataTypeModel = copy(classes = classes ++ rhs.classes)
}

object DataTypeModel {
  val empty = DataTypeModel()

  private[kaleidox] def constraintRegex(p: String): scala.util.matching.Regex =
    scala.util.Try {
      java.util.regex.Pattern.compile(p)
      p.r
    }.getOrElse(
      RAISE.syntaxErrorFault(s"DATATYPE constraint 'pattern' requires a valid regular expression: '$p'.")
    )

  implicit object DataTypeModelMonoid extends Monoid[DataTypeModel] {
    def zero = DataTypeModel.empty
    def append(lhs: DataTypeModel, rhs: => DataTypeModel) = lhs + rhs
  }

  sealed trait DataTypeClass extends Description.Holder {
  }
  object DataTypeClass {
    case class Plain(
      description: Description,
      datatype: DataType,
      packageName: String = "domain", // TODO
      constraints: List[Constraint] = Nil
    ) extends DataTypeClass {
    }

    case class Complex(
      description: Description,
      constitutes: VectorMap[String, DataTypeClass],
      packageName: String = "domain" // TODO
    ) extends DataTypeClass {
    }

    def createOption(config: Config, p: LogicalSection): Option[DataTypeClass] =
      Builder(config).createOption(p)

    case class Builder(
      config: Config
    ) extends Model.ModelBuilderBase {
      type T = DataTypeClass

      protected def is_Accept(p: LogicalSection): Boolean = true

      protected def create_Model(
        p: LogicalSection,
        ps: Vector[LogicalSection],
        desc: Description
      ): T = {
        val pkg = _package_from_text(p.text)
        val rows = ps.filter(_is_attribute_section).flatMap { section =>
          _attributes_from_tables(dox_table_list(section)).orElse(_attributes_from_text_table(section.text)).getOrElse(Vector.empty)
        }
        _datatype_from_rows(desc, pkg, rows).getOrElse(_datatype_from_text(desc, pkg, p.text))
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
          getOrElse(_package_from_text(p.text))
        properties.getString("datatype").
          orElse(properties.getString("type")).
          orElse(properties.getString("value")).
          flatMap(_scalar_datatype).
          map(Plain(desc, _, pkg)).
          getOrElse(_datatype_from_text(desc, pkg, p.text))
      }

      protected def create_Model(
        p: LogicalSection,
        desc: Description,
        tables: List[Table]
      ): T = {
        val pkg = _package_from_text(p.text)
        val rows = _attributes_from_tables(tables).orElse(_attributes_from_text_table(p.text)).getOrElse(Vector.empty)
        _datatype_from_rows(desc, pkg, rows).getOrElse(_datatype_from_text(desc, pkg, p.text))
      }

      private def _datatype_from_rows(
        desc: Description,
        pkg: String,
        rows: Vector[AttributeRow]
      ): Option[T] =
        rows match {
          case Vector(AttributeRow(name, datatype, constraints)) if name.equalsIgnoreCase("value") =>
            Some(Plain(desc, datatype, pkg, constraints))
          case xs if xs.nonEmpty =>
            val constitutes = VectorMap(xs.map { row =>
              row.name -> Plain(Description.name(row.name), row.datatype, pkg, row.constraints)
            })
            Some(Complex(desc, constitutes, pkg))
          case _ => None
        }

      private def _datatype_from_text(
        desc: Description,
        pkg: String,
        text: String
      ): T = {
        val datatype = text.linesIterator.map(_.trim).filterNot(_.isEmpty).collectFirst {
          case _datatype_line(key, value) if key.equalsIgnoreCase("datatype") || key.equalsIgnoreCase("type") || key.equalsIgnoreCase("value") =>
            value.trim.stripPrefix("\"").stripSuffix("\"")
          case line if !line.contains("=") && !line.contains(":") && !line.startsWith("|") =>
            line
        }.flatMap(_scalar_datatype)
        datatype.map(Plain(desc, _, pkg)).getOrElse {
          RAISE.syntaxErrorFault(s"DATATYPE '${desc.name}' requires a scalar type or ATTRIBUTE table.")
        }
      }

      private case class AttributeRow(
        name: String,
        datatype: DataType,
        constraints: List[Constraint]
      )

      private def _attributes_from_tables(ps: List[Table]): Option[Vector[AttributeRow]] = {
        val rows = ps.toVector.flatMap(_.toVectorMapStringVector)
        if (rows.isEmpty)
          None
        else
          Some(rows.map(_attribute_from_table))
      }

      private def _attributes_from_text_table(text: String): Option[Vector[AttributeRow]] = {
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
            Some(body.map(row => _attribute_from_table(_to_row_map(header, row))))
        }
      }

      private def _attribute_from_table(row: VectorMap[String, String]): AttributeRow = {
        val name = _row_get(row, "name").
          getOrElse(RAISE.syntaxErrorFault("DATATYPE attribute table row requires name."))
        val datatype = _row_get(row, "type", "datatype").
          flatMap(_scalar_datatype).
          getOrElse(RAISE.syntaxErrorFault(s"DATATYPE attribute '$name' requires a supported scalar type."))
        AttributeRow(name, datatype, _constraints(row))
      }

      private def _constraints(row: VectorMap[String, String]): List[Constraint] = {
        val minlength = _row_get(row, "min-length", "min_length", "minLength").map(x => CMinLength(_int_constraint("min-length", x))).toList
        val maxlength = _row_get(row, "max-length", "max_length", "maxLength").map(x => CMaxLength(_int_constraint("max-length", x))).toList
        val pattern = _row_get(row, "pattern", "regex").map(x => CRegex(constraintRegex(x))).toList
        val format = _row_get(row, "format").map(CFormat.apply).toList
        minlength ++ maxlength ++ pattern ++ format
      }

      private def _int_constraint(name: String, p: String): Int =
        scala.util.Try(p.toInt).getOrElse(
          RAISE.syntaxErrorFault(s"DATATYPE constraint '$name' requires an integer value: '$p'.")
        )

      private def _scalar_datatype(p: String): Option[DataType] =
        DataType.get(p.trim.toLowerCase(java.util.Locale.ROOT))

      private def _package_from_text(text: String): String =
        text.linesIterator.collectFirst {
          case _datatype_line(key, value) if key.equalsIgnoreCase("package") || key.equalsIgnoreCase("package_name") =>
            value.trim.stripPrefix("\"").stripSuffix("\"")
        }.map(_.trim).filterNot(_.isEmpty).getOrElse("domain")

      private val _datatype_line = """\s*([A-Za-z_][A-Za-z0-9_\-]*)\s*[=:]\s*(.+)\s*""".r

      private def _is_attribute_section(p: LogicalSection): Boolean =
        p.keyForModel.equalsIgnoreCase("ATTRIBUTE") || p.nameForModel.equalsIgnoreCase("ATTRIBUTE")

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

      private def _row_get(
        row: VectorMap[String, String],
        names: String*
      ): Option[String] =
        names.toStream.flatMap { name =>
          row.get(name).orElse(row.find(_._1.equalsIgnoreCase(name)).map(_._2))
        }.headOption.map(_.trim).filterNot(_.isEmpty)

      case class Z(
        table: Vector[Table] = Vector.empty
      ) {
        def r = ???

        // def +(rhs: LogicalBlock) = rhs match {
        //   case StartBlock => this
        //   case EndBlock => this
        //   case m: LogicalSection => _section(m)
        //   case m: LogicalParagraph => this
        //   case m: LogicalVerbatim => this
        // }

        // private def _section(p: LogicalSection) =
        //   ???
      }
    }
  }

  def apply(p: DataTypeClass): DataTypeModel =
    DataTypeModel(classes = VectorMap(p.name -> p))

  def create(config: Config, p: LogicalSection): DataTypeModel =
    Builder(config).createOption(p) getOrElse DataTypeModel.empty

  case class Builder(config: Config) extends Model.ModelBuilderBase {
    type T = DataTypeModel

    protected def is_Accept(p: LogicalSection): Boolean =
      p.nameForModel equalsIgnoreCase "datatype"

    protected def create_Model(
      p: LogicalSection,
      ps: Vector[LogicalSection],
      desc: Description
    ): T = {
      val classes = ps.flatMap(DataTypeClass.createOption(config, _))
      DataTypeModel(desc, VectorMap(classes.map(x => x.name -> x)))
    }

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      properties: IRecord
    ): T = DataTypeModel.empty

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      tables: List[Table]
    ): T = DataTypeModel.empty
  }
}
