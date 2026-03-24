package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section, Table}
import org.smartdox.Description
import org.smartdox.parser.Dox2Parser
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.Showable
import org.goldenport.kaleidox.CmlSectionFormat
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.parser.LogicalSection
import org.goldenport.util.StringUtils

/*
 * @since   Mar. 22, 2026
 * @version Mar. 24, 2026
 * @author  ASAMI, Tomoharu
 */
case class OperationModel(
  operations: Vector[OperationModel.OperationDefinition] = Vector.empty,
  values: Vector[OperationModel.InputValueDefinition] = Vector.empty,
  description: Description = Description.name("operation")
) extends Model.ISubModel {
  import OperationModel._

  protected def display_String: String =
    operations.map(_.name).mkString(",")

  protected def print_String: String =
    operations.map(_.name).mkString(",")

  protected def show_String: String =
    operations.map(_.name).mkString(",")

  def isEmpty: Boolean =
    operations.isEmpty && values.isEmpty

  def toOption: Option[OperationModel] =
    if (isEmpty) None else Some(this)

  def +(rhs: OperationModel): OperationModel =
    copy(
      operations = operations ++ rhs.operations,
      values = values ++ rhs.values
    )

  def normalizedOperations: Vector[NormalizedOperationDefinition] = {
    val valueMap = values.groupBy(_.name).map { case (k, v) => k -> v.head }
    val seen = scala.collection.mutable.Map.empty[String, Int]

    operations.map { op =>
      val kind = op.kind.getOrElse(_raise(s"Operation '${op.name}' requires TYPE (COMMAND|QUERY)."))
      val output = op.outputType.map(_.trim).filterNot(_.isEmpty).getOrElse {
        _raise(s"Operation '${op.name}' requires OUTPUT.")
      }
      val inputType = op.inputType.map(_.trim).filterNot(_.isEmpty)
      val parameters = op.parameters
      if (inputType.isEmpty && parameters.isEmpty)
        _raise(s"Operation '${op.name}' requires INPUT or PARAMETER.")

      val resolvedInputType = inputType.getOrElse {
        val base = s"${StringUtils.capitalize(op.name)}Input"
        val n = seen.getOrElse(base, 0) + 1
        seen.update(base, n)
        if (n == 1) base else s"${base}${n}"
      }

      val resolvedValueKind = valueMap.get(resolvedInputType).map(_.kind).orElse {
        op.inputType.flatMap(valueMap.get).map(_.kind)
      }.getOrElse {
        kind match {
          case OperationKind.Command => InputValueKind.CommandValue
          case OperationKind.Query => InputValueKind.QueryValue
        }
      }

      _validate_kind(op.name, kind, resolvedValueKind)
      _validate_dual_consistency(op.name, inputType.flatMap(valueMap.get), parameters)

      NormalizedOperationDefinition(
        name = op.name,
        kind = kind,
        inputType = resolvedInputType,
        outputType = output,
        inputValueKind = resolvedValueKind,
        description = op.description,
        parameters = parameters
      )
    }
  }

  private def _validate_kind(
    opname: String,
    opkind: OperationKind,
    valuekind: InputValueKind
  ): Unit =
    (opkind, valuekind) match {
      case (OperationKind.Command, InputValueKind.QueryValue) =>
        _raise(s"Operation '$opname' TYPE=COMMAND cannot use query-value input.")
      case (OperationKind.Query, InputValueKind.CommandValue) =>
        _raise(s"Operation '$opname' TYPE=QUERY cannot use command-value input.")
      case _ =>
        ()
    }

  private def _validate_dual_consistency(
    opname: String,
    input: Option[InputValueDefinition],
    params: Vector[FieldDefinition]
  ): Unit =
    input.foreach { in =>
      if (params.nonEmpty && in.fields != params)
        _raise(s"Operation '$opname' INPUT and PARAMETER are inconsistent.")
    }

  private def _raise(message: String): Nothing =
    RAISE.syntaxErrorFault(message)
}

object OperationModel {
  sealed trait OperationKind
  object OperationKind {
    case object Command extends OperationKind
    case object Query extends OperationKind

    def parse(p: String): Option[OperationKind] =
      Option(p).map(_.trim.toLowerCase).collect {
        case "command" => Command
        case "query" => Query
      }
  }

  sealed trait InputValueKind
  object InputValueKind {
    case object CommandValue extends InputValueKind
    case object QueryValue extends InputValueKind
  }

  case class FieldDefinition(
    name: String,
    datatype: String,
    multiplicity: String = "1"
  )

  case class InputValueDefinition(
    name: String,
    kind: InputValueKind,
    fields: Vector[FieldDefinition]
  )

  case class OperationDefinition(
    name: String,
    kind: Option[OperationKind] = None,
    inputType: Option[String] = None,
    outputType: Option[String] = None,
    description: Option[String] = None,
    parameters: Vector[FieldDefinition] = Vector.empty
  )

  case class NormalizedOperationDefinition(
    name: String,
    kind: OperationKind,
    inputType: String,
    outputType: String,
    inputValueKind: InputValueKind,
    description: Option[String] = None,
    parameters: Vector[FieldDefinition]
  )

  val empty = OperationModel()

  implicit object OperationModelMonoid extends Monoid[OperationModel] {
    def zero = OperationModel.empty
    def append(lhs: OperationModel, rhs: => OperationModel) = lhs + rhs
  }

  def create(config: Config, p: LogicalSection): OperationModel = {
    val dox = Dox2Parser.parse(config.doxConfig, p)
    _parse_dox(dox)
  }

  private def _parse_dox(p: Dox): OperationModel =
    p match {
      case m: Section => _parse_section(m)
      case m => m.elements.foldMap(_parse_dox)
    }

  private def _parse_section(p: Section): OperationModel =
    p.keyForModel match {
      case "operation" =>
        val ops = p.sections.toVector.map(_parse_operation_definition)
        OperationModel(operations = ops)
      case "command" =>
        val values = p.sections.toVector.map(_parse_value_definition(_, InputValueKind.CommandValue))
        OperationModel(values = values)
      case "query" =>
        val values = p.sections.toVector.map(_parse_value_definition(_, InputValueKind.QueryValue))
        OperationModel(values = values)
      case _ =>
        p.sections.foldMap(_parse_section)
    }

  private def _parse_operation_definition(
    p: Section
  ): OperationDefinition = {
    val kv = _merged_key_values(p)
    val kindFromType = kv.collectFirst {
      case (k, v) if k == "type" => OperationKind.parse(v).getOrElse(_raise(s"Operation '${p.nameForModel}' TYPE must be COMMAND or QUERY."))
    }
    val kindFromMarker = _kind_from_marker_section(p)
    val kind = kindFromType.orElse(kindFromMarker)
    val input = kv.collectFirst {
      case (k, v) if k == "input" => v.trim
    }.filterNot(Strings.blankp)
    val output = kv.collectFirst {
      case (k, v) if k == "output" || k == "result" => v.trim
    }.filterNot(Strings.blankp)
    val description = kv.collectFirst {
      case (k, v) if k == "description" => v.trim
    }.filterNot(Strings.blankp)
    val params = p.sections.filter(_.keyForModel == "parameter").toVector.flatMap(_parse_parameter_section)
    OperationDefinition(
      name = p.nameForModel.trim,
      kind = kind,
      inputType = input,
      outputType = output,
      description = description,
      parameters = params
    )
  }

  private def _kind_from_marker_section(
    p: Section
  ): Option[OperationKind] = {
    val markers = p.sections.toVector.flatMap { s =>
      s.keyForModel match {
        case "command" => Some(OperationKind.Command)
        case "query" => Some(OperationKind.Query)
        case _ => None
      }
    }.distinct
    markers match {
      case Vector() => None
      case Vector(one) => Some(one)
      case _ =>
        _raise(s"Operation '${p.nameForModel}' cannot define both COMMAND and QUERY markers.")
    }
  }

  private def _parse_value_definition(
    p: Section,
    kind: InputValueKind
  ): InputValueDefinition = {
    val fields = p.sections.filter(_.keyForModel == "attribute").toVector.flatMap(_parse_attribute_section)
    InputValueDefinition(
      name = p.nameForModel.trim,
      kind = kind,
      fields = fields
    )
  }

  private def _parse_parameter_section(
    p: Section
  ): Vector[FieldDefinition] = {
    val fromTables = p.tableList.toVector.flatMap(_table_fields)
    if (fromTables.nonEmpty)
      fromTables
    else
      _field_lines(_section_body_text(p))
  }

  private def _parse_attribute_section(
    p: Section
  ): Vector[FieldDefinition] = {
    val fromTables = p.tableList.toVector.flatMap(_table_fields)
    if (fromTables.nonEmpty)
      fromTables
    else
      _field_lines(_section_body_text(p))
  }

  private def _field_lines(
    p: String
  ): Vector[FieldDefinition] =
    CmlSectionFormat.fieldDefinitions(p).map { case (n, t, multi) =>
      FieldDefinition(n, t, multi)
    }

  private def _table_fields(
    table: Table
  ): Vector[FieldDefinition] = {
    val records = SimpleModelerUtils.toRecords(table).toVector
    records.flatMap { r =>
      val name = r.getStringCaseInsensitive(Vector("name"))
      val tpe = r.getStringCaseInsensitive(Vector("type"))
      val multi = r.getStringCaseInsensitive(Vector("multiplicity")).getOrElse("1")
      for {
        n <- name.map(_.trim).filterNot(_.isEmpty)
        t <- tpe.map(_.trim).filterNot(_.isEmpty)
      } yield FieldDefinition(n, t, multi.trim)
    }
  }

  private def _merged_key_values(
    p: Section
  ): Vector[(String, String)] = {
    val fromtext = _key_values(_section_body_text(p))
    val fromdl = p.dls.toVector.flatMap(x => _key_values(x.toText))
    val fromsections = p.sections.toVector.flatMap { s =>
      val fromsectiontext = _key_values(_section_body_text(s))
      val fromsectiondl = s.dls.toVector.flatMap(x => _key_values(x.toText))
      val direct = fromsectiontext ++ fromsectiondl
      if (direct.nonEmpty)
        direct
      else {
        val key = s.keyForModel.toLowerCase
        val body = _section_body_text(s).linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
        if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
      }
    }
    fromtext ++ fromdl ++ fromsections
  }

  private def _key_values(p: String): Vector[(String, String)] =
    CmlSectionFormat.keyValues(p)

  private def _section_body_text(p: Section): String =
    p.getStringIfOnlyText.map(_.trim).filterNot(_.isEmpty).getOrElse {
      val s = p.toText.trim
      if (p.sections.nonEmpty && _looks_like_heading_dump(s))
        ""
      else
        s
    }

  private def _looks_like_heading_dump(p: String): Boolean =
    p.startsWith("#") || p.linesIterator.exists(x => x.trim.startsWith("###") || x.trim.startsWith("##"))

  private def _raise(message: String): Nothing =
    RAISE.syntaxErrorFault(message)
}
