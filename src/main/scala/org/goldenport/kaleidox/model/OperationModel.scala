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
 *  version Mar. 28, 2026
 *  version Apr. 13, 2026
 *  version May.  8, 2026
 * @version Jul. 15, 2026
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
    val valuemap = values.groupBy(_.name).map { case (k, v) => k -> v.head }
    val localinputnames = scala.collection.mutable.Set.empty[String]

    operations.map { op =>
      val output = op.outputType.map(_.trim).filterNot(_.isEmpty).getOrElse {
        _raise(s"Operation '${op.name}' requires OUTPUT.")
      }
      val inputtype = op.inputType.map(_.trim).filterNot(_.isEmpty)
      val parameters = op.parameters
      if (inputtype.isEmpty && parameters.isEmpty)
        _raise(s"Operation '${op.name}' requires INPUT or PARAMETER.")

      val kind = op.kind.orElse(_infer_kind(op.name, inputtype, valuemap)).getOrElse {
        _raise(s"Operation '${op.name}' requires TYPE (COMMAND|QUERY) or an INPUT value definition.")
      }

      val resolvedinputtype = inputtype.getOrElse {
        val suffix = kind match {
          case OperationKind.Command => "Command"
          case OperationKind.Query => "Query"
        }
        val base = s"${StringUtils.capitalize(op.name)}$suffix"
        if (!localinputnames.add(base))
          _raise(s"Operation-local input VALUE '$base' is defined more than once.")
        base
      }

      val resolvedvaluekind = valuemap.get(resolvedinputtype).map(_.kind).orElse {
        op.inputType.flatMap(valuemap.get).map(_.kind)
      }.getOrElse {
        kind match {
          case OperationKind.Command => InputValueKind.CommandValue
          case OperationKind.Query => InputValueKind.QueryValue
        }
      }

      _validate_kind(op.name, kind, resolvedvaluekind)
      _validate_dual_consistency(op.name, inputtype.flatMap(valuemap.get), parameters)

      NormalizedOperationDefinition(
        name = op.name,
        kind = kind,
        summary = op.summary,
        execution = op.execution,
        implementation = op.implementation,
        entityName = op.entityName,
        entityNames = op.entityNames,
        inputType = resolvedinputtype,
        inputSummary = op.inputSummary,
        inputDescription = op.inputDescription,
        outputType = output,
        outputSummary = op.outputSummary,
        outputDescription = op.outputDescription,
        inputValueKind = resolvedvaluekind,
        description = op.description,
        precondition = op.precondition,
        postcondition = op.postcondition,
        visibility = op.visibility,
        access = op.access,
        authorization = op.authorization,
        rules = op.rules,
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

  private def _infer_kind(
    opname: String,
    inputtype: Option[String],
    valuemap: Map[String, InputValueDefinition]
  ): Option[OperationKind] = {
    val valuekind = inputtype.flatMap(valuemap.get).map(_.kind).orElse {
      valuemap.get(s"${StringUtils.capitalize(opname)}Command").map(_.kind)
    }.orElse {
      valuemap.get(s"${StringUtils.capitalize(opname)}Query").map(_.kind)
    }
    valuekind.map {
      case InputValueKind.CommandValue => OperationKind.Command
      case InputValueKind.QueryValue => OperationKind.Query
    }
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

    def parse(p: String): Option[InputValueKind] =
      Option(p).map(_.trim.toLowerCase(java.util.Locale.ROOT)).collect {
        case "command" => CommandValue
        case "query" => QueryValue
      }
  }

  case class FieldDefinition(
    name: String,
    datatype: String,
    multiplicity: String = "1",
    label: Option[String] = None,
    controlType: Option[String] = None,
    placeholder: Option[String] = None,
    help: Option[String] = None,
    required: Option[Boolean] = None,
    confidentiality: Option[String] = None
  )

  case class InputValueDefinition(
    name: String,
    kind: InputValueKind,
    fields: Vector[FieldDefinition]
  )

  case class OperationDefinition(
    name: String,
    kind: Option[OperationKind] = None,
    summary: Option[String] = None,
    execution: Option[String] = None,
    implementation: Option[String] = None,
    entityName: Option[String] = None,
    entityNames: Vector[String] = Vector.empty,
    inputType: Option[String] = None,
    inputSummary: Option[String] = None,
    inputDescription: Option[String] = None,
    outputType: Option[String] = None,
    outputSummary: Option[String] = None,
    outputDescription: Option[String] = None,
    description: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None,
    visibility: Option[String] = None,
    access: Option[AccessDefinition] = None,
    authorization: Option[AuthorizationDefinition] = None,
    rules: Vector[String] = Vector.empty,
    parameters: Vector[FieldDefinition] = Vector.empty
  )

  case class AuthorizationDefinition(
    operationModes: Vector[String] = Vector.empty,
    allowAnonymous: Option[Boolean] = None,
    anonymousOperationModes: Vector[String] = Vector.empty
  )

  case class AccessDefinition(
    policy: String,
    resource: Option[String] = None,
    target: Option[String] = None,
    mode: Option[String] = None,
    relation: Option[String] = None,
    operationModel: Option[String] = None,
    entityUsage: Option[String] = None,
    entityOperationKind: Option[String] = None,
    entityApplicationDomain: Option[String] = None,
    condition: Option[String] = None
  )

  case class NormalizedOperationDefinition(
    name: String,
    kind: OperationKind,
    summary: Option[String],
    execution: Option[String],
    implementation: Option[String],
    entityName: Option[String] = None,
    entityNames: Vector[String] = Vector.empty,
    inputType: String,
    inputSummary: Option[String] = None,
    inputDescription: Option[String] = None,
    outputType: String,
    outputSummary: Option[String] = None,
    outputDescription: Option[String] = None,
    inputValueKind: InputValueKind,
    description: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None,
    visibility: Option[String] = None,
    access: Option[AccessDefinition] = None,
    authorization: Option[AuthorizationDefinition] = None,
    rules: Vector[String] = Vector.empty,
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
    val inputspec = _io_spec(p, "input")
    val outputspec = _io_spec(p, "output")
    val kindfromtype = kv.collectFirst {
      case (k, v) if k == "type" => OperationKind.parse(v).getOrElse(_raise(s"Operation '${p.nameForModel}' TYPE must be COMMAND or QUERY."))
    }
    val kindfrommarker = _kind_from_marker_section(p)
    val kind = kindfromtype.orElse(kindfrommarker)
    val input = inputspec.tpe.orElse(kv.collectFirst {
      case (k, v) if k == "input" => v.trim
    }.filterNot(Strings.blankp))
    val execution = kv.collectFirst {
      case (k, v) if k == "execution" || k == "directive" => v.trim
    }.filterNot(Strings.blankp)
    val implementation = kv.collectFirst {
      case (k, v) if k == "implementation" => v.trim
    }.filterNot(Strings.blankp)
    val output = outputspec.tpe.orElse(kv.collectFirst {
      case (k, v) if k == "output" || k == "result" => v.trim
    }.filterNot(Strings.blankp))
    val summary = kv.collectFirst {
      case (k, v) if k == "summary" => v.trim
    }.filterNot(Strings.blankp)
    val description = kv.collectFirst {
      case (k, v) if k == "description" => v.trim
    }.filterNot(Strings.blankp)
    val precondition = kv.collectFirst {
      case (k, v) if k == "precondition" || k == "pre-condition" => v.trim
    }.filterNot(Strings.blankp)
    val postcondition = kv.collectFirst {
      case (k, v) if k == "postcondition" || k == "post-condition" => v.trim
    }.filterNot(Strings.blankp)
    val access = _access_definition(p)
    val authorization = _authorization_definition(p)
    val entitynames = _entity_names(p)
    val rules = _rule_lines(p)
    val params = p.sections.filter(_.keyForModel == "parameter").toVector.flatMap(_parse_parameter_section)
    OperationDefinition(
      name = p.nameForModel.trim,
      kind = kind,
      summary = summary,
      execution = execution,
      implementation = implementation,
      entityName = entitynames.headOption,
      entityNames = entitynames,
      inputType = input,
      inputSummary = inputspec.summary,
      inputDescription = inputspec.description,
      outputType = output,
      outputSummary = outputspec.summary,
      outputDescription = outputspec.description,
      description = description,
      precondition = precondition,
      postcondition = postcondition,
      visibility = _visibility_text(p),
      access = access,
      authorization = authorization,
      rules = rules,
      parameters = params
    )
  }

  private case class IoSpec(
    tpe: Option[String] = None,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  private def _io_spec(
    p: Section,
    key: String
  ): IoSpec =
    p.sections.find(_.keyForModel == key).fold(IoSpec()) { s =>
      val tpe = s.sections.find(_.keyForModel == "type").flatMap(x => _section_body_text(x).linesIterator.map(_.trim).find(_.nonEmpty)).filterNot(Strings.blankp)
      val summary = s.sections.find(_.keyForModel == "summary").flatMap(x => _section_body_text(x).linesIterator.map(_.trim).find(_.nonEmpty)).filterNot(Strings.blankp)
      val description = s.sections.find(_.keyForModel == "description").flatMap(x => _section_body_text(x).linesIterator.map(_.trim).find(_.nonEmpty)).filterNot(Strings.blankp)
      IoSpec(tpe, summary, description)
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

  private def _rule_lines(
    p: Section
  ): Vector[String] =
    p.sections.find(_.keyForModel == "rule").toVector.flatMap { s =>
      CmlSectionFormat.valueLines(s.toText)
    }

  private def _access_definition(
    p: Section
  ): Option[AccessDefinition] =
    p.sections.find(_.keyForModel == "access").flatMap { s =>
      val kv = _merged_key_values(s).toMap
      val policy = kv.get("policy").map(_.trim).filterNot(Strings.blankp)
      policy.map { x =>
        AccessDefinition(
          policy = x,
          resource = kv.get("resource").map(_.trim).filterNot(Strings.blankp),
          target = kv.get("target").map(_.trim).filterNot(Strings.blankp),
          mode = kv.get("mode").orElse(kv.get("access_mode")).map(_.trim).filterNot(Strings.blankp),
          relation = kv.get("relation").orElse(kv.get("relation_rule")).map(_.trim).filterNot(Strings.blankp),
          operationModel = kv.get("operation_model").orElse(kv.get("operationmodel")).map(_.trim).filterNot(Strings.blankp),
          entityUsage = kv.get("entity_usage").orElse(kv.get("entityusage")).map(_.trim).filterNot(Strings.blankp),
          entityOperationKind = kv.get("entity_operation_kind").orElse(kv.get("entityoperationkind")).orElse(kv.get("operation_kind")).map(_.trim).filterNot(Strings.blankp),
          entityApplicationDomain = kv.get("entity_application_domain").orElse(kv.get("entityapplicationdomain")).orElse(kv.get("application_domain")).map(_.trim).filterNot(Strings.blankp),
          condition = kv.get("condition").orElse(kv.get("conditions")).orElse(kv.get("abac")).orElse(kv.get("abac_condition")).orElse(kv.get("abac_conditions")).orElse(kv.get("natural_condition")).orElse(kv.get("natural_conditions")).map(_.trim).filterNot(Strings.blankp)
        )
      }
    }

  private def _visibility_text(
    p: Section
  ): Option[String] =
    p.sections.find(_.keyForModel == "visibility").flatMap { s =>
      val text = CmlSectionFormat.valueLines(s.toText).mkString(" ").trim
      if (Strings.blankp(text)) None else Some(text)
    }.orElse {
      p.sections.find(_.keyForModel == "access").flatMap { s =>
        val kv = _merged_key_values(s).toMap
        kv.get("visibility").orElse(kv.get("resource_visibility")).map(_.trim).filterNot(Strings.blankp)
      }
    }

  private def _authorization_definition(
    p: Section
  ): Option[AuthorizationDefinition] =
    p.sections.find(s => s.keyForModel == "authorization" || s.keyForModel == "operation-authorization").map { s =>
      val kv = _merged_key_values(s).toMap
      AuthorizationDefinition(
        operationModes = _string_vector(kv, "operationmodes", "operation_modes", "modes"),
        allowAnonymous = _boolean(kv, "allowanonymous", "allow_anonymous"),
        anonymousOperationModes = _string_vector(kv, "anonymousoperationmodes", "anonymous_operation_modes", "anonymousmodes", "anonymous_modes")
      )
    }.filter { a =>
      a.operationModes.nonEmpty || a.allowAnonymous.nonEmpty || a.anonymousOperationModes.nonEmpty
    }

  private def _string_vector(
    kv: Map[String, String],
    keys: String*
  ): Vector[String] =
    keys.iterator.flatMap(kv.get).toSeq.headOption
      .map(_.split("[,|\\s]+").toVector.map(_.trim).filterNot(Strings.blankp))
      .getOrElse(Vector.empty)

  private def _boolean(
    kv: Map[String, String],
    keys: String*
  ): Option[Boolean] =
    keys.iterator.flatMap(kv.get).map(_.trim.toLowerCase(java.util.Locale.ROOT)).collectFirst {
      case "true" | "yes" | "on" | "1" => true
      case "false" | "no" | "off" | "0" => false
    }

  private def _entity_name(
    p: Section
  ): Option[String] =
    p.sections.find(_.keyForModel == "entity").flatMap(x => Option(_section_body_text(x)).map(_.trim).filterNot(Strings.blankp))

  private def _entity_names(
    p: Section
  ): Vector[String] =
    p.sections.find(_.keyForModel == "entity").toVector.flatMap { s =>
      s.toText.split("[,;\\n\\r]+").toVector.map(_.trim).filterNot(Strings.blankp)
    }

  private def _parse_parameter_section(
    p: Section
  ): Vector[FieldDefinition] = {
    val fromtables = p.tableList.toVector.flatMap(_table_fields)
    val fromsections = _named_field_sections(p)
    val fromtext =
      if (fromtables.isEmpty && fromsections.isEmpty)
        _field_lines(_section_body_text(p))
      else
        Vector.empty
    _merge_fields(fromtables ++ fromsections ++ fromtext)
  }

  private def _parse_attribute_section(
    p: Section
  ): Vector[FieldDefinition] = {
    val fromtables = p.tableList.toVector.flatMap(_table_fields)
    val fromsections = _named_field_sections(p)
    val fromtext =
      if (fromtables.isEmpty && fromsections.isEmpty)
        _field_lines(_section_body_text(p))
      else
        Vector.empty
    _merge_fields(fromtables ++ fromsections ++ fromtext)
  }

  private def _named_field_sections(
    p: Section
  ): Vector[FieldDefinition] =
    p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
      val kv = _merged_key_values(s).toMap
      _field_definition(
        s.nameForModel,
        kv.getOrElse("type", kv.getOrElse("datatype", "")),
        kv.getOrElse("multiplicity", "1"),
        kv
      )
    }.filter(_.datatype.nonEmpty)

  private def _merge_fields(
    ps: Vector[FieldDefinition]
  ): Vector[FieldDefinition] = {
    case class Z(xs: Vector[(String, FieldDefinition)]) {
      def +(rhs: FieldDefinition): Z = {
        val key = rhs.name.trim.toLowerCase
        xs.indexWhere(_._1 == key) match {
          case -1 => copy(xs = xs :+ (key -> rhs))
          case i =>
            val lhs = xs(i)._2
            val merged = FieldDefinition(
              name = lhs.name,
              datatype = if (rhs.datatype.nonEmpty) rhs.datatype else lhs.datatype,
              multiplicity = if (rhs.multiplicity.nonEmpty) rhs.multiplicity else lhs.multiplicity,
              label = rhs.label.orElse(lhs.label),
              controlType = rhs.controlType.orElse(lhs.controlType),
              placeholder = rhs.placeholder.orElse(lhs.placeholder),
              help = rhs.help.orElse(lhs.help),
              required = rhs.required.orElse(lhs.required),
              confidentiality = rhs.confidentiality.orElse(lhs.confidentiality)
            )
            copy(xs = xs.updated(i, key -> merged))
        }
      }
      def result: Vector[FieldDefinition] = xs.map(_._2)
    }
    ps.foldLeft(Z(Vector.empty))(_ + _).result
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
      } yield FieldDefinition(
        n,
        t,
        multi.trim,
        label = _string_from_record(r, "web-label", "weblabel"),
        controlType = _string_from_record(r, "web-control-type", "web-controltype", "webcontroltype", "web-control", "webcontrol", "web-widget", "webwidget"),
        placeholder = _string_from_record(r, "web-placeholder", "webplaceholder"),
        help = _string_from_record(r, "web-help", "webhelp"),
        required = _boolean_from_record(r, "web-required"),
        confidentiality = _string_from_record(r, "confidentiality", "confidentiality-level", "confidentialitylevel", "security-level", "securitylevel")
      )
    }
  }

  private def _string_from_record(
    record: org.goldenport.record.v3.Record,
    keys: String*
  ): Option[String] =
    record.getStringCaseInsensitive(keys.toVector).map(_.trim).filterNot(Strings.blankp)

  private def _field_definition(
    name: String,
    datatype: String,
    multiplicity: String,
    kv: Map[String, String]
  ): FieldDefinition =
    FieldDefinition(
      name = name,
      datatype = datatype,
      multiplicity = multiplicity,
      label = _string(kv, "web-label", "weblabel"),
      controlType = _string(kv, "web-control-type", "web-controltype", "webcontroltype", "web-control", "webcontrol", "web-widget", "webwidget"),
      placeholder = _string(kv, "web-placeholder", "webplaceholder"),
      help = _string(kv, "web-help", "webhelp"),
      required = _boolean(kv, "web-required", "webrequired"),
      confidentiality = _string(kv, "confidentiality", "confidentiality-level", "confidentialitylevel", "security-level", "securitylevel")
    )

  private def _string(
    kv: Map[String, String],
    keys: String*
  ): Option[String] =
    keys.iterator.flatMap(kv.get).map(_.trim).find(!Strings.blankp(_))

  private def _boolean_from_record(
    record: org.goldenport.record.v3.Record,
    key: String
  ): Option[Boolean] =
    record.getStringCaseInsensitive(Vector(key, key.replace("-", ""))).flatMap(x => _boolean(Map(key -> x), key))

  private def _merged_key_values(
    p: Section
  ): Vector[(String, String)] = {
    val fromtext = _key_values(_section_body_text(p))
    val fromdl = p.dls.toVector.flatMap(x => _key_values(x.toText))
    val fromsections = p.sections.toVector.flatMap { s =>
      val fromsectiontext = _key_values(_section_body_text(s))
      val fromsectiondl = s.dls.toVector.flatMap(x => _key_values(x.toText))
      val direct = fromsectiontext ++ fromsectiondl
      val key = s.keyForModel.toLowerCase
      val body = _section_body_text(s).linesIterator.map(_.trim).filterNot(_.isEmpty).mkString("\n")
      val sectionvalue = if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
      direct ++ sectionvalue
    }
    fromtext ++ fromdl ++ fromsections
  }

  private def _key_values(p: String): Vector[(String, String)] =
    CmlSectionFormat.keyValues(p)

  private def _section_body_text(p: Section): String = {
    val data = p.toData().trim
    if (data.nonEmpty)
      data
    else {
      val lines = p.toText.linesIterator.toVector
      val body = lines.dropWhile(x => x.trim.startsWith("#")).takeWhile(x => !x.trim.startsWith("#")).mkString("\n").trim
      if (body.nonEmpty) body else p.toText.trim
    }
  }

  private def _looks_like_heading_dump(p: String): Boolean =
    p.startsWith("#") || p.linesIterator.exists(x => x.trim.startsWith("###") || x.trim.startsWith("##"))

  private def _raise(message: String): Nothing =
    RAISE.syntaxErrorFault(message)
}
