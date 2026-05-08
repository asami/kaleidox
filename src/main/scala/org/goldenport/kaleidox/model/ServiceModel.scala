package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox._
import org.smartdox.parser.Dox2Parser
import org.goldenport.{RAISE, Strings}
import org.goldenport.context._
import org.goldenport.i18n.I18NString
import org.goldenport.record.v2.{Schema, DataType, Multiplicity}
import org.goldenport.record.v3.{Column, ValueDomain}
import org.goldenport.collection.VectorMap
import org.goldenport.values.PathName
import org.goldenport.sexpr.{SExpr, SScript, SNil}
import org.goldenport.sexpr.eval.LispFunction
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.CmlSectionFormat
import org.goldenport.kaleidox.model.ValueModel.ValueClass
import org.goldenport.parser.LogicalSection

/*
 * @since   Mar. 13, 2021
 *  version Mar. 24, 2026
 *  version Mar. 27, 2021
 *  version Apr. 29, 2021
 *  version May. 27, 2021
 *  version Jun. 20, 2021
 *  version Oct.  1, 2022
 *  version Aug. 21, 2023
 * @version May.  8, 2026
 * @author  ASAMI, Tomoharu
 */
case class ServiceModel(
  classes: VectorMap[String, ServiceModel.ServiceClass] = VectorMap.empty,
  description: Description = Description.name("service")
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString(",")

  protected def print_String: String = classes.values.map(x => x.name).mkString(",")

  protected def show_String: String = classes.values.map(x => x.name).mkString(",")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[ServiceModel] = if (isEmpty) None else Some(this)

  def +(rhs: ServiceModel): ServiceModel = copy(classes = classes ++ rhs.classes)

  def getFunction(name: String): Option[LispFunction] = {
    val pn = PathName(name, ".")
    val servicename = pn.body
    val opname = pn.leaf
    classes.get(servicename).flatMap(_.getFunction(opname))
  }
}

object ServiceModel {
  val empty = ServiceModel()

  implicit object ServiceModelMonoid extends Monoid[ServiceModel] {
    def zero = ServiceModel.empty
    def append(lhs: ServiceModel, rhs: => ServiceModel) = lhs + rhs
  }

  def apply(p: ServiceClass): ServiceModel = ServiceModel(VectorMap(p.name -> p))

  case class ServiceClass(
    name: String,
    operations: ServiceClass.Operations,
    description: Option[String] = None,
    entityName: Option[String] = None,
    entityNames: Vector[String] = Vector.empty,
    access: Option[OperationModel.AccessDefinition] = None,
    useCases: Vector[ServiceClass.UseCaseDefinition] = Vector.empty
  ) {
    def isEmpty = operations.isEmpty
    def toOption: Option[ServiceClass] = if (isEmpty) None else Some(this)

    def getFunction(name: String): Option[LispFunction] =
     operations.getOperation(name).map(_.toFunction)
  }

  object ServiceClass {
    case class UseCaseDefinition(
      name: String,
      summary: Option[String] = None,
      description: Option[String] = None,
      actor: Option[String] = None,
      primaryActor: Option[String] = None,
      secondaryActor: Option[String] = None,
      supportingActor: Option[String] = None,
      stakeholder: Option[String] = None,
      goal: Option[String] = None,
      precondition: Option[String] = None,
      postcondition: Option[String] = None,
      scenarios: Vector[UseCaseScenario] = Vector.empty
    )

    case class UseCaseScenario(
      name: String,
      summary: Option[String] = None,
      description: Option[String] = None,
      steps: Vector[String] = Vector.empty,
      alternates: Vector[String] = Vector.empty,
      exceptions: Vector[String] = Vector.empty
    )

    case class Operations(operations: VectorMap[String, Operation] = VectorMap.empty) {
      def isEmpty = operations.isEmpty
      def getOperation(name: String): Option[Operation] = operations.get(name)
    }
    object Operations {
      def apply(ps: Seq[Operation]): Operations = {
        val a = VectorMap(ps.map(x => x.name -> x))
        Operations(a)
      }
    }

    case class Operation(
      name: String,
      input: Input,
      output: Output,
      method: Method,
      kind: Option[OperationModel.OperationKind] = None,
      summary: Option[String] = None,
      description: Option[String] = None,
      precondition: Option[String] = None,
      postcondition: Option[String] = None,
      execution: Option[String] = None,
      implementation: Option[String] = None,
      entityName: Option[String] = None,
      entityNames: Vector[String] = Vector.empty,
      visibility: Option[String] = None,
      access: Option[OperationModel.AccessDefinition] = None,
      authorization: Option[OperationModel.AuthorizationDefinition] = None,
      rules: Vector[String] = Vector.empty,
      parameters: Vector[OperationModel.FieldDefinition] = Vector.empty
    ) {
      def toFunction: LispFunction = method.toFunction
    }

    case class Input(
      parameters: Parameters,
      tpe: Option[String] = None,
      value: Option[ValueClass] = None,
      summary: Option[String] = None,
      description: Option[String] = None
    ) {
      def resolve(u: LispContext, ps: List[SExpr]): ValidationNel[ArgumentFault, List[SExpr]] = parameters.resolve(u, ps)
    }

    case class Parameters(parameters: Vector[Parameter] = Vector.empty) {
      def length = parameters.length

      def resolve(u: LispContext, ps: List[SExpr]): ValidationNel[ArgumentFault, List[SExpr]] = {
        case class Slot(parameter: Option[Parameter], result: ValidationNel[ArgumentFault, SExpr])
        case class Z(
          params: Vector[Parameter] = parameters,
          results: Vector[Slot] = Vector.empty
        ) {
          def r =
            if (parameters.length == results.length)
              _result
            else if (parameters.length > results.length)
              _missing
            else
              _toomuch

          private def _result = _sequence(results.map(_.result).toList)

          private def _missing: ValidationNel[ArgumentFault, List[SExpr]] = {
            val missings = parameters diff _using_params
            val paramnames = missings.map(_.name)
            Failure(NonEmptyList.nel(MissingArgumentFault(paramnames), _faults))
          }

          private def _toomuch = {
            val toomuchs = _using_params diff parameters
            val paramnames = toomuchs.map(_.name)
            Failure(NonEmptyList.nel(TooManyArgumentsFault(paramnames), _faults))
          }

          private def _using_params: Vector[Parameter] = results.flatMap(_.parameter)

          private def _faults: IList[ArgumentFault] = {
            val a = results.map(_.result).flatMap {
              case Failure(fs) => fs.list.toList
              case _ => Nil
            }.toList
            IList.fromList(a)
          }

          private def _sequence(p: List[ValidationNel[ArgumentFault, SExpr]]): ValidationNel[ArgumentFault, List[SExpr]] =
            p match {
              case Nil => Validation.success(Nil)
              case x :: xs => _sequence(x.map(List(_)), xs)
            }

          private def _sequence(
            h: ValidationNel[ArgumentFault, List[SExpr]],
            t: List[ValidationNel[ArgumentFault, SExpr]]
          ): ValidationNel[ArgumentFault, List[SExpr]] = t match {
            case Nil => h
            case x :: xs =>
              val a = h |+| x.map(List(_))
              _sequence(a, xs)
          }

          def +(rhs: SExpr) =
            params.headOption.
              map(x => copy(params = params.tail, results = results :+ Slot(Option(x), x.resolve(rhs)))).
              getOrElse(copy(results = results :+ Slot(None, Validation.success(rhs))))
        }
        ps.foldLeft(Z())(_+_).r
      }
    }
    object Parameters {
      val empty = Parameters()
    }

    case class Parameter(
      description: Description,
      column: Column
    ) extends Description.Holder {
      def datatype = column.datatype

      def resolve(p: SExpr): ValidationNel[ArgumentFault, SExpr] =
        column.resolve(p.asObject) match {
          case Success(s) => Success(SExpr.create(s))
          case Failure(e) => Failure(NonEmptyList(InvalidArgumentFault(column.name, e.list.toList)))
        }
    }
    object Parameter {
      def create(p: Map[String, String]): Parameter = {
        val name = take_name(p)
        val datatype = take_datatype(p)
        val multiplicity = take_multiplicity(p)
        val column = Column(name, datatype, multiplicity)
        val desc = take_description(p)
        Parameter(Description.name(name, desc), column)
      }
    }

    case class Output(
      result: Result,
      tpe: Option[String] = None,
      value: Option[ValueClass] = None,
      summary: Option[String] = None,
      description: Option[String] = None
    ) {
      def resolve(u: LispContext, p: SExpr): ValidationNel[ResultFault, SExpr] = result.resolve(u, p)
    }

    sealed trait Result {
      def resolve(u: LispContext, p: SExpr): ValidationNel[ResultFault, SExpr]

    }
    object Result {
      val empty = VoidResult

      case object VoidResult extends Result {
        def resolve(u: LispContext, p: SExpr): ValidationNel[ResultFault, SExpr] = Success(SNil)
      }

      case class DataResult(
        description: Description,
        domain: ValueDomain
      ) extends Result with Description.Holder {
        def resolve(u: LispContext, p: SExpr): ValidationNel[ResultFault, SExpr] =
          domain.resolve(p.asObject) match {
            case Success(s) => Success(SExpr.create(s))
            case Failure(e) =>
              Failure(NonEmptyList(ValueDomainResultFault(I18NString.mkI18NString(e.map(_.message.toI18NString), ";"))))
          }
      }

      def create(p: Map[String, String]): DataResult = {
        val datatype = take_datatype(p)
        val multiplicity = take_multiplicity(p)
        val desc = take_description(p)
        DataResult(Description(desc), ValueDomain(datatype, multiplicity))
      }
    }

    sealed trait Method {
      def toFunction: LispFunction
    }
    object Method {
      import org.goldenport.sexpr._
      import org.goldenport.sexpr.eval.{Parameters => SParameters, _}

      case object UnimplementedMethod extends Method {
        def toFunction = UnimplementedFunction
      }

      case class KaleidoxMethod(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        script: List[SExpr]
      ) extends Method {
        def toFunction = KaleidoxFunction(service, operation, in, out, script)
      }

      case class ScriptMethod(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        script: SScript
      ) extends Method {
        def toFunction = ScriptFunction(service, operation, in, out, script)
      }

      case object UnimplementedFunction extends ParameterEvalFunction {
        val specification = FunctionSpecification("service-unimplemented", 0)
        def eval(p: SParameters) = { // TODO
          val x = p.argument1[SExpr](specification)
          SBoolean(x.isInstanceOf[SAtom])
        }
      }

      case class KaleidoxFunction(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        script: List[SExpr]
      ) extends ApplyFunction {
        override def kindName = "Operation"
        val params = in.parameters
        val specification = FunctionSpecification(s"service-script-kaleidox(${service}.${operation})", params.length)
        def apply(u: LispContext): LispContext = _eval(service, operation, u, specification, in, out, script)
      }

      case class ScriptFunction(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        script: SScript
      ) extends ApplyFunction {
        override def kindName = "Operation"
        val params = in.parameters
        val specification = FunctionSpecification("service-script", params.length)
        def apply(u: LispContext): LispContext = _eval(service, operation, u, specification, in, out, List(script))
      }

      private def _eval(
        service: String,
        operation: String,
        u: LispContext,
        spec: FunctionSpecification,
        in: Input,
        out: Output,
        script: List[SExpr]
      ): LispContext = {
        val label = s"${service}.${operation}"
        val params = in.parameters
        val paramlist = params.parameters.map(_.name).toList
        // TODO
        // see org.goldenport.kaleidox.lisp.Evaluator._get_specification
        // see org.goldenport.sexpr.eval.LispEvaluator.getSpecification
        // see org.goldenport.sexpr.eval.Evaluator.get_specification
        val a0 = u.parameters.argumentsUsingProperties(paramlist)
        val r0 = in.resolve(u, a0) match {
          case Success(a) =>
            val l = SLambda(label, paramlist, script)
            val b = SCell(l, SList.create(a))
            u.eval(b)
          case Failure(e) =>
            u.trace.fault(e.list.toList)
            val c = Conclusion.argumentFault(e.list.toList)
            SError(c)
        }
        val r = out.resolve(u, r0) match {
          case Success(a) => a
          case Failure(e) => 
            u.trace.fault(e.list.toList)
            val c = Conclusion.resultFault(e.list.toList)
            SError(c)
        }
        u.toResult(r)
      }
    }

    def createOption(config: Config, p: Section): Option[ServiceClass] =
      new Builder(config).createOption(p)

    def createOption(config: Config, p: LogicalSection): Option[ServiceClass] = {
      val dox = Dox2Parser.parse(config.doxConfig, p)
      _find_section(dox, p.nameForModel).flatMap(new Builder(config).createOption)
    }

    private def _find_section(p: Dox, name: String): Option[Section] = p match {
      case m: Section if m.nameForModel == name => Some(m)
      case m => m.elements.view.flatMap(_find_section(_, name)).headOption
    }

    protected final def take_name(p: Map[String, String]): String =
      p.get("名前").getOrElse("")

    protected final def take_datatype(p: Map[String, String]): DataType =
      DataType.to(p.get("型").getOrElse("string"))

    protected final def take_multiplicity(p: Map[String, String]): Multiplicity =
      Multiplicity.to(p.get("多重度").getOrElse("1"))

    protected final def take_description(p: Map[String, String]): Dox =
      Dox.text(p.get("説明").getOrElse(""))

    class Builder(val config: Config) {
      val autoCapitalize: Boolean = false

      def createOption(p: Section): Option[ServiceClass] = {
        val name = p.nameForModel
        // p.tables
        val xs = p.sections.flatMap(_get_operations(name, _))
        val entities = _entity_names(p)
        ServiceClass(
          name,
          Operations(xs),
          _description_text(p),
          entities.headOption,
          entities,
          _access_definition(p),
          _use_case_definitions(p)
        ).toOption
      }

      private def _get_operations(service: String, p: Section): Vector[Operation] =
        if (_is_operation(p))
          p.sections.flatMap(_get_operation(service, _)).toVector
        else
          Vector.empty

      private def _is_operation(p: Section) = _key_is(p, "operation")

      private def _get_operation(service: String, p: Section): Option[Operation] = {
        val name = p.nameForModel
        val sections = p.sections
        val features = p.tables.headOption
        val in = sections.flatMap(_get_operation_in).headOption.getOrElse(RAISE.syntaxErrorFault("No input"))
        val out = sections.flatMap(_get_operation_out).headOption.getOrElse(RAISE.syntaxErrorFault("No output"))
        // val method = Method.UnimplementedMethod
        // val method = {
        //   val script = SScript("arg1 + arg2") // TODO
        //   Method.ScriptMethod(in, out, script)
        // }
        val method = sections.flatMap(_get_method(service, name, in, out, _)).headOption.getOrElse(Method.UnimplementedMethod)
        Some(Operation(
          name = name,
          input = in,
          output = out,
          method = method,
          kind = _kind_opt(p),
          summary = _summary_text(p),
          description = _description_text(p),
          precondition = _precondition_text(p),
          postcondition = _postcondition_text(p),
          execution = _execution_text(p),
          implementation = _implementation_text(p),
          entityName = _entity_names(p).headOption,
          entityNames = _entity_names(p),
          visibility = _visibility_text(p),
          access = _access_definition(p),
          authorization = _authorization_definition(p),
          rules = _rule_lines(p),
          parameters = _parameter_fields(p)
        ))
      }

      private def _get_operation_in(p: Section): Option[Input] =
        if (_is_in(p))
          Some(_to_operation_in(p))
        else
          None

      private def _is_in(p: Section) = {
        val k = _key(p)
        k == "in" || k == "input"
      }

      private def _to_operation_in(p: Section) = {
        val params = p.tables.headOption.map(_to_params).getOrElse(Parameters.empty)
        val value = _inline_value(p)
        Input(
          parameters = params,
          tpe = _type_text(p).orElse(value.map(_.name)),
          value = value,
          summary = _summary_text(p),
          description = _description_text(p)
        )
      }

      private def _to_params(p: Table): Parameters = {
        val xs = p.toVectorMapStringVector.map(Parameter.create)
        Parameters(xs)
      }

      private def _get_operation_out(p: Section): Option[Output] =
        if (_is_out(p))
          Some(_to_operation_out(p))
        else
          None

      private def _is_out(p: Section) = {
        val k = _key(p)
        k == "out" || k == "output"
      }

      private def _to_operation_out(p: Section) = {
        p.tables.headOption
        val result = p.tables.headOption.map(_to_result).getOrElse(Result.empty)
        val value = _inline_value(p)
        Output(
          result = result,
          tpe = _type_text(p).orElse(value.map(_.name)),
          value = value,
          summary = _summary_text(p),
          description = _description_text(p)
        )
      }

      private def _to_result(p: Table): Result =
        p.toVectorMapStringVector.headOption.map(Result.create).getOrElse(Result.empty)

      private def _get_method(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        p: Section
      ): Option[Method] =
        if (_is_method(p))
          Some(_to_method(service, operation, in, out, p))
        else
          None

      private def _is_method(p: Section) = _key_is(p, "method")

      private def _description_text(p: Section): Option[String] =
        p.sections.find(_key_is(_, "description")).
          map(_.toText.trim).
          filter(_.nonEmpty)

      private def _use_case_definitions(p: Section): Vector[UseCaseDefinition] =
        p.sections.toVector.filter(s => _is_use_case_key(s.keyForModel)).flatMap { s =>
          s.sections.toVector.map(_parse_use_case_definition)
        }

      private def _parse_use_case_definition(p: Section): UseCaseDefinition = {
        val kv = _merged_key_values(p)
        UseCaseDefinition(
          name = p.nameForModel.trim,
          summary = _value_opt(kv, "summary"),
          description = _description_text(p),
          actor = _value_opt(kv, "actor"),
          primaryActor = _value_opt(kv, "primary actor", "primaryactor", "primary_actor"),
          secondaryActor = _value_opt(kv, "secondary actor", "secondaryactor", "secondary_actor"),
          supportingActor = _value_opt(kv, "supporting actor", "supportingactor", "supporting_actor"),
          stakeholder = _value_opt(kv, "stakeholder", "stakeholders"),
          goal = _value_opt(kv, "goal"),
          precondition = _value_opt(kv, "precondition", "pre-condition"),
          postcondition = _value_opt(kv, "postcondition", "post-condition"),
          scenarios = _use_case_scenarios(p)
        )
      }

      private def _use_case_scenarios(p: Section): Vector[UseCaseScenario] =
        p.sections.toVector.filter(_key_is(_, "scenario")).flatMap { s =>
          s.sections.toVector.map(_parse_use_case_scenario)
        }

      private def _parse_use_case_scenario(p: Section): UseCaseScenario = {
        val kv = _merged_key_values(p)
        UseCaseScenario(
          name = p.nameForModel.trim,
          summary = _value_opt(kv, "summary"),
          description = _description_text(p),
          steps = _scenario_steps(p),
          alternates = _scenario_value_sections(p, "alternate"),
          exceptions = _scenario_value_sections(p, "exception")
        )
      }

      private def _summary_text(p: Section): Option[String] =
        p.sections.find(_key_is(_, "summary")).
          flatMap(_section_body_text)

      private def _precondition_text(p: Section): Option[String] =
        p.sections.find(s => _key_is(s, "precondition", "pre-condition")).
          flatMap(_section_body_text)

      private def _postcondition_text(p: Section): Option[String] =
        p.sections.find(s => _key_is(s, "postcondition", "post-condition")).
          flatMap(_section_body_text)

      private def _rule_lines(p: Section): Vector[String] =
        p.sections.find(_key_is(_, "rule")).toVector.flatMap(s => CmlSectionFormat.valueLines(s.toText))

      private def _execution_text(p: Section): Option[String] =
        p.sections.find(s => _key_is(s, "execution", "directive")).
          flatMap(_section_body_text).map(_.trim).filterNot(Strings.blankp)

      private def _implementation_text(p: Section): Option[String] =
        p.sections.find(_key_is(_, "implementation")).
          flatMap(_section_body_text).map(_.trim).filterNot(Strings.blankp)

      private def _parameter_fields(p: Section): Vector[OperationModel.FieldDefinition] =
        p.sections.filter(_key_is(_, "parameter")).toVector.flatMap(_parse_parameter_section)

      private def _parse_parameter_section(p: Section): Vector[OperationModel.FieldDefinition] = {
        val fromTables = p.tableList.toVector.flatMap(_table_fields)
        val fromText = if (fromTables.isEmpty) _field_lines(_section_body_text(p).getOrElse("")) else Vector.empty
        fromTables ++ fromText
      }

      private def _field_lines(p: String): Vector[OperationModel.FieldDefinition] =
        CmlSectionFormat.fieldDefinitions(p).map { case (n, t, multi) =>
          OperationModel.FieldDefinition(n, t, multi)
        }

      private def _table_fields(table: Table): Vector[OperationModel.FieldDefinition] = {
        val records = SimpleModelerUtils.toRecords(table).toVector
        records.flatMap { r =>
          val name = r.getStringCaseInsensitive(Vector("name"))
          val tpe = r.getStringCaseInsensitive(Vector("type"))
          val multi = r.getStringCaseInsensitive(Vector("multiplicity")).getOrElse("1")
          for {
            n <- name.map(_.trim).filterNot(_.isEmpty)
            t <- tpe.map(_.trim).filterNot(_.isEmpty)
          } yield OperationModel.FieldDefinition(
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

      private def _boolean_from_record(
        record: org.goldenport.record.v3.Record,
        key: String
      ): Option[Boolean] =
        record.getStringCaseInsensitive(Vector(key, key.replace("-", ""))).flatMap(x => _boolean(Map(key -> x), key))

      private def _authorization_definition(
        p: Section
      ): Option[OperationModel.AuthorizationDefinition] =
        p.sections.find(s => _key_is(s, "authorization", "operation-authorization")).map { s =>
          val kv = _merged_key_values(s).toMap
          OperationModel.AuthorizationDefinition(
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

      private def _access_definition(
        p: Section
      ): Option[OperationModel.AccessDefinition] =
        p.sections.find(_key_is(_, "access")).flatMap { s =>
          val kv = _merged_key_values(s)
          val policy = _value_opt(kv, "policy")
          policy.map { x =>
            OperationModel.AccessDefinition(
              policy = x,
              resource = _value_opt(kv, "resource"),
              target = _value_opt(kv, "target"),
              mode = _value_opt(kv, "mode").orElse(_value_opt(kv, "access_mode")),
              relation = _value_opt(kv, "relation").orElse(_value_opt(kv, "relation_rule")),
              operationModel = _value_opt(kv, "operation_model").orElse(_value_opt(kv, "operationmodel")),
              entityUsage = _value_opt(kv, "entity_usage").orElse(_value_opt(kv, "entityusage")),
              entityOperationKind = _value_opt(kv, "entity_operation_kind").orElse(_value_opt(kv, "entityoperationkind")).orElse(_value_opt(kv, "operation_kind")),
              entityApplicationDomain = _value_opt(kv, "entity_application_domain").orElse(_value_opt(kv, "entityapplicationdomain")).orElse(_value_opt(kv, "application_domain")),
              condition = _value_opt(kv, "condition")
                .orElse(_value_opt(kv, "conditions"))
                .orElse(_value_opt(kv, "abac"))
                .orElse(_value_opt(kv, "abac_condition"))
                .orElse(_value_opt(kv, "abac_conditions"))
                .orElse(_value_opt(kv, "natural_condition"))
                .orElse(_value_opt(kv, "natural_conditions"))
            )
          }
        }

      private def _visibility_text(
        p: Section
      ): Option[String] =
        p.sections.find(_key_is(_, "visibility")).flatMap { s =>
          val text = CmlSectionFormat.valueLines(s.toText).mkString(" ").trim
          if (Strings.blankp(text)) None else Some(text)
        }.orElse {
          p.sections.find(_key_is(_, "access")).flatMap { s =>
            val kv = _merged_key_values(s)
            _value_opt(kv, "visibility").orElse(_value_opt(kv, "resource_visibility"))
          }
        }

      private def _entity_name(
        p: Section
      ): Option[String] =
        p.sections.find(_key_is(_, "entity")).flatMap(_section_body_text).map(_.trim).filterNot(Strings.blankp)

      private def _entity_names(
        p: Section
      ): Vector[String] =
        p.sections.find(_key_is(_, "entity")).toVector.flatMap { s =>
          s.toText.split("[,;\\n\\r]+").toVector.map(_.trim).filterNot(Strings.blankp)
        }

      private def _type_text(p: Section): Option[String] =
        p.sections.find(_key_is(_, "type")).
          flatMap(_section_body_text)

      private def _kind_opt(p: Section): Option[OperationModel.OperationKind] =
        p.sections.find(_key_is(_, "type")).
          flatMap(_section_body_text).
          flatMap(OperationModel.OperationKind.parse)

      private def _inline_value(p: Section): Option[ValueClass] =
        p.sections.find(_key_is(_, "value")).
          flatMap(_.sections.headOption).
          flatMap(SchemaModel.SchemaClass.createOption).
          map(ValueClass(_))

      private def _key(p: Section): String =
        p.keyForModel.toLowerCase(java.util.Locale.ROOT)

      private def _key_is(p: Section, names: String*): Boolean = {
        val key = _key(p)
        names.exists(_.equalsIgnoreCase(key))
      }

      private def _section_body_text(p: Section): Option[String] =
        Option(_section_body_data(p)).
          map(_.linesIterator.map(_.trim).find(_.nonEmpty).orNull).
          filterNot(Strings.blankp)

      private def _value_opt(
        kv: Vector[(String, String)],
        ks: String*
      ): Option[String] =
        kv.collectFirst {
          case (k, v) if ks.contains(k) => v.trim
        }.filterNot(Strings.blankp)

      private def _merged_key_values(
        p: Section
      ): Vector[(String, String)] = {
        val fromtext = _key_values(p.toText)
        val fromsections = p.sections.toVector.flatMap { s =>
          val sectionbody = _section_body_data(s)
          val fromsectiontext = _key_values(sectionbody)
          val key = s.keyForModel.toLowerCase
          val body = sectionbody.linesIterator.map(_.trim).filterNot(_.isEmpty).mkString("\n")
          val sectionvalue = if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
          if (fromsectiontext.nonEmpty)
            fromsectiontext ++ sectionvalue
          else {
            sectionvalue
          }
        }
        fromtext ++ fromsections
      }

      private def _key_values(
        p: String
      ): Vector[(String, String)] =
        CmlSectionFormat.keyValues(p)

      private def _section_body_data(p: Section): String = {
        val data = p.toData().trim
        if (data.nonEmpty)
          data
        else
          p.toText.trim
      }

      private def _value_lines(
        p: String
      ): Vector[String] =
        CmlSectionFormat.valueLines(p)

      private def _scenario_steps(
        p: Section
      ): Vector[String] = {
        val explicit = _scenario_value_sections(p, "step")
        if (explicit.nonEmpty)
          explicit
        else {
          val lines = p.toText.linesIterator.map(_.trim).filterNot(_.isEmpty).toVector
          val xs =
            if (lines.size > 1)
              lines.flatMap(_split_scenario_steps)
            else
              lines.headOption.map(_split_scenario_steps).getOrElse(Vector.empty)
          if (xs.nonEmpty) xs else lines
        }
      }

      private def _scenario_value_sections(
        p: Section,
        key: String
      ): Vector[String] =
        p.sections.toVector.filter(_.keyForModel.equalsIgnoreCase(key)).flatMap { s =>
          val lines = s.toText.linesIterator.map(_.trim).filterNot(_.isEmpty).toVector
          val xs =
            if (lines.size > 1)
              lines.flatMap(_split_scenario_steps)
            else
              lines.headOption.map(_split_scenario_steps).getOrElse(Vector.empty)
          if (xs.nonEmpty) xs else lines
        }

      private def _split_scenario_steps(
        p: String
      ): Vector[String] = {
        val s = p.trim
        val numbered = "(?=\\d+\\.\\s+)".r.split(s).toVector.map(_.trim).filterNot(_.isEmpty)
        if (numbered.size > 1)
          numbered
        else
          _split_sentence_steps(s)
      }

      private def _split_sentence_steps(
        p: String
      ): Vector[String] = {
        val xs = "(?<=[.!?])(?=[A-Z])".r.split(p).toVector.map(_.trim).filterNot(_.isEmpty)
        if (xs.size > 1) xs else Vector(p)
      }

      private def _is_use_case_key(p: String): Boolean = {
        val s = Option(p).map(_.trim.toLowerCase).getOrElse("")
        s == "use case" || s == "usecase"
      }

      private def _to_method(
        service: String,
        operation: String,
        in: Input,
        out: Output,
        p: Section
      ): Method =
        p.sections match {
          case Nil =>
            val s = Script.parse(config, p.toText)
            Method.KaleidoxMethod(service, operation, in, out, s.listSExpr)
          case x :: _ =>
            val name = x.nameForModel
            val s = SScript(name, p.toText)
            Method.ScriptMethod(service, operation, in, out, s)
        }
    }
  }
}
