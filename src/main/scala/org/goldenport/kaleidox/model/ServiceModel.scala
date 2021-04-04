package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox._
import org.goldenport.RAISE
import org.goldenport.context._
import org.goldenport.record.v2.{Schema, DataType, Multiplicity}
import org.goldenport.record.v3.{Column}
import org.goldenport.collection.VectorMap
import org.goldenport.values.PathName
import org.goldenport.sexpr.{SExpr, SScript}
import org.goldenport.sexpr.eval.LispFunction
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.kaleidox._

/*
 * @since   Mar. 13, 2021
 * @version Mar. 27, 2021
 * @author  ASAMI, Tomoharu
 */
case class ServiceModel(
  classes: VectorMap[String, ServiceModel.ServiceClass] = VectorMap.empty
) {
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
    operations: ServiceClass.Operations
  ) {
    def isEmpty = operations.isEmpty
    def toOption: Option[ServiceClass] = if (isEmpty) None else Some(this)

    def getFunction(name: String): Option[LispFunction] =
     operations.getOperation(name).map(_.toFunction)
  }

  object ServiceClass {
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
      method: Method
    ) {
      def toFunction: LispFunction = method.toFunction
    }

    case class Input(
      parameters: Parameters
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
            Failure(NonEmptyList.nel(TooMuchArgumentFault(paramnames), _faults))
          }

          private def _using_params: Vector[Parameter] = results.flatMap(_.parameter)

          private def _faults = results.map(_.result).flatMap {
            case Failure(fs) => fs.list
            case _ => Nil
          }.toList

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
        ps./:(Z())(_+_).r
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
        column.resolve(p.asObject).map(SExpr.create)
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
      result: Result
    )

    sealed trait Result
    object Result {
      val empty = VoidResult

      case object VoidResult extends Result
      case class DataResult(description: Description, datatype: DataType, multiplicity: Multiplicity) extends Result with Description.Holder {
      }
      def create(p: Map[String, String]): DataResult = {
        val datatype = take_datatype(p)
        val multiplicity = take_multiplicity(p)
        val desc = take_description(p)
        DataResult(Description(desc), datatype, multiplicity)
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

      case class KaleidoxMethod(in: Input, out: Output, script: List[SExpr]) extends Method {
        def toFunction = KaleidoxFunction(in, out, script)
      }

      case class ScriptMethod(in: Input, out: Output, script: SScript) extends Method {
        def toFunction = ScriptFunction(in, out, script)
      }

      case object UnimplementedFunction extends EvalFunction {
        val specification = FunctionSpecification("service-unimplemented", 0)
        def eval(p: SParameters) = { // TODO
          val x = p.argument1[SExpr](specification)
          SBoolean(x.isInstanceOf[SAtom])
        }
      }

      case class KaleidoxFunction(in: Input, out: Output, script: List[SExpr]) extends ApplyFunction {
        val params = in.parameters
        val specification = FunctionSpecification("service-kaleidox-script", params.length)
        def apply(u: LispContext): LispContext = _eval(u, specification, in, out, script)
      }

      case class ScriptFunction(in: Input, out: Output, script: SScript) extends ApplyFunction {
        val params = in.parameters
        val specification = FunctionSpecification("service-script", params.length)
        def apply(u: LispContext): LispContext = _eval(u, specification, in, out, List(script))
      }

      private def _eval(
        u: LispContext,
        spec: FunctionSpecification,
        in: Input,
        out: Output,
        script: List[SExpr]
      ): LispContext = {
        val params = in.parameters
        val paramlist = params.parameters.map(_.name).toList
        // TODO
        // see org.goldenport.kaleidox.lisp.Evaluator._get_specification
        // see org.goldenport.sexpr.eval.LispEvaluator.getSpecification
        // see org.goldenport.sexpr.eval.Evaluator.get_specification
        val a0 = u.parameters.argumentsUsingProperties(paramlist)
        val r = in.resolve(u, a0) match {
          case Success(a) =>
            val l = SLambda(paramlist, script)
            val b = SCell(l, SList.create(a))
            u.eval(b)
          case Failure(e) =>
            u.trace.fault(e.list)
            val c = Conclusion.argumentFault(e.list)
            SError(c)
        }
        u.toResult(r)
      }
    }

    def createOption(config: Config, p: Section): Option[ServiceClass] =
      new Builder(config).createOption(p)

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
        val xs = p.sections.flatMap(_get_operations)
        ServiceClass(name, Operations(xs)).toOption
      }

      private def _get_operations(p: Section): Vector[Operation] =
        if (_is_operation(p))
          p.sections.flatMap(_get_operation).toVector
        else
          Vector.empty

      private def _is_operation(p: Section) = p.keyForModel == "operation"

      private def _get_operation(p: Section): Option[Operation] = {
        val name = p.nameForModel
        val sections = p.sections
        val features = p.tables.headOption
        val in = sections.flatMap(_get_operation_in).headOption.getOrElse(RAISE.syntaxErrorFault("No in"))
        val out = sections.flatMap(_get_operation_out).headOption.getOrElse(RAISE.syntaxErrorFault("No out"))
        // val method = Method.UnimplementedMethod
        // val method = {
        //   val script = SScript("arg1 + arg2") // TODO
        //   Method.ScriptMethod(in, out, script)
        // }
        val method = sections.flatMap(_get_method(in, out, _)).headOption.getOrElse(Method.UnimplementedMethod)
        Some(Operation(name, in, out, method))
      }

      private def _get_operation_in(p: Section): Option[Input] =
        if (_is_in(p))
          Some(_to_operation_in(p))
        else
          None

      private def _is_in(p: Section) = p.keyForModel == "in"

      private def _to_operation_in(p: Section) = {
        val params = p.tables.headOption.map(_to_params).getOrElse(Parameters.empty)
        Input(params)
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

      private def _is_out(p: Section) = p.keyForModel == "out"

      private def _to_operation_out(p: Section) = {
        p.tables.headOption
        val result = p.tables.headOption.map(_to_result).getOrElse(Result.empty)
        Output(result)
      }

      private def _to_result(p: Table): Result =
        p.toVectorMapStringVector.headOption.map(Result.create).getOrElse(Result.empty)

      private def _get_method(in: Input, out: Output, p: Section): Option[Method] =
        if (_is_method(p))
          Some(_to_method(in, out, p))
        else
          None

      private def _is_method(p: Section) = p.keyForModel == "method"

      private def _to_method(in: Input, out: Output, p: Section): Method =
        p.sections match {
          case Nil =>
            val s = Script.parse(config, p.toText)
            Method.KaleidoxMethod(in, out, s.listSExpr)
          case x :: _ =>
            val name = x.nameForModel
            val s = SScript(name, p.toText)
            Method.ScriptMethod(in, out, s)
        }
    }
  }
}