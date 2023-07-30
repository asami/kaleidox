package org.goldenport.kaleidox.lisp

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.log.Loggable
import org.goldenport.trace.TraceContext
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{LispBinding, Parameters, LispContext}
import org.goldenport.sexpr.eval.LispFunction
import org.goldenport.sexpr.eval.FunctionSpecification
import org.goldenport.sexpr.eval.IncidentSequence
import org.goldenport.util.StringUtils
import org.goldenport.kaleidox._

/*
 * @since   Aug. 19, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Feb. 28, 2019
 *  version Mar.  9, 2019
 *  version Apr. 21, 2019
 *  version May. 21, 2019
 *  version Jun.  9, 2019
 *  version Aug.  4, 2019
 *  version Sep. 28, 2019
 *  version Oct. 14, 2019
 *  version Nov.  8, 2019
 *  version Feb. 29, 2020
 *  version Jan. 16, 2021
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 *  version Apr. 25, 2021
 *  version May. 10, 2021
 *  version Sep. 20, 2021
 *  version Nov. 29, 2021
 *  version Apr. 24, 2022
 *  version May.  5, 2022
 *  version Nov. 28, 2022
 * @version Jul. 29, 2023
 * @author  ASAMI, Tomoharu
 */
case class Evaluator(
  context: ExecutionContext,
  universe: Universe
) extends Loggable {
  import Evaluator._

  private val _binding = new Evaluator.Binding(universe)
  private val _evaluator = new org.goldenport.sexpr.eval.LispEvaluator[Context] {
    def config = context.config
    def dateTimeContext = config.dateTimeContext
    def i18nContext = config.i18nContext
    def queryContext = context.queryContext
    def featureContext = context.feature

    init_binding(_binding)
    override def create_Eval_Context(x: SExpr) = Context(
      this.apply,
      Evaluator.this,
      context,
      universe,
      Some(x),
      IncidentSequence.empty,
      None
    )

    override protected def eval_Atom(p: SAtom): Option[SExpr] = universe.getBinding(p.name)
  }

  def evaluator(u: Universe) = new org.goldenport.sexpr.eval.LispEvaluator[Context] {
    def config = context.config
    def dateTimeContext = context.dateTimeContext
    def i18nContext = u.getI18NContext getOrElse config.i18nContext
    def queryContext = context.queryContext
    def featureContext = context.feature

    init_binding(new Evaluator.Binding(u))

    override def create_Eval_Context(x: SExpr) = Context(
      this.apply,
      Evaluator.this,
      context,
      u,
      Some(x),
      IncidentSequence.empty,
      None
    )

    override protected def resolve_Aliase(name: String): Option[SExpr] =
      resolve_aliase(name)

    override protected def eval_Atom(p: SAtom): Option[SExpr] = {
      val name = p.name
      name match { // This logic for aliases are virtually unused. See normalize.
        case "#" => Some(u.takeHistory)
        case "?" => Some(u.peek)
        case "!" => Some(u.takeCommandHistory)
        case _ => _mark_number(name).map {
          case ("#", n) => Some(u.takeHistory(n))
          case ("?", n) => Some(u.peek(n))
          case ("!", n) => Some(u.takeCommandHistory(n))
        }.getOrElse(
          u.getBinding(p.name)
        )
      }
    }

    override protected def apply_Lambda(c: LispContext, l: SLambda, args: List[SExpr]): LispContext =
      c match {
        case m: Context => _apply_lambda(m, l, args)
        case _ => RAISE.noReachDefect
      }

    private def _apply_lambda(c: Context, l: SLambda, args: List[SExpr]): LispContext = {
      val engine = c.executionContext.engine getOrElse RAISE.noReachDefect
      val script = Script(l.expressions)
      val universe = c.universe.next(l.parameters, args, c.traceContext.toHandle)
      val r = engine.run(c.executionContext, universe, script)
      val value = r._2.toList match {
        case Nil => SNil
        case x :: Nil => SExpr.create(x.asSExpr)
        case xs => SList.create(xs.map(x => SExpr.create(x.asSExpr)))
      }
      val incident = _distill_incident(r._3)
      val bindings = c.bindings
      c.toResult(value, bindings, incident)
    }

    private def _distill_incident(p: Universe): IncidentSequence = p.takeIncidentSequence

    override protected def resolve_Parameters(c: LispContext, l: SLambda, args: List[SExpr]): (LispContext, List[SExpr]) = {
      val (u, args9) = normalize_parameters_lambda(SList.create(args), l.parameters)
      args9 match {
        case m: SList => (lift_Context(c).withUniverse(u), m.list)
        case _ => RAISE.noReachDefect
      }
    }
  }

  def evalScript(p: SExpr): Universe = {
    context.traceContext.execute("Script", p) {
      val sexpr = _normalize(p)
      val r = if (true)
        eval(sexpr)
      else
        evalLazy(sexpr)
      context.traceContext.result(r, r.getValue.getOrElse("VOID"))
    }
  }

  private def _normalize(p: SExpr): SExpr = context.promotion(p).
    map(x => SList(SAtom(x), p)).
    getOrElse(p)

  // private def _show(ps: Vector[Expression]): String = ps match {
  //   case Vector() => ""
  //   case Vector(x) => x.show
  //   case xs: Vector[Expression] => xs.map(_.embed).mkString("[", " ", "]")
  // }

  // private def _show(p: Expression) = p.show

  def eval(p: SExpr): Universe = {
    val (u, s) = normalize(p)
    val c = evaluator(u).apply(s)
    val r = c.pushOrMute(p).universe
    log_trace(s"EVAL: ${p.show} => ${c.universe.show} => ${r.show}")
    r
  }

  def evalLazy(p: SExpr): Universe = {
    val (u, s) = normalize(p)
    val c = evaluator(u).applyLazy(s)
    val r = c.push(p).universe
    log_trace(s"EVALLAZY: ${p.show} => ${c.universe.show} => ${r.show}")
    r
  }

  def normalize(p: SExpr): (Universe, SExpr) = _normalize(universe, p)

  private def _normalize(pu: Universe, p: SExpr): (Universe, SExpr) = p match {
    case m: SAtom => _binding.getFunction(m.name).map { f =>
      val n = f.specification.numberOfRequiredArguments
      // SList.create(m :: List.tabulate(n)(_ => SList(SAtom("pop"))))
      pu.makeStackParameters(n) match {
        case \/-((u, params)) => (u, SList.create(m :: params))
        case -\/(e) => (pu, e)
      }
    }.getOrElse {
      val expr = resolve_aliase(m.name) getOrElse SList(SAtom("eval-or-invoke"), m)
      (pu, expr)
    }
    case m: SCell => m.car match {
      case SAtom(name) => name match {
        case ":" => _no_stack(m.cdr)
        case _ => _normalize(m, name)
      }
      case mm => _normalize(context.promotion(mm), m)
    }
    case m => (pu, m)
  }

  protected final def resolve_aliase(name: String): Option[SExpr] =
    name match {
      case "#" => Some(SList(SAtom("history")))
      case "?" => Some(SList(SAtom("peek")))
      case "??" => Some(SList(SAtom("pop")))
      case "!" => Some(SList(SAtom("command")))
      case _ =>
        _mark_number(name).map {
          case ("#", n) => SList(SAtom("history"), SNumber(n))
          case ("?", n) => SList(SAtom("peek"), SNumber(n))
          case ("??", n) => SList(SAtom("pop"), SNumber(n))
          case ("!", n) => SList(SAtom("command"), SNumber(n))
        }
    }

  private def _mark_number(p: String): Option[(String, Int)] =
    p.headOption.flatMap(c => 
      if (c == '#' || c == '?' || c == '!')
        StringUtils.intOption(p.tail).map(x => (s"$c", x))
      else
        None
    )

  private def _no_stack(p: SExpr): (Universe, SExpr) = {
    val verb = "mute"
    _normalize(SList(SAtom(verb), p), verb)
  }

  private def _normalize(name: Option[String], p: SCell): (Universe, SExpr) =
    name.map(x => _normalize(SCell(SAtom(x), p), x)).getOrElse((universe, p))

  private def _normalize_BAK(m: SList, name: String): (Universe, SExpr) =
    _binding.getFunction(name).map { f =>
      val params = Parameters(m)
      val n = f.specification.numberOfRequiredArguments
      val nn = n - (params.arguments.length - 1)
      if (nn > 0) {
        // m.append(List.tabulate(nn)(_ => SList(SAtom("pop"))))
        universe.makeStackParameters(nn) match {
          case \/-((u, params)) => (u, SList.create(m.list ::: params))
          case -\/(e) => (universe, e)
        }
      } else {
        (universe, m)
      }
    }.getOrElse((universe, m))

  private def _normalize(m: SList, name: String): (Universe, SExpr) =
    _get_specification(name). //    get_specification(name).
      map(spec => normalize_parameters_function(m, spec)).
      getOrElse((universe, m))

  // see KaleidoxFunction
  // see org.goldenport.sexpr.eval.LispEvaluator.getSpecification
  // see org.goldenport.sexpr.eval.Evaluator.get_specification
  private def _get_specification(name: String) = _binding.getFunction(name).map(_.specification)

  protected final def normalize_parameters_function(m: SList, spec: FunctionSpecification): (Universe, SExpr) = {
    val n = spec.numberOfRequiredArguments
    val a = Parameters.fromExpression(m)
    // val params = spec.resolve(a) ; conflict variable handling.
    val params = a
    val resolved = m.list(0) :: params.restore
    val nn = n - params.arguments.length
    if (nn > 0) {
      universe.makeStackParameters(nn) match {
        case \/-((u, xs)) => (u, SList.create(resolved ::: xs))
        case -\/(e) => (universe, e)
      }
    } else {
      (universe, SList.create(resolved))
    }
  }

  protected final def normalize_parameters_lambda(m: SList, paramnames: List[String]): (Universe, SExpr) = {
    val params = Parameters(m)
    val n = paramnames.length
    val nn = n - params.arguments.length
    if (nn > 0) {
      case class Slot(name: String, binding: Option[SExpr]) {
        def isBinded = binding.isDefined
      }
      case class Z(slots: Vector[Slot] = Vector.empty) {
        def r = {
          val nunbinds = slots.filterNot(_.isBinded).length
          universe.makeStackParameters(nunbinds) match {
            case \/-((u, xs)) => (u, SList.create(m.list ::: _build(xs)))
            case -\/(e) => (universe, e)
          }
        }

        private def _build(ps: List[SExpr]): List[SExpr] = {
          case class ZZ(xs: List[SExpr] = ps, rs: Vector[SExpr] = Vector.empty) {
            def r = rs.toList

            def +(rhs: Slot) = {
              rhs.binding match {
                case Some(s) => copy(rs = rs :+ s)
                case None => copy(xs = xs.tail, rs = rs :+ xs.head)
              }
            }
          }
          slots./:(ZZ())(_+_).r
        }

        def +(rhs: String) =
          if (_is_global_parameter(rhs)) {
            _get_binding(rhs) match {
              case Some(s) => s match {
                case SNil => _add(Slot(rhs, None))
                case m: SExpr => _add(Slot(rhs, Some(m)))
                case m => _add(Slot(rhs, Some(SExpr.create(m))))
              }
              case None => _add(Slot(rhs, None))
            }
          } else {
            _add(Slot(rhs, None))
          }

        // FUTURE : Global parameter convension (e.g. First letter is upper case)
        private def _is_global_parameter(key: String) = true

        // FUTURE : control binding scope (e.g. eliminates setup.bindings, config.bindings)
        private def _get_binding(key: String): Option[SExpr] = 
          universe.getBinding(key)

        private def _add(p: Slot) = copy(slots = slots :+ p)
      }
      paramnames.takeRight(nn)./:(Z())(_+_).r

      // universe.makeStackParameters(nn) match {
      //   case \/-((u, xs)) => (u, SList.create(m.list ::: xs))
      //   case -\/(e) => (universe, e)
      // }
    } else {
      (universe, m)
    }
  }

  def functionParser = _evaluator.functionParser

  def applyScript(p: String): Universe = {
    val s = Script.parse(context.config, p)
    case class Z(zu: Universe) {
      def r = zu

      def +(rhs: SExpr) = {
        val (u, s) = _normalize(zu, rhs)
        val c = evaluator(u).apply(s)
        val r = c.pushOrMute(rhs).universe
        copy(zu = r)
      }
    }
    s.listSExpr./:(Z(universe))(_+_).r
  }
}

object Evaluator {
  class Binding(val universe: Universe) extends LispBinding[Context] {
    override protected def get_Atom(name: String): Option[SExpr] =
      universe.bindings.get(name).map(SExpr.create)

    override protected def extension_Functions = KaleidoxFunction.functions

    override protected def get_Function(p: Context): Option[LispFunction] =
      p.value match {
        case m: SCell => p.args match {
          case Nil => None
          case x :: xs => x match {
            case SAtom(name) => universe.service.getFunction(name)
            case SExpression(expr) => universe.service.getFunction(expr)
            case _ => None
          }
        }
        case _ => None
      }
  }
}
