package org.goldenport.kaleidox.lisp

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.log.Loggable
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{LispBinding, Parameters, LispContext}
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
 * @version Aug.  4, 2019
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
    def i18nContext = config.i18nContext

    init_binding(_binding)
    override def create_Eval_Context(x: SExpr) = Context(
      this.apply,
      context,
      universe,
      Some(x),
      None,
      None
    )

    override protected def eval_Atom(p: SAtom): Option[SExpr] = universe.getBinding(p.name)
  }

  def evaluator(u: Universe) = new org.goldenport.sexpr.eval.LispEvaluator[Context] {
    def config = context.config
    def i18nContext = u.getI18NContext getOrElse config.i18nContext

    init_binding(new Evaluator.Binding(u))

    override def create_Eval_Context(x: SExpr) = Context(
      this.apply,
      context,
      u,
      Some(x),
      None,
      None
    )

    override protected def eval_Atom(p: SAtom): Option[SExpr] = {
      val name = p.name
      name match {
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
      val universe = c.universe.next(l.parameters, args)
      val r = engine.run(universe, script)
      val value = r._2.toList match {
        case Nil => SNil
        case x :: Nil => SExpr.create(x.asSExpr)
        case xs => SList.create(xs.map(x => SExpr.create(x.asSExpr)))
      }
      val incident = None // TODO
      val bindings = c.bindings
      c.toResult(value, incident, bindings)
    }
  }

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

  def normalize(p: SExpr): (Universe, SExpr) = p match {
    case m: SAtom => _binding.getFunction(m.name).map { f =>
      val n = f.specification.numberOfMeaningfulParameters
      // SList.create(m :: List.tabulate(n)(_ => SList(SAtom("pop"))))
      universe.makeStackParameters(n) match {
        case \/-((u, params)) => (u, SList.create(m :: params))
        case -\/(e) => (universe, e)
      }
    }.getOrElse {
      val name = m.name
      val expr = name match {
        case "#" => SList(SAtom("history"), SNumber(1))
        case "?" => SList(SAtom("peek"), SNumber(1))
        case "!" => SList(SAtom("command"), SNumber(1))
        case _ => 
          _mark_number(m.name).map {
            case ("#", n) => SList(SAtom("history"), SNumber(n))
            case ("?", n) => SList(SAtom("peek"), SNumber(n))
            case ("!", n) => SList(SAtom("command"), SNumber(n))
          }.getOrElse {
            SList(SAtom("eval-or-invoke"), m)
          }
      }
      (universe, expr)
    }
    case m: SCell => m.car match {
      case SAtom(name) => name match {
        case ":" => _no_stack(m.cdr)
        case _ => _normalize(m, name)
      }
      case mm => _normalize(context.promotion(mm), m)
    }
    case m => (universe, m)
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

  private def _normalize(m: SList, name: String): (Universe, SExpr) =
    _binding.getFunction(name).map { f =>
      val params = Parameters(m)
      val n = f.specification.numberOfMeaningfulParameters
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
}

object Evaluator {
  class Binding(val universe: Universe) extends LispBinding[Context] {
    override protected def get_Atom(name: String): Option[SExpr] =
      universe.bindings.get(name).map(SExpr.create)
  }
}
