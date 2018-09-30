package org.goldenport.kaleidox.lisp

import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispBinding
import org.goldenport.kaleidox._

/*
 * @since   Aug. 19, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class Evaluator(
  context: ExecutionContext,
  universe: Universe
) {
  import Evaluator._
  private val _binding = Evaluator.Binding()
  private val _evaluator = new org.goldenport.sexpr.eval.LispEvaluator[Context] {
    def config = context.config
    init_binding(_binding)
    override def create_Eval_Context(x: SExpr) = Context(
      this.apply,
      context,
      universe,
      Some(x),
      None
    )
  }

  def eval(p: SExpr): Universe = {
    val s = _normalize(p)
    val c = _evaluator.applyLazy(s)
    val r = c.push.universe
    println(s"eval: ${p.show} => ${c.universe.show} => ${r.show}")
    r
  }

  private def _normalize(p: SExpr): SExpr = p match {
    case m: SUrl => SList(SAtom("fetch"), m)
    case m: SAtom => _binding.getFunction(m.name).map { f =>
      val n = f.specification.numberOfMeaningfulParameters
      SList.create(m :: List.tabulate(n)(_ => SList(SAtom("pop"))))
    }.getOrElse(m)
    case m: SCell => m.car match {
      case mm: SAtom => _binding.getFunction(mm.name).map { f =>
        val n = f.specification.numberOfMeaningfulParameters
        val nn = n - (m.length - 1)
        if (nn > 0)
          m.append(List.tabulate(nn)(_ => SList(SAtom("pop"))))
        else
          m
      }.getOrElse(m)
      case _ => m
    }
    case m => m
  }
}

object Evaluator {
  case class Binding() extends LispBinding[Context] {
  }
}
