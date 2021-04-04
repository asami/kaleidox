package org.goldenport.kaleidox.http

import org.goldenport.RAISE
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SError
import org.goldenport.kaleidox._

/*
 * @since   Mar. 19, 2021
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
class HttpHandle(engine: Engine) {
  val universe = engine.universe
  val context = engine.context
  val config = context.config

  def execute(req: HttpRequest, pres: HttpResponse): HttpResponse = {
    val funcname = req.pathname.components.mkString(".")
    val model: Model = Model.httpCall(config, funcname, req.query, req.form)
    val (report, r, newuniverse) = engine.run(universe, model)
    r match {
      case Seq() => _resp()
      case Seq(x) => _resp(report, x)
      case m: Seq[Expression] => _resp(report, m)
    }
  }

  private def _resp() = HttpResponse.ok

  private def _resp(report: EvalReport, p: Expression) = {
    p.asSExpr match {
      case m: SError => HttpResponse.conclusion(context, report, m.conclusion)
//      case m => HttpResponse.json(Record.data("data" -> m.print).toJsonString)
      case m => HttpResponse.data(context, report, m.print)
    }
  }

  private def _resp(report: EvalReport, ps: Seq[Expression]) = {
    RAISE.unsupportedOperationFault
  }
}
