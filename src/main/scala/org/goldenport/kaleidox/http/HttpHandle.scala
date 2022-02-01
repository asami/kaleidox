package org.goldenport.kaleidox.http

import org.goldenport.RAISE
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.sexpr._
import org.goldenport.kaleidox._

/*
 * @since   Mar. 19, 2021
 *  version Mar. 28, 2021
 *  version Apr. 22, 2021
 * @version Jan. 24, 2022
 * @author  ASAMI, Tomoharu
 */
class HttpHandle(engine: Engine) {
  val universe = engine.universe
  val context = engine.context
  val config = context.config

  def execute(req: HttpRequest, pres: HttpResponse, contextpath: String): HttpResponse = {
    ???
  }

  def execute(req: HttpRequest, pres: HttpResponse): HttpResponse = {
    val funcname = req.pathname.components.mkString(".")
    val model: Model = Model.httpCall(config, funcname, req.query, req.form)
    val params = HttpParameters.create(req)
    val (report, r, newuniverse) = engine.run(universe, model)
    r match {
      case Seq() => _resp(params)
      case Seq(x) => _resp(params, report, x)
      case m: Seq[Expression] => _resp(params, report, m)
    }
  }

  private def _resp(params: HttpParameters) = HttpResponse.ok

  private def _resp(params: HttpParameters, report: EvalReport, p: Expression) = {
    p.asSExpr match {
      case m: SError => HttpResponse.conclusion(context, params, report, m.conclusion)
//      case m => HttpResponse.json(Record.data("data" -> m.print).toJsonString)
      case m => HttpResponse.data(context, params, report, m)
    }
  }

  private def _resp(params: HttpParameters, report: EvalReport, ps: Seq[Expression]) = {
    RAISE.unsupportedOperationFault
  }
}
