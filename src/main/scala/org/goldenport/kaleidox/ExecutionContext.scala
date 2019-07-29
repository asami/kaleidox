package org.goldenport.kaleidox

import javax.script._
import org.goldenport.RAISE
import org.goldenport.config.ConfigHelper
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.FeatureContext

/*
 * @since   Aug. 11, 2018
 *  version Sep. 24, 2018
 *  version Oct. 10, 2018
 *  version Feb. 25, 2019
 *  version Mar. 24, 2019
 *  version May. 16, 2019
 *  version Jun. 24, 2019
 * @version Jul. 25, 2019
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  config: Config,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  scriptContext: ScriptEngineManager,
  sqlContext: SqlContext,
  feature: FeatureContext,
  engine: Option[Engine] = None
) extends ConfigHelper {
  def newline = config.newline

  def withEngine(p: Engine) = copy(engine = Some(p))

  // FUTURE customizable
  private def _is_eager_evaluation = true

  def promotion(p: SExpr): Option[String] =
    if (_is_eager_evaluation) {
      Option(p) collect {
        case m: SUrl => m.getSuffix.map {
          case "xsl" => "xslt"
          case _ => "fetch"
        }.getOrElse("fetch")
        case m: SXPath => "path-get"
        case m: SXsl => "xslt"
      }
    } else {
      None
    }

  def run(u: Universe, p: Script) =
    engine.map(_.run(u, p)).getOrElse(RAISE.noReachDefect)
}

object ExecutionContext {
  def apply(p: Config): ExecutionContext = ExecutionContext(
    p,
    p.serviceLogic,
    p.storeLogic,
    p.scriptContext,
    p.sqlContext,
    p.feature
  )
}
