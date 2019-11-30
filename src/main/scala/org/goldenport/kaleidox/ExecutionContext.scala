package org.goldenport.kaleidox

import javax.script._
import org.goldenport.RAISE
import org.goldenport.config.ConfigHelper
import org.goldenport.i18n.I18NContext
import org.goldenport.log.LogConfig
import org.goldenport.io.ResourceManager
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{ScriptEngineContext, FeatureContext}

/*
 * @since   Aug. 11, 2018
 *  version Sep. 24, 2018
 *  version Oct. 10, 2018
 *  version Feb. 25, 2019
 *  version Mar. 24, 2019
 *  version May. 16, 2019
 *  version Jun. 24, 2019
 *  version Jul. 25, 2019
 *  version Aug. 17, 2019
 *  version Sep.  1, 2019
 *  version Oct. 27, 2019
 * @version Nov.  9, 2019
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  config: Config,
  i18nContext: I18NContext,
  logConfig: LogConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  scriptContext: ScriptEngineContext,
  sqlContext: SqlContext,
  resourceManager: ResourceManager,
  feature: FeatureContext,
  engine: Option[Engine] = None
) extends ConfigHelper {
  def newline = config.newline
  def queryContext = sqlContext.queryContext

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
    p.i18nContext,
    p.logConfig,
    p.serviceLogic,
    p.storeLogic,
    p.scriptContext,
    p.sqlContext,
    p.resourceManager,
    p.feature
  )
}
