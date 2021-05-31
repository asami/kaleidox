package org.goldenport.kaleidox

import javax.script._
import org.goldenport.RAISE
import org.goldenport.monitor.Monitor
import org.goldenport.cli.Environment
import org.goldenport.cli.Environment.EnvironmentExecutionContextBase
import org.goldenport.config.ConfigHelper
import org.goldenport.recorder.ForwardRecorder
import org.goldenport.i18n.I18NContext
import org.goldenport.log.LogConfig
import org.goldenport.trace.TraceContext
import org.goldenport.io.ResourceManager
import org.goldenport.statemachine.StateMachineSpace
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{ScriptEngineContext, FeatureContext}

/*
 * org.goldenport.Config
 * org.goldenport.config.Config
 * org.goldenport.monitor.Monitor
 * org.goldenport.context.ExecutionContextBase
 * org.goldenport.cli.Config [config.Config]
 * org.goldenport.cli.Environment
 * org.goldenport.cli.Environment.EnvironmentExecutionContextBase (monitor.Monitor)
 * org.goldenport.kaleidox.Config (cli.Config)
 * org.goldenport.kaleidox.ExecutionContext [EnvironmentExecutionContextBase]
 * 
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
 *  version Nov.  9, 2019
 *  version Feb. 26, 2020
 *  version May. 30, 2020
 *  version Jan. 23, 2021
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 *  version Apr.  5, 2021
 * @version May. 21, 2021
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  environment: Environment,
  config: Config,
  i18nContext: I18NContext,
  logConfig: LogConfig,
  traceContext: TraceContext,
  statemachineSpace: StateMachineSpace,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  scriptContext: ScriptEngineContext,
  sqlContext: SqlContext,
  resourceManager: ResourceManager,
  feature: FeatureContext,
  engine: Option[Engine] = None
) extends EnvironmentExecutionContextBase { // ConfigHelper with ForwardRecorder {
  def numericalOperations = config.numericalOperations
  def newline = config.newline
  def queryContext = sqlContext.queryContext

  def newContext(p: Engine) = copy(engine = Some(p), traceContext = TraceContext.create())
  def newContextForFuture() = copy(traceContext = TraceContext.create())

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
  trait Logic {
    def context: ExecutionContext
    def locale = context.locale
  }

  def apply(env: Environment, p: Config): ExecutionContext = ExecutionContext(
    env,
    p,
    p.i18nContext,
    p.logConfig,
    TraceContext.create(),
    StateMachineSpace.create(),
    p.serviceLogic,
    p.storeLogic,
    p.scriptContext,
    p.sqlContext,
    p.resourceManager,
    p.feature
  )
}
