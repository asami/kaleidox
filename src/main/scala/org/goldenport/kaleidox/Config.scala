package org.goldenport.kaleidox

import javax.script._
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.sexpr.eval.LispConfig
import org.goldenport.log.LogLevel
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.util.HoconUtils.RichConfig

/*
 * @since   Aug. 20, 2018
 *  version Sep. 24, 2018
 *  version Oct. 10, 2018
 * @version Feb.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  isLocation: Boolean = true
) extends LispConfig {
  lazy val scriptContext = new ScriptEngineManager()
  def properties = cliConfig.properties
  def newline = cliConfig.newline
  def logLevel: LogLevel = cliConfig.logLevel
  def withLogLevel(p: LogLevel) = copy(cliConfig.withLogLevel(p))
  def withServiceLogic(p: UnitOfWorkLogic) = copy(serviceLogic = p)
  def withStoreLogic(p: StoreOperationLogic) = copy(storeLogic = p)
  def withoutLocation = copy(isLocation = false)
}

object Config {
  val defaultServiceLogic = new StandardUnitOfWorkLogic()
  val defaultStoreLogic = new StandardStoreOperationLogic()
  val default = Config(CliConfig.default, defaultServiceLogic, defaultStoreLogic)
  val noLocation = default.withoutLocation

  def create(env: Environment): Config =
    Config(env.config, defaultServiceLogic, defaultStoreLogic)

  object log {
    val error = default.withLogLevel(LogLevel.Error)
    val warn = default.withLogLevel(LogLevel.Warn)
    val info = default.withLogLevel(LogLevel.Info)
    val debug = default.withLogLevel(LogLevel.Debug)
    val trace = default.withLogLevel(LogLevel.Trace)
  }
}
