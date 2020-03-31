package org.goldenport.kaleidox

import javax.script._
import java.io.File
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.io.ResourceManager
import org.goldenport.sexpr.eval.{LispConfig, ScriptEngineContext, FeatureContext}
import org.goldenport.log.LogLevel
import org.goldenport.matrix.INumericalOperations
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.query.QueryExpression
import org.goldenport.util.DateTimeUtils

/*
 * @since   Aug. 20, 2018
 *  version Sep. 24, 2018
 *  version Oct. 10, 2018
 *  version Feb.  7, 2019
 *  version Mar. 24, 2019
 *  version Apr.  6, 2019
 *  version May. 19, 2019
 *  version Jul. 14, 2019
 *  version Aug. 17, 2019
 *  version Sep. 23, 2019
 *  version Oct. 31, 2019
 *  version Nov.  9, 2019
 *  version Feb. 26, 2020
 * @version Mar. 30, 2020
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  isLocation: Boolean = true
) extends LispConfig {
  lazy val scriptContext = ScriptEngineContext.default
  lazy val sqlContext =
    if (true) // TODO configurable
      SqlContext.createEachTime(properties, createQueryContext())
    else if (false)
      SqlContext.createAutoCommit(properties, createQueryContext())
    else
      SqlContext.createConnectionPool(properties, createQueryContext())
  lazy val resourceManager = new ResourceManager()
  lazy val feature = FeatureContext.create(properties, cliConfig.i18n, sqlContext)
  def properties = cliConfig.properties
  def i18nContext = cliConfig.i18n
  def createQueryContext() = QueryExpression.Context(
    DateTimeUtils.toDateTime(System.currentTimeMillis, i18nContext.datetimezone),
    i18nContext.datetimezone
  )
  def logConfig = cliConfig.log
  def charset = cliConfig.charset
  def newline = cliConfig.newline
  def homeDirectory = cliConfig.homeDirectory
  def getProjectDirectory = cliConfig.projectDirectory
  def workDirectory = cliConfig.workDirectory
  def logLevel: LogLevel = logConfig.level getOrElse LogLevel.Info
  def numericalOperations: INumericalOperations = cliConfig.numericalOperations
  def withLogLevel(p: LogLevel) = copy(cliConfig.withLogLevel(p))
  def withServiceLogic(p: UnitOfWorkLogic) = copy(serviceLogic = p)
  def withStoreLogic(p: StoreOperationLogic) = copy(storeLogic = p)
  def withoutLocation = copy(isLocation = false)
}

object Config {
  val defaultServiceLogic = new StandardUnitOfWorkLogic()
  val defaultStoreLogic = new StandardStoreOperationLogic()
  val default = Config(CliConfig.c, defaultServiceLogic, defaultStoreLogic)
  val noLocation = default.withoutLocation

  def create(env: Environment): Config = {
    Config(
      env.config,
      defaultServiceLogic,
      defaultStoreLogic
    )
  }

  object log {
    val error = default.withLogLevel(LogLevel.Error)
    val warn = default.withLogLevel(LogLevel.Warn)
    val info = default.withLogLevel(LogLevel.Info)
    val debug = default.withLogLevel(LogLevel.Debug)
    val trace = default.withLogLevel(LogLevel.Trace)
  }
}
