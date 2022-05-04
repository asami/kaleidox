package org.goldenport.kaleidox

import javax.script._
import java.io.File
import org.goldenport.hocon.RichConfig
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.io.ResourceManager
import org.goldenport.sexpr.eval.{LispConfig, ScriptEngineContext, FeatureContext}
import org.goldenport.sexpr.script.Script.{Config => SConfig}
import org.goldenport.log.LogLevel
import org.goldenport.matrix.INumericalOperations
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v3.sql.SqlContext
import org.goldenport.record.store.StoreFactory
import org.goldenport.record.query.QueryExpression
import org.goldenport.statemachine.StateMachineSpace
import org.goldenport.util.DateTimeUtils
import org.goldenport.kaleidox.model.entity.KaleidoxEntityFactory
import org.goldenport.kaleidox.extension.ExtensionContext
import org.goldenport.kaleidox.extension.modeler.Modeler

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
 *  version Mar. 30, 2020
 *  version Feb. 20, 2021
 *  version Sep. 24, 2021
 *  version Oct.  4, 2021
 *  version Dec. 19, 2021
 * @version Apr.  4, 2022
 * @author  ASAMI, Tomoharu
 */
case class Config(
  cliConfig: CliConfig,
  scriptConfig: SConfig,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  isLocation: Boolean = true,
  variations: Config.Variations = Config.Variations.default,
  extension: ExtensionContext = ExtensionContext.default
) extends LispConfig {
  import Config._
  lazy val scriptContext = ScriptEngineContext.default
  lazy val queryContext = createQueryContext()
  lazy val sqlContext =
    if (_is_memory(properties))
      SqlContext.createMemory(properties, queryContext)
    else if (true) // TODO configurable
      SqlContext.createEachTime(properties, queryContext)
    else if (false)
      SqlContext.createAutoCommit(properties, queryContext)
    else
      SqlContext.createConnectionPool(properties, queryContext)
  lazy val resourceManager = new ResourceManager()
  lazy val stateMachineSpace = StateMachineSpace.create()
  lazy val storeFactory = new StoreFactory(cliConfig.makeConfig("store"), sqlContext)
  lazy val entityFactory = new KaleidoxEntityFactory(i18nContext, storeFactory, stateMachineSpace)
  lazy val feature = FeatureContext.create(properties, cliConfig.i18n, sqlContext, entityFactory)

  def properties = cliConfig.properties
  def dateTimeContext = cliConfig.dateTimeContext
  def i18nContext = cliConfig.i18n
  def createQueryContext() = QueryExpression.Context(
    DateTimeUtils.toDateTime(System.currentTimeMillis, i18nContext.datetimezone)
  )
  def logConfig = cliConfig.log
  def charset = cliConfig.charset
  def newline = cliConfig.newline
  def homeDirectory = cliConfig.homeDirectory
  def getProjectDirectory = cliConfig.projectDirectory
  def workDirectory = cliConfig.workDirectory
  def logLevel: LogLevel = logConfig.level getOrElse LogLevel.Info
  def numericalOperations: INumericalOperations = cliConfig.numericalOperations
  def prompt = cliConfig.properties.
    getStringOption(PROP_PROMPT).getOrElse(variations.prompt)
  def withLogLevel(p: LogLevel) = copy(cliConfig.withLogLevel(p))
  def withServiceLogic(p: UnitOfWorkLogic) = copy(serviceLogic = p)
  def withStoreLogic(p: StoreOperationLogic) = copy(storeLogic = p)
  def withoutLocation = copy(isLocation = false)

  def setPrompt(p: String) = copy(variations = variations.setPrompt(p))
  def setModeler(p: Modeler) = copy(extension = extension.setModeler(p))

  private def _is_memory(p: RichConfig): Boolean = p.getConfigOption("db").isEmpty
}

object Config {
  import Script._

  final val PROP_PROMPT = "prompt"

  case class Variations(
    prompt: String = "kaleidox> "
  ) {
    def setPrompt(p: String) = copy(prompt = p)
  }
  object Variations {
    val default = Variations()
  }

  val defaultServiceLogic = new StandardUnitOfWorkLogic()
  val defaultStoreLogic = new StandardStoreOperationLogic()
  val scriptConfig = SConfig.default.addStringLiteralTokenizers(
    DoxLiteralTokenizer
  )
  val default = Config(CliConfig.c, scriptConfig, defaultServiceLogic, defaultStoreLogic)
  val noLocation = default.withoutLocation

  def create(env: Environment): Config = {
    Config(
      env.config,
      scriptConfig,
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
