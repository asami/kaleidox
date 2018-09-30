package org.goldenport.kaleidox

import javax.script._
import ch.qos.logback.classic.Level
import org.goldenport.sexpr.eval.LispConfig
import org.goldenport.util.HoconUtils.RichConfig
import org.goldenport.record.unitofwork.interpreter._

/*
 * @since   Aug. 20, 2018
 * @version Sep. 24, 2018
 * @author  ASAMI, Tomoharu
 */
case class Config(
  hocon: RichConfig,
  logLevel: Option[Level],
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic
) extends LispConfig {
  def withLogLevel(p: Level) = copy(logLevel = Some(p))
  def withServiceLogic(p: UnitOfWorkLogic) = copy(serviceLogic = p)
  def withStoreLogic(p: StoreOperationLogic) = copy(storeLogic = p)
  lazy val scriptContext = new ScriptEngineManager()
}

object Config {
  val defaultServiceLogic = new StandardUnitOfWorkLogic()
  val defaultStoreLogic = new StandardStoreOperationLogic()
  val hocon = org.goldenport.config.Config.loadHocon() // file
  val default = Config(hocon, None, defaultServiceLogic, defaultStoreLogic)
  object log {
    val error = default.withLogLevel(Level.ERROR)
    val warn = default.withLogLevel(Level.WARN)
    val info = default.withLogLevel(Level.INFO)
    val debug = default.withLogLevel(Level.DEBUG)
    val trace = default.withLogLevel(Level.TRACE)
  }
}
