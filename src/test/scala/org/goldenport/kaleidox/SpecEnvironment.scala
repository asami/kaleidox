package org.goldenport.kaleidox

import org.goldenport.log.LogLevel
import org.goldenport.cli.Environment
import org.goldenport.kaleidox.interpreter.Interpreter
import org.goldenport.record.unitofwork.interpreter.MockUnitOfWorkLogic

/*
 * @since   Jan.  2, 2019
 * @version Oct. 14, 2024
 * @author  ASAMI, Tomoharu
 */
trait SpecEnvironment {
  def logLevel: LogLevel = LogLevel.Debug

  val mocklogic = MockUnitOfWorkLogic.build(
    "http://www.yahoo.com" -> "OK"
  )
  val envJaJp = Environment.createJaJp()
  val configJaJp = {
    val a = Config.create(envJaJp)
    a.withServiceLogic(mocklogic)
      .withLogLevel(logLevel) // trace // warn // debug
  }
  val envEnUs = Environment.createEnUs()
  val configEnUs = {
    val a = Config.create(envEnUs)
    a.withServiceLogic(mocklogic)
      .withLogLevel(logLevel) // trace // warn // debug
  }
  val envEnGb = Environment.createEnGb()
  val configEnGb = {
    val a = Config.create(envEnGb)
    a.withServiceLogic(mocklogic)
      .withLogLevel(logLevel) // trace // warn // debug
  }
  val envDeDe = Environment.createDeDe()
  val configDeDe = {
    val a = Config.create(envDeDe)
    a.withServiceLogic(mocklogic)
      .withLogLevel(logLevel) // trace // warn // debug
  }
  val envDeCh = Environment.createDeCh()
  val configDeCh = {
    val a = Config.create(envDeCh)
    a.withServiceLogic(mocklogic)
      .withLogLevel(logLevel) // trace // warn // debug
  }

  val context = ExecutionContext(envJaJp, configJaJp)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)
  engine.initialize()

  def parse(p: String): Script = parseJaJP(p)
  def parseJaJP(p: String): Script = Script.parse(configJaJp, p)
  def parseEnUs(p: String): Script = Script.parse(configEnUs, p)
  def parseEnGb(p: String): Script = Script.parse(configEnGb, p)
  def parseDeDe(p: String): Script = Script.parse(configDeDe, p)
  def parseDeCh(p: String): Script = Script.parse(configDeCh, p)
  def parseWithoutMetaCommand(p: String): Script = Script.parseWithoutMetaCommand(configJaJp, p)
}
