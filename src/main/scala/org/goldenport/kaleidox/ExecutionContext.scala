package org.goldenport.kaleidox

import javax.script._
import org.goldenport.config.ConfigHelper
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}

/*
 * @since   Aug. 11, 2018
 *  version Sep. 24, 2018
 *  version Oct. 10, 2018
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  config: Config,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  scriptContext: ScriptEngineManager
) extends ConfigHelper {
  def newline = config.newline
}

object ExecutionContext {
  def apply(p: Config): ExecutionContext = ExecutionContext(
    p,
    p.serviceLogic,
    p.storeLogic,
    p.scriptContext
  )
}
