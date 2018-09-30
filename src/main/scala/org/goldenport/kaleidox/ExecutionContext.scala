package org.goldenport.kaleidox

import javax.script._
import org.goldenport.record.unitofwork.interpreter.{UnitOfWorkLogic, StoreOperationLogic}

/*
 * @since   Aug. 11, 2018
 * @version Sep. 24, 2018
 * @author  ASAMI, Tomoharu
 */
case class ExecutionContext(
  config: Config,
  serviceLogic: UnitOfWorkLogic,
  storeLogic: StoreOperationLogic,
  scriptContext: ScriptEngineManager
) {
}

object ExecutionContext {
  def apply(p: Config): ExecutionContext = ExecutionContext(
    p,
    p.serviceLogic,
    p.storeLogic,
    p.scriptContext
  )
}
