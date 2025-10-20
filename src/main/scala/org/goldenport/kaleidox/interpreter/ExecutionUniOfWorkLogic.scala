package org.goldenport.kaleidox.interpreter

import scalaz.concurrent.Task
import org.goldenport.cli.{Environment, Config => CliConfig}
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SExpr
import org.goldenport.kaleidox._

/*
 * @since   Sep.  9, 2018
 *  version Sep. 24, 2018
 * @version Sep. 12, 2025
 * @author  ASAMI, Tomoharu
 */
class ExecutionUnitOfWorkLogic(
  executionContext: UnitOfWorkLogic.ExecutionContext
) extends StandardUnitOfWorkLogic(executionContext) {
}

object ExecutionUnitOfWorkLogic {
  def create(env: Environment): ExecutionUnitOfWorkLogic = {
    val ec = new UnitOfWorkLogic.ExecutionContext(
      env.contextFoundation,
      env.recorder
    )
    new ExecutionUnitOfWorkLogic(ec)
  }
}
