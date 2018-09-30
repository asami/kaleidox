package org.goldenport.kaleidox.interpreter

import scalaz.concurrent.Task
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.UnitOfWork._
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SExpr
import org.goldenport.kaleidox._

/*
 * @since   Sep.  9, 2018
 * @version Sep. 24, 2018
 * @author  ASAMI, Tomoharu
 */
class ExecutionUnitOfWorkLogic() extends UnitOfWorkLogic {
  def abort(e: Throwable): Unit = ???
  def abort(message: String): Unit = ???
  def commit(): org.goldenport.record.unitofwork.CommitResult = ???
  def invokeService(req: ServiceRequest): ServiceResponse = ???
}
