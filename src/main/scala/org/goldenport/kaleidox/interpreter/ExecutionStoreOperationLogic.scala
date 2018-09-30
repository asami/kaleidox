package org.goldenport.kaleidox.interpreter

import scalaz.concurrent.Task
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v2.unitofwork.interpreter._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SExpr
import org.goldenport.kaleidox._

/*
 * @since   Sep.  9, 2018
 * @version Sep. 24, 2018
 * @author  ASAMI, Tomoharu
 */
class ExecutionStoreOperationLogic() extends JournalStoreOperationLogic {
  protected def generate_id(store: Store): Store.Id = ???
  protected def get_item_store(store: Store): Option[JournalStoreOperationLogic.ItemStore] = ???

  def get(store: org.goldenport.record.unitofwork.Store,id: org.goldenport.record.unitofwork.Store.Id): org.goldenport.record.unitofwork.GetResult = ???
  def getExclusive(store: org.goldenport.record.unitofwork.Store,id: org.goldenport.record.unitofwork.Store.Id): org.goldenport.record.unitofwork.GetResult = ???
  def getShare(store: org.goldenport.record.unitofwork.Store,id: org.goldenport.record.unitofwork.Store.Id): org.goldenport.record.unitofwork.GetResult = ???
  def gets(store: org.goldenport.record.unitofwork.Store,ids: Seq[org.goldenport.record.unitofwork.Store.Id]): org.goldenport.record.unitofwork.GetsResult = ???
  def getsExclusive(store: org.goldenport.record.unitofwork.Store,ids: Seq[org.goldenport.record.unitofwork.Store.Id]): org.goldenport.record.unitofwork.GetsResult = ???
  def getsShare(store: org.goldenport.record.unitofwork.Store,ids: Seq[org.goldenport.record.unitofwork.Store.Id]): org.goldenport.record.unitofwork.GetsResult = ???
  def select(store: org.goldenport.record.unitofwork.Store,query: org.goldenport.record.unitofwork.Query): org.goldenport.record.unitofwork.SelectResult = ???
  def selectExclusive(store: org.goldenport.record.unitofwork.Store,query: org.goldenport.record.unitofwork.Query): org.goldenport.record.unitofwork.SelectResult = ???
  def selectShare(store: org.goldenport.record.unitofwork.Store,query: org.goldenport.record.unitofwork.Query): org.goldenport.record.unitofwork.SelectResult = ???
}
