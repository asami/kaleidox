package org.goldenport.kaleidox.interpreter

import scalaz.concurrent.Task
import org.goldenport.RAISE
import org.goldenport.record.unitofwork._
import org.goldenport.record.unitofwork.interpreter._
import org.goldenport.record.v2.unitofwork.interpreter._
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SExpr
import org.goldenport.kaleidox._

/*
 * @since   Sep.  9, 2018
 *  version Sep. 24, 2018
 * @version Sep. 14, 2019
 * @author  ASAMI, Tomoharu
 */
class ExecutionStoreOperationLogic() extends JournalStoreOperationLogic {
  protected def generate_id(store: Store): Store.Id = RAISE.notImplementedYetDefect
  protected def get_item_store(store: Store): Option[JournalStoreOperationLogic.ItemStore] = RAISE.notImplementedYetDefect

  def get(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getSync(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getShare(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def getExclusive(store: Store,id: Store.Id): GetResult = RAISE.notImplementedYetDefect
  def gets(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsSync(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsShare(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def getsExclusive(store: Store,ids: Seq[Store.Id]): GetsResult = RAISE.notImplementedYetDefect
  def select(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectSync(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectShare(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
  def selectExclusive(store: Store,query: Query): SelectResult = RAISE.notImplementedYetDefect
}
