package org.goldenport.kaleidox.model.entity

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.context.Consequence
import org.goldenport.i18n.I18NContext
import org.goldenport.record.store.{Id => StoreId, Collection, Query}
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.RecordSequence
import org.goldenport.record.v3.Table.HeaderStrategy
import org.goldenport.sm._
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.sexpr.eval.entity._
import org.goldenport.kaleidox.model.EntityModel

/*
 * @since   Sep. 20, 2021
 *  version Sep. 24, 2021
 *  version Oct. 31, 2021
 *  version Nov. 28, 2021
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
class KaleidoxEntityCollection(
  val factory: KaleidoxEntityFactory,
  val name: Symbol,
  val kaleidoxEntityClass: EntityModel.EntityClass,
  val storeCollection: Collection
) extends EntityCollection {
  def i18nContext: I18NContext = factory.i18nContext
  def entityClass = kaleidoxEntityClass

  def get(id: StoreId): SExpr = SExpr.runOrNotFound(id.string) {
    for {
      a <- storeCollection.get(id).traverse(kaleidoxEntityClass.reconstitute)
    } yield a.map(SEntity)
  }

  def query(q: Query): SExpr = SExpr.run {
    for {
      a <- storeCollection.select(q).irecords.traverse(kaleidoxEntityClass.reconstitute)
    } yield {
      val xs = a.map(SEntity)
      SVector.create(xs)
    }
  }

  def select(pq: Query, header: Option[HeaderStrategy]): SExpr = SExpr.run {
    val q = kaleidoxEntityClass.persistentQuery(pq)
    for {
      a <- storeCollection.select(q).irecords.traverse(kaleidoxEntityClass.unmarshallProperties)
      b <- Consequence.success(RecordSequence(a))
    } yield {
      val t = header.map(x => b.toTable(i18nContext, x)).getOrElse(b.toTable)
      STable(t)
    }
  }

  def create(rec: IRecord)(implicit ctx: LispContext): SExpr = SExpr.run {
    for {
      entity <- kaleidoxEntityClass.create(rec)
    } yield {
      storeCollection.insert(entity.persistentRecord)
      SEntity(entity)
    }
  }

  def update(id: StoreId, rec: IRecord)(implicit ctx: LispContext): SExpr = SExpr.execute {
    storeCollection.update(id, rec)
    SBoolean.TRUE
  }

  def delete(id: StoreId)(implicit ctx: LispContext): SExpr = RAISE.notImplementedYetDefect

  def constitute(): SExpr = {
    storeCollection.create()
    SBoolean.TRUE
  }

  def destroy(): SExpr = RAISE.notImplementedYetDefect
}
