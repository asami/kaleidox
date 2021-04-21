package org.goldenport.kaleidox.http

import org.goldenport.record.v3.IRecord
import org.goldenport.record.http.Request.Method
import org.goldenport.values.PathName

/*
 * See org.goldenport.record.http.Request, arcadia.context.Request
 * 
 * @since   Mar.  2, 2021
 *  version Mar. 20, 2021
 * @version Apr. 22, 2021
 * @author  ASAMI, Tomoharu
 */
trait HttpRequest {
  def pathname: PathName
  def method: Method
  def queryWhole: IRecord
  private lazy val _query: IRecord = _normalize(queryWhole)
  private lazy val _query_meta: IRecord = _normalize_meta(queryWhole)
  def query = _query
  def queryMeta: IRecord = _query_meta
  def formWhole: IRecord
  private lazy val _form: IRecord = _normalize(formWhole)
  private lazy val _form_meta: IRecord = _normalize_meta(formWhole)
  def form: IRecord = _form
  def formMeta: IRecord = _form_meta

  private def _normalize(p: IRecord) = p.toRecord.removeField(_.name.startsWith(HttpRequest.META_PREFIX))
  private def _normalize_meta(p: IRecord) = p.toRecord.flatMapField(x =>
    if (x.key.name.startsWith(HttpRequest.META_PREFIX))
      List(x.withKey(x.key.name.drop(1)))
    else
      Nil
  )
}

object HttpRequest {
  val META_PREFIX = "_"
}
