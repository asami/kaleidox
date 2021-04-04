package org.goldenport.kaleidox.http

import org.goldenport.record.v3.IRecord
import org.goldenport.record.http.Request.Method
import org.goldenport.values.PathName

/*
 * See org.goldenport.record.http.Request, arcadia.context.Request
 * 
 * @since   Mar.  2, 2021
 * @version Mar. 20, 2021
 * @author  ASAMI, Tomoharu
 */
trait HttpRequest {
  def pathname: PathName
  def method: Method
  def query: IRecord
  def form: IRecord
}

object HttpRequest {
}
