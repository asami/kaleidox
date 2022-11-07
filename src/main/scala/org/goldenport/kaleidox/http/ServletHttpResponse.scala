package org.goldenport.kaleidox.http

import java.nio.charset.Charset
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.goldenport.RAISE
import org.goldenport.bag.ChunkBag

/*
 * @since   Mar.  2, 2021
 *  version Mar. 24, 2021
 * @version Oct. 30, 2022
 * @author  ASAMI, Tomoharu
 */
case class ServletHttpResponse(response: HttpServletResponse) extends HttpResponse {
  def status: Int = response.getStatus
  def mimeType: String = RAISE.notImplementedYetDefect
  def charset: Charset = RAISE.notImplementedYetDefect
  def content: ChunkBag = RAISE.notImplementedYetDefect
  def headers: Seq[Header] = RAISE.notImplementedYetDefect
  def cookies: Seq[Cookie] = RAISE.notImplementedYetDefect
  def getValue = None
}

object ServletHttpResponse {
}
