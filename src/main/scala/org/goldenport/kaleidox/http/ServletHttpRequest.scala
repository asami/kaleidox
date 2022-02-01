package org.goldenport.kaleidox.http

import scala.collection.JavaConverters._
import java.net.URL
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.Part
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.record.http.Request.Method
import org.goldenport.record.parser.RecordParser
import org.goldenport.values.PathName

/*
 * @since   Mar.  2, 2021
 *  version Mar. 21, 2021
 *  version Apr. 22, 2021
 * @version Jan. 23, 2022
 * @author  ASAMI, Tomoharu
 */
case class ServletHttpRequest(request: HttpServletRequest) extends HttpRequest {
  val url: URL = new URL(request.getRequestURL.toString)
  val pathname: PathName = {
    val a1 = request.getServletPath
    val a2 = request.getRequestURI
    val a = if (Strings.notblankp(a1))
      a1
    else if (Strings.notblankp(a2))
      a2
    else 
      url.getPath
    PathName(a)
  }
  val method: Method = Method.takeIgnoreCase(request.getMethod)

  private val _parser_config = RecordParser.Config.default // TODO

  val queryWhole: IRecord = {
    val parser = RecordParser(_parser_config)
    parser.httpQuery(Option(request.getQueryString)).take
  }
  val formWhole: IRecord =
    if (request.getContentType == Strings.mimetype.multipart_form_data) {
      val parser = RecordParser(_parser_config)
      val xs = request.getParts.asScala.map(_part)
      Record.create(xs)
    } else {
      Record.empty
    }

  private def _part(p: Part): (String, Any) = {
    RAISE.notImplementedYetDefect
  }
}

object ServletHttpRequest {
}
