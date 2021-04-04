package org.goldenport.kaleidox.http

import java.nio.charset.Charset
import java.io.OutputStream
import org.goldenport.Platform
import org.goldenport.Strings
import org.goldenport.context.{Conclusion, Fault}
import org.goldenport.context.ReactionStrategy
import org.goldenport.trace._
import org.goldenport.bag.{ChunkBag, EmptyBag, StringBag}
import org.goldenport.record.v3.Record
import org.goldenport.kaleidox.{ExecutionContext, EvalReport}

/*
 * See org.goldenport.record.http.Response, arcadia.context.Response
 *
 * @since   Mar.  2, 2021
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
trait HttpResponse {
  def status: Int
  def mimeType: String
  def charset: Charset
  def contentType: String = s"${mimeType}; ${charset.name}"
  def content: ChunkBag
  def headers: Seq[Header]
  def cookies: Seq[Cookie]
  def writeContent(p: OutputStream): Unit = content.copyTo(p)
  def writeContentClose(p: OutputStream): Unit = {
    writeContent(p)
    p.close()
  }
}

object HttpResponse {
  private def _ok = Standard()
  private def _json = Standard(mimeType = Strings.mimetype.application_json)
  val ok: HttpResponse = _ok
  val json: HttpResponse = _json

  case class Standard(
    status: Int = 200,
    mimeType: String = Strings.mimetype.text_html,
    charset: Charset = Platform.charset.UTF8,
    headers: List[Header] = Nil,
    cookies: List[Cookie] = Nil,
    content: ChunkBag = EmptyBag
  ) extends HttpResponse {
    def withContent(p: String): Standard = withContent(new StringBag(p))
    def withContent(p: ChunkBag): Standard = copy(content = p)
    def withCodeContent(code: Int, p: String): Standard = withCodeContent(code, new StringBag(p))
    def withCodeContent(code: Int, p: ChunkBag): Standard = copy(status = code, content = p)
  }

  def json(p: String): HttpResponse = _json.withContent(new StringBag(p))

  def conclusion(ctx: ExecutionContext, report: EvalReport, p: Conclusion): HttpResponse =
    ConclusionLogic(ctx, report, p).apply()

  case class ConclusionLogic(
    context: ExecutionContext,
    report: EvalReport,
    p: Conclusion
  ) extends LogicBase {
    def apply(): HttpResponse = {
      val rec = Record.data(
        "status" -> p.code.main
      ) + Record.dataOption(
        "detail" -> p.code.detail.map(_.code),
        "app_status" -> p.code.application,
        "reaction" -> _reaction,
        "error" -> _error
      ) + response_common
      val content = rec.toJsonString
      _json.withCodeContent(p.code.main, content)
    }

    private def _reaction: Option[String] = p.strategy.reaction match {
      case ReactionStrategy.NoneReaction => None
      case _ => Some(p.strategy.reaction.name)
    }

    private def _error: Option[Vector[Record]] = {
      val r = p.faults.faults.map(_error)
      if (r.isEmpty)
        None
      else
        Some(r)
    }

    private def _error(p: Fault): Record = Record.create(p.properties(locale))

  }

  def data(ctx: ExecutionContext, report: EvalReport, p: String): HttpResponse =
    DataLogic(ctx, report, p).apply()

  case class DataLogic(
    context: ExecutionContext,
    report: EvalReport,
    data: String
  ) extends LogicBase {
    def apply(): HttpResponse = {
      val rec = Record.data(
        "status" -> 200,
        "data" -> data
      ) + response_common
      val content = rec.toJsonString
      _json.withCodeContent(200, content)
    }
  }

  trait LogicBase extends ExecutionContext.Logic {
    def context: ExecutionContext
    def report: EvalReport

    def apply(): HttpResponse

    protected final def response_common: Record = Record.dataOption(
      "warning" -> None,
      "trace" -> report_trace,
      "statictics" -> report_statictics,
      "system_status" -> None
    )

    protected final def report_trace: Option[Record] =
      Some(_trace(report.traceContext.toTrace))

    // private def _trace: Option[Record] = Some(_trace(conclusion.trace))

    private def _trace(p: Trace): Record = Record.create(p.properties)

    protected final def report_statictics: Option[Record] = {
      None
    }
  }
}
