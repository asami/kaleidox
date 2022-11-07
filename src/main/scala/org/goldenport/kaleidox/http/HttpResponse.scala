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
import org.goldenport.sexpr.SExpr
import org.goldenport.kaleidox.{ExecutionContext, EvalReport}

/*
 * See org.goldenport.record.http.Response, arcadia.context.Response
 *
 * @since   Mar.  2, 2021
 *  version Mar. 28, 2021
 *  version Apr. 22, 2021
 *  version Mar. 19, 2022
 * @version Oct. 30, 2022
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
  def getValue: Option[SExpr]
}

object HttpResponse {
  private def _ok = Plain()
  private def _json = Plain(mimeType = Strings.mimetype.application_json)
  val ok: HttpResponse = _ok
  val json: HttpResponse = _json

  case class Plain(
    status: Int = 200,
    mimeType: String = Strings.mimetype.text_html,
    charset: Charset = Platform.charset.UTF8,
    headers: List[Header] = Nil,
    cookies: List[Cookie] = Nil,
    content: ChunkBag = EmptyBag, // TODO lazy for performance in cozy.
    value: Option[SExpr] = None
  ) extends HttpResponse {
    def getValue = value
    def withContent(p: String): Plain = withContent(new StringBag(p))
    def withContent(p: ChunkBag): Plain = copy(content = p)
    def withCodeContent(code: Int, p: String): Plain = withCodeContent(code, new StringBag(p))
    def withCodeContent(code: Int, p: ChunkBag): Plain = copy(status = code, content = p)
    def withValue(p: SExpr) = copy(value = Some(p))
  }

  def json(p: String): HttpResponse = _json.withContent(new StringBag(p))

  def conclusion(
    ctx: ExecutionContext,
    params: HttpParameters,
    report: EvalReport,
    p: Conclusion
  ): HttpResponse =
    ConclusionLogic(ctx, params, Logic.Common.create(params), report, p).apply()

  case class ConclusionLogic(
    context: ExecutionContext,
    params: HttpParameters,
    common: Logic.Common,
    report: EvalReport,
    p: Conclusion
  ) extends Logic {
    def apply(): HttpResponse = {
      val rec = Record.data(
        "status" -> p.code
      ) + Record.dataOption(
        "detail" -> p.status.detail.map(_.code),
        "app_status" -> p.status.application,
        "reaction" -> _reaction,
        "error" -> _error
      ) + response_common
      val content = rec.toJsonString
      _json.withCodeContent(p.code, content)
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

  def data(
    ctx: ExecutionContext,
    params: HttpParameters,
    report: EvalReport,
    p: SExpr
  ): HttpResponse =
    DataLogic(ctx, params, Logic.Common.create(params), report, p).apply()

  case class DataLogic(
    context: ExecutionContext,
    params: HttpParameters,
    common: Logic.Common,
    report: EvalReport,
    data: SExpr
  ) extends Logic {
    def apply(): HttpResponse = {
      val rec = Record.data(
        "status" -> 200,
        "data" -> data.asRecordOrJavaObject
      ) + response_common
      val content = rec.toJsonString
      _json.withCodeContent(200, content).withValue(data)
    }
  }

  trait Logic extends ExecutionContext.Logic with Logic.Common.Holder {
    def context: ExecutionContext
    def params: HttpParameters
    def common: Logic.Common
    def report: EvalReport

    def apply(): HttpResponse

    private def _if_available[T](b: Boolean)(f: => Option[T]): Option[T] =
      if (b)
        f
      else
        None

    protected final def response_common: Record = Record.dataOption(
      "warning" -> None,
      "trace" -> _if_available(isTrace)(report_trace),
      "metrics" -> _if_available(isMetrics)(report_metrics),
      "system_status" -> None
    )

    protected final def report_trace: Option[Record] =
      Some(_trace(report.traceContext.toTrace))

    // private def _trace: Option[Record] = Some(_trace(conclusion.trace))

    private def _trace(p: Trace): Record = Record.create(p.properties)

    protected final def report_metrics: Option[Record] = {
      None
    }
  }
  object Logic {
    case class Common(
      isTrace: Boolean,
      isMetrics: Boolean
    )
    object Common {
      trait Holder {
        def common: Common

        def isTrace: Boolean = common.isTrace
        def isMetrics: Boolean = common.isMetrics
      }

      def create(p: HttpParameters): Common =
        Common(
          p.controls.isTrace,
          p.controls.isMetrics
        )
    }
  }
}
