package org.goldenport.kaleidox

import scalaz._, Scalaz._

import org.goldenport.RAISE
import org.goldenport.trace.TraceContext
import org.goldenport.console.{MessageSequence, Message}

/*
 * @since   Mar. 28, 2021
 * @version Mar. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class EvalReport(
  traceContext: TraceContext = TraceContext.create(),
  messages: MessageSequence = MessageSequence.empty
) {
  def +(rhs: EvalReport) = {
    val tc = if (rhs.traceContext == traceContext)
      traceContext
    else if (rhs.traceContext.isEmpty)
      traceContext
    else if (traceContext.isEmpty)
      rhs.traceContext
    else
      RAISE.notImplementedYetDefect
    if (tc == traceContext && rhs.messages.isEmpty)
      this
    else
      EvalReport(tc, messages + rhs.messages)
  }
}

object EvalReport {
  implicit object EvalReportMonoid extends Monoid[EvalReport] {
    def zero = empty
    def append(lhs: EvalReport, rhs: => EvalReport) = lhs + rhs
  }

  val empty = EvalReport()

  def create(p: ExecutionContext): EvalReport = EvalReport(
    p.traceContext
  )
}
