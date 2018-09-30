package org.goldenport.kaleidox

import scala.collection.immutable.Stack
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._

/*
 * @since   Aug. 20, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class Blackboard(
  source: Space,
  sink: Space
) {
  def getValue: Option[Expression] = sink.getValue
  def getValueSExpr: Option[SExpr] = getValue.map(_.asSExpr)
  def bindings: Record = sink.bindings

  def show = s"Bloackboard(${getValue})"

  def apply(p: SExpr, bindings: Record) = copy(
    sink = sink(p, bindings)
  )

  def apply(p: SExpr) = copy(
    sink = sink(p)
  )

  def apply(bindings: Record) = copy(
    sink = sink(bindings)
  )

  def apply() = copy(
    sink = sink()
  )
}

object Blackboard {
  val empty = Blackboard(Space.empty, Space.empty)
}
