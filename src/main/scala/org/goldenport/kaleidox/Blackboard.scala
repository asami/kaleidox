package org.goldenport.kaleidox

import scala.collection.immutable.Stack
import org.goldenport.record.v3.IRecord
import org.goldenport.sexpr._

/*
 * @since   Aug. 20, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 * @version Mar.  2, 2019
 * @author  ASAMI, Tomoharu
 */
case class Blackboard(
  source: Space,
  sink: Space
) {
  def getValue: Option[Expression] = sink.getValue
  def getValueSExpr: Option[SExpr] = getValue.map(_.asSExpr)
  def bindings: IRecord = sink.bindings

  def show = s"Bloackboard(${getValue})"

  def apply(p: SExpr, updates: IRecord) = copy(
    sink = sink(p, bindings.update(updates))
  )

  def apply(p: SExpr) = copy(
    sink = sink(p)
  )

  def apply(updates: IRecord) = copy(
    sink = sink(bindings.update(updates))
  )

  def apply() = copy(
    sink = sink()
  )

  def getBinding(p: String): Option[SExpr] = source.getBinding(p)
}

object Blackboard {
  val empty = Blackboard(Space.empty, Space.empty)
}
