package org.goldenport.kaleidox

import scala.collection.immutable.Stack
import org.goldenport.record.v3.IRecord
import org.goldenport.incident.Incident
import org.goldenport.sexpr._

/*
 * @since   Aug. 20, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar.  2, 2019
 *  version Jun.  9, 2019
 * @version Mar. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class Blackboard(
  source: Space,
  sink: Space
) {
  def getValue: Option[Expression] = sink.getValue
  def getValueSExpr: Option[SExpr] = getValue.map(_.asSExpr)
  def getStimulus: Option[Expression] = sink.getStimulus
  def getStimulusSExpr: Option[SExpr] = getStimulus.map(_.asSExpr)
  def getIncident: Option[Incident] = sink.getIncident
  def bindings: IRecord = sink.bindings

  def show = s"Blackboard(${getValue})"

  def next(p: SExpr, updates: IRecord, s: SExpr, i: Option[Incident]) = copy(
    sink = sink.next(p, bindings.update(updates), s, i)
  )

  def next(p: SExpr, s: SExpr, i: Option[Incident]) = copy(
    sink = sink.next(p, s, i)
  )

  def next(updates: IRecord, s: SExpr) = copy(
    sink = sink.next(bindings.update(updates), s)
  )

  def next() = copy(
    sink = sink.next()
  )

  def next(updates: IRecord) = copy(
    sink = sink.next(bindings.update(updates))
  )

  def getBinding(p: String): Option[SExpr] = source.getBinding(p)
}

object Blackboard {
  val empty = Blackboard(Space.empty, Space.empty)
}
