package org.goldenport.kaleidox.lisp

import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import org.goldenport.kaleidox._

/*
 * @since   Aug. 19, 2018
 *  version Sep. 30, 2018
 *  version Oct. 17, 2018
 *  version Mar. 24, 2019
 *  version Apr. 12, 2019
 * @version May. 21, 2019
 * @author  ASAMI, Tomoharu
 */
case class Context(
  evaluator: LispContext => LispContext,
  executionContext: ExecutionContext,
  universe: Universe,
  valueOption: Option[SExpr],
  incident: Option[Incident],
  bindingsOption: Option[IRecord]
) extends org.goldenport.sexpr.eval.LispContext {
  override def toString() = s"Context(${value.show}, ${universe.show})"

  def config = executionContext.config
  def serviceLogic = executionContext.serviceLogic
  def storeLogic = executionContext.storeLogic
  def scriptContext = executionContext.scriptContext
  def sqlContext = executionContext.sqlContext
  def feature = executionContext.feature

  def pure(p: SExpr): Context = copy(valueOption = Some(p))
  def value: SExpr = getValue getOrElse SNil
  def getValue: Option[SExpr] = valueOption orElse universe.getValue.map(_.asSExpr)
  lazy val bindings: IRecord = bindingsOption.getOrElse(Record.empty) update universe.bindings

  def toResult(p: SExpr, i: Option[Incident], bindings: IRecord) = copy(valueOption = Some(p), incident = i, bindingsOption = Some(bindings))
  def addBindings(bindings: IRecord) = copy(bindingsOption = Some(bindings))

  def push = (valueOption, bindingsOption) match {
    case (Some(v), Some(b)) => copy(universe = universe(v, b))
    case (Some(v), None) => copy(universe = universe(v))
    case (None, Some(b)) => copy(universe = universe(b))
    case (None, None) => copy(universe = universe())
  }

  def pushOrMute = valueOption match {
    case Some(SMute(expr)) => copy(universe = universe.withMuteValue(expr))
    case _ => push
  }

  override def pop = pop(1)
  override def pop(n: Int) = copy(
    valueOption = universe.getValueSExpr.map(SMute(_)),
    bindingsOption = Some(universe.bindings),
    universe = universe.pop(n)
  )
  override def peek = universe.peek
  override def peek(n: Int) = universe.peek(n)
  override def takeHistory(n: Int) = universe.takeHistory(n)

  override def getPipelineIn: Option[SExpr] = universe.getPipelineIn
}
