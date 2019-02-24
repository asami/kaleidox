package org.goldenport.kaleidox.lisp

import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import org.goldenport.kaleidox._

/*
 * @since   Aug. 19, 2018
 *  version Sep. 30, 2018
 * @version Oct. 17, 2018
 * @author  ASAMI, Tomoharu
 */
case class Context(
  evaluator: LispContext => LispContext,
  executionContext: ExecutionContext,
  universe: Universe,
  valueOption: Option[SExpr],
  bindingsOption: Option[IRecord]
) extends org.goldenport.sexpr.eval.LispContext {
  override def toString() = s"Context(${value.show}, ${universe.show})"

  def config = executionContext.config
  def serviceLogic = executionContext.serviceLogic
  def storeLogic = executionContext.storeLogic
  def scriptContext = executionContext.scriptContext

  def pure(p: SExpr): Context = copy(valueOption = Some(p))
  def value: SExpr = getValue getOrElse SNil
  def getValue: Option[SExpr] = valueOption orElse universe.getValue.map(_.asSExpr)
  def bindings: IRecord = bindingsOption getOrElse universe.bindings

  def toResult(p: SExpr, bindings: IRecord) = copy(valueOption = Some(p), bindingsOption = Some(bindings))

  def push = (valueOption, bindingsOption) match {
    case (Some(v), Some(b)) => copy(universe = universe(v, b))
    case (Some(v), None) => copy(universe = universe(v))
    case (None, Some(b)) => copy(universe = universe(b))
    case (None, None) => copy(universe = universe())
  }
  
  override def pop = copy(valueOption = universe.getValueSExpr, bindingsOption = Some(universe.bindings), universe = universe.pop)

  override def getPipelineIn: Option[SExpr] = universe.getPipelineIn
}
