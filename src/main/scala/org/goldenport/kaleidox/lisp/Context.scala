package org.goldenport.kaleidox.lisp

import org.goldenport.context.Effect
import org.goldenport.trace.TraceContext
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.incident.{Incident => LibIncident}
import org.goldenport.statemachine.{ExecutionContext => StateMachineContext}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval._
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.model.KaleidoxStateMachineLogic
import org.goldenport.kaleidox.extension.modeler.Modeler

/*
 * @since   Aug. 19, 2018
 *  version Sep. 30, 2018
 *  version Oct. 17, 2018
 *  version Mar. 24, 2019
 *  version Apr. 12, 2019
 *  version May. 21, 2019
 *  version Jun.  9, 2019
 *  version Aug. 25, 2019
 *  version Sep. 28, 2019
 *  version Feb. 29, 2020
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 *  version Apr. 13, 2021
 *  version May. 21, 2021
 *  version Nov. 29, 2021
 * @version Dec. 18, 2021
 * @author  ASAMI, Tomoharu
 */
case class Context(
  evaluator: LispContext => LispContext,
  kaleidoxEvaluator: Evaluator,
  executionContext: ExecutionContext,
  universe: Universe,
  valueOption: Option[SExpr],
  incident: Option[LibIncident],
  bindingsOption: Option[IRecord],
  futureEffect: Option[Effect.FutureEffect] = None
) extends org.goldenport.sexpr.eval.LispContext {
  override def toString() = display

  def config = executionContext.config
  def i18nContext = executionContext.i18nContext
  def serviceLogic = executionContext.serviceLogic
  def storeLogic = executionContext.storeLogic
  def scriptContext = executionContext.scriptContext
  def sqlContext = executionContext.sqlContext
  def resourceManager = executionContext.resourceManager
  def numericalOperations = executionContext.numericalOperations
  def feature = executionContext.feature
  def extension = executionContext.extension
  def traceContext = executionContext.traceContext
  // lazy val statemachineContext: StateMachineContext = StateMachineContext.create(
  //   executionContext.traceContext,
  //   KaleidoxStateMachineLogic.Factory.create,
  //   this
  // )
  def statemachineSpace = executionContext.statemachineSpace
  lazy val display = s"Context(${value.display}, ${universe.display})"

  def setModeler(p: Modeler) = copy(
    executionContext = executionContext.setModeler(p)
  )

  def pure(p: SExpr): Context = copy(valueOption = Some(p))
  def createContextForFuture(effect: Effect.FutureEffect): LispContext =
    copy(executionContext = executionContext.newContextForFuture(), futureEffect = Some(effect))
  def value: SExpr = getValue getOrElse SNil
  def getValue: Option[SExpr] = valueOption orElse universe.getValue.map(_.asSExpr)
  lazy val bindings: IRecord = bindingsOption.getOrElse(Record.empty) update universe.bindings

  def toResult(p: SExpr, i: Option[LibIncident], bindings: IRecord) = copy(
    valueOption = Some(p),
    incident = i,
    bindingsOption = Some(bindings)
  )
  def addBindings(bindings: IRecord) = copy(bindingsOption = Some(bindings))

  def withUniverse(u: Universe) = copy(universe = u)

  def push(p: SExpr) = {
    val t = traceContext.toHandle
    (valueOption, bindingsOption) match {
      case (Some(v), Some(b)) => copy(universe = universe.next(v, b, p, incident, t))
      case (Some(v), None) => copy(universe = universe.next(v, p, incident, t))
      case (None, Some(b)) => copy(universe = universe.next(b, t))
      case (None, None) => copy(universe = universe.next(t))
    }
  }

  def pushOrMute(p: SExpr) = valueOption match {
    case Some(SMute(expr)) => copy(universe = universe.withMuteValue(expr))
    case _ => push(p)
  }

  override def pop = pop(1)
  override def pop(n: Int) = copy(
    valueOption = universe.getValueSExpr.map(SMute(_)),
    bindingsOption = Some(universe.bindings),
    universe = universe.pop(n)
  )
  override def peek = SMute(universe.peek)
  override def peek(n: Int) = SMute(universe.peek(n))
  override def getStack(n: Int) = universe.getStack(n)
  override def takeHistory(n: Int) = universe.takeHistory(n)
  override def getHistory(n: Int) = universe.getHistory(n)
  override def getCommandHistory(n: Int) = universe.getCommandHistory(n)

  override def getPipelineIn: Option[SExpr] = universe.getPipelineIn
}
