package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.collection.immutable.Stack
import org.goldenport.RAISE
import org.goldenport.i18n.I18NContext
import org.goldenport.context.Conclusion
import org.goldenport.trace.TraceHandle
import org.goldenport.parser.{ErrorMessage, WarningMessage}
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.incident.{Incident => LibIncident}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.IncidentSequence
import org.goldenport.kaleidox.model.ServiceModel
import org.goldenport.kaleidox.model.EventModel
import org.goldenport.kaleidox.model.StateMachineModel

/*
 * @since   Sep.  9, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar.  9, 2019
 *  version Apr. 13, 2019
 *  version May. 22, 2019
 *  version Jun.  9, 2019
 *  version Aug. 25, 2019
 *  version Feb. 29, 2020
 *  version Jan.  9, 2021
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 *  version Apr. 13, 2021
 *  version May. 10, 2021
 * @version Apr. 24, 2022
 * @author  ASAMI, Tomoharu
 */
case class Universe(
  config: Space,
  setup: Space,
  parameters: Space,
  history: Vector[Universe.HistorySlot],
  stack: Stack[Blackboard],
//  trace: Vector[Conclusion],
  muteValue: Option[SExpr], // for mute
  errors: Vector[ErrorMessage],
  warnings: Vector[WarningMessage],
  model: Model
) {
  import Universe.HistorySlot

  def getI18NContext: Option[I18NContext] = None

  def service: ServiceModel = model.getServiceModel.orZero
  def event: EventModel = model.getEventModel.orZero
  def stateMachine: StateMachineModel = model.getStateMachineModel.orZero

  def init = history.headOption getOrElse Blackboard.empty
  def current = stack.headOption getOrElse Blackboard.empty
  def getValue: Option[Expression] = muteValue.map(LispExpression) orElse current.getValue
  def getValueSExpr: Option[SExpr] = muteValue orElse current.getValueSExpr
  lazy val bindings: IRecord = current.bindings update parameters.bindings update setup.bindings update config.bindings
  def getPipelineIn: Option[SExpr] = {
    // println(s"Universe: $stack")
    // println(s"Universe: ${stack.headOption}")
    // println(s"Universe: ${stack.headOption.flatMap(_.getValueSExpr)}")
    // println(s"Universe: ${stack.headOption.flatMap(_.getValueSExpr).flatMap(_.getInputSource)}")
    stack.headOption.flatMap(_.getValueSExpr)
  }

  lazy val display = s"Universe(${stack.map(_.show)})"
  def show = display

  def addSetupModel(p: Model): Universe = copy(setup = setup.addModel(p))

  def pop: Universe = pop(1) // this // pop(1) & push(1)
  def pop(n: Int): Universe = {
    @annotation.tailrec
    def go(s: Stack[Blackboard], n: Int): (Blackboard, Stack[Blackboard]) = {
      if (n <= 1)
        stack.pop2
      else
        go(s.pop, n - 1)
    }
    val (x, s) = go(stack, n)
    // val r = s.push(x)
    copy(stack = s)
  }
  def peek: SExpr = stack.apply(0).getValueSExpr.getOrElse(SNil)
  def peek(n: Int): SExpr = stack.apply(n - 1).getValueSExpr.getOrElse(SNil)
  def getStack(n: Int): Option[SExpr] = stack.apply(n - 1).getValueSExpr
  def takeHistory: SExpr = history.lastOption.flatMap(_.getValueSExpr).getOrElse(SNil)
  def takeHistory(n: Int): SExpr = history.apply(n - 1).getValueSExpr.getOrElse(SNil)
  def getHistory(n: Int): Option[SExpr] = history.apply(n - 1).getValueSExpr
  def takeCommandHistory: SExpr = RAISE.notImplementedYetDefect
  def takeCommandHistory(n: Int): SExpr = RAISE.notImplementedYetDefect
  def getCommandHistory(n: Int): Option[SExpr] = RAISE.notImplementedYetDefect

  // push and append
  def next(
    p: SExpr,
    bindings: IRecord,
    s: SExpr,
    i: IncidentSequence,
    t: TraceHandle
  ): Universe = {
    val newbb = current.next(p, bindings, s, i)
    copy(
      history = history :+ _history_slot(newbb, p, t),
      stack = stack.push(newbb),
      muteValue = None
    )
  }

  // push and append
  // def next(
  //   p: SExpr,
  //   bindings: IRecord,
  //   s: SExpr,
  //   i: Vector[LibIncident],
  //   t: TraceHandle
  // ): Universe = {
  //   val newbb = current.next(p, bindings, s, i)
  //   copy(
  //     history = history :+ _history_slot(newbb, p, t),
  //     stack = stack.push(newbb),
  //     muteValue = None
  //   )
  // }

  def next(
    p: SExpr,
    s: SExpr,
    i: IncidentSequence,
    t: TraceHandle
  ): Universe = {
    val newbb = current.next(p, s, i)
    copy(
      history = history :+ _history_slot(newbb, p, t),
      stack = stack.push(newbb),
      muteValue = None
    )
  }

  // def next(
  //   p: SExpr,
  //   s: SExpr,
  //   i: Vector[LibIncident],
  //   t: TraceHandle
  // ): Universe = {
  //   val newbb = current.next(p, s, i)
  //   copy(
  //     history = history :+ _history_slot(newbb, p, t),
  //     stack = stack.push(newbb),
  //     muteValue = None
  //   )
  // }

  private def _history_slot(bb: Blackboard, sexpr: SExpr, t: TraceHandle): HistorySlot =
    HistorySlot(bb, _conclusion(sexpr, t))

  private def _conclusion(sexpr: SExpr, t: TraceHandle): Conclusion = sexpr match {
    case m: SError => m.conclusion.withTrace(t)
    case m => Conclusion.Ok.withTrace(t)
  }

  // def next(p: SExpr, bindings: IRecord, s: SExpr): Universe = {
  //   val newbb = current.next(p, bindings, s)
  //   copy(history = history :+ newbb, stack = stack.push(newbb), muteValue = None)
  // }

  // TODO for lambda evaluation
  def next(params: List[String], args: List[SExpr], t: TraceHandle): Universe = {
    val z: List[(String, SExpr)] = params.zip(args)
    val a = Record.create(z)
    next(a, t) // TODO
  }

  def next(bindings: IRecord, t: TraceHandle): Universe = {
    val newbb = current.next(bindings)
    copy(
      history = history :+ _history_slot(newbb),
      stack = stack.push(newbb),
      muteValue = None
    )
  }

  def next(t: TraceHandle): Universe = {
    val newbb = current.next()
    copy(
      history = history :+ _history_slot(newbb),
      stack = stack.push(newbb)
    )
  }

  private def _history_slot(bb: Blackboard): HistorySlot = HistorySlot(bb, Conclusion.Ok)

//  private def _conclusion(p: TraceHandle): Conclusion = ???

  def withMuteValue(p: SExpr): Universe = copy(muteValue = Some(p))

  def makeStackParameters(n: Int): \/[SError, (Universe, List[SExpr])] =
    if (n <= 0) {
      \/-(this, Nil)
    } else if (stack.length < n) {
      -\/(SError.stackUnderflow)
    } else {
      def go(i: Int, s: Stack[Blackboard], r: Vector[SExpr]): (Universe, List[SExpr]) =
        if (i <= 0) {
          (copy(stack = s), r.toList)
        } else {
          val (x, s1) = s.pop2
          go(i - 1, s1, r :+ x.getValueSExpr.getOrElse(SNil))
        }
      \/-(go(n, stack, Vector.empty))
    }

  def getBinding(p: String): Option[SExpr] = {
    current.getBinding(p) orElse
    parameters.getBinding(p) orElse
    setup.getBinding(p) orElse
    config.getBinding(p)
  }
}

object Universe {
  case class HistorySlot(blackboard: Blackboard, conclusion: Conclusion) {
    def getValueSExpr = blackboard.getValueSExpr
    def getStimulusSExpr = blackboard.getStimulusSExpr
    def getIncident = blackboard.getIncident
  }
  object HistorySlot {
    val empty = HistorySlot(Blackboard.empty, Conclusion.Ok)
  }

  val empty = Universe(
    Space.empty,
    Space.empty,
    Space.empty,
    Vector(HistorySlot.empty),
    Stack(Blackboard.empty),
//    Vector(Conclusion.Ok),
    None,
    Vector.empty,
    Vector.empty,
    Model.empty
  )

  def apply(
    config: Space,
    setup: Space,
    parameters: Space,
    init: Blackboard,
    model: Model
  ): Universe =
    Universe(
      config,
      setup,
      parameters,
      Vector(HistorySlot(init, Conclusion.Ok)),
      Stack(init),
//      Vector(Conclusion.Ok),
      None,
      model.errors,
      model.warnings,
      model
    )
}
