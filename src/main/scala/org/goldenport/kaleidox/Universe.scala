package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.collection.immutable.Stack
import org.goldenport.RAISE
import org.goldenport.i18n.I18NContext
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.incident.{Incident => LibIncident}
import org.goldenport.sexpr._

/*
 * @since   Sep.  9, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar.  9, 2019
 *  version Apr. 13, 2019
 *  version May. 22, 2019
 *  version Jun.  9, 2019
 * @version Aug. 25, 2019
 * @author  ASAMI, Tomoharu
 */
case class Universe(
  config: Space,
  setup: Space,
  parameters: Space,
  history: Vector[Blackboard],
  stack: Stack[Blackboard],
  muteValue: Option[SExpr] // for mute
) {
  def getI18NContext: Option[I18NContext] = None

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
  def takeHistory: SExpr = RAISE.notImplementedYetDefect
  def takeHistory(n: Int): SExpr = history.apply(n - 1).getValueSExpr.getOrElse(SNil)
  def takeCommandHistory: SExpr = RAISE.notImplementedYetDefect
  def takeCommandHistory(n: Int): SExpr = RAISE.notImplementedYetDefect

  // push and append
  def next(p: SExpr, bindings: IRecord, s: SExpr, i: Option[LibIncident]): Universe = {
    val newbb = current.next(p, bindings, s, i)
    copy(history = history :+ newbb, stack = stack.push(newbb), muteValue = None)
  }

  def next(p: SExpr, s: SExpr, i: Option[LibIncident]): Universe = {
    val newbb = current.next(p, s, i)
    copy(history = history :+ newbb, stack = stack.push(newbb), muteValue = None)
  }

  // def next(p: SExpr, bindings: IRecord, s: SExpr): Universe = {
  //   val newbb = current.next(p, bindings, s)
  //   copy(history = history :+ newbb, stack = stack.push(newbb), muteValue = None)
  // }

  def next(params: List[String], args: List[SExpr]): Universe = {
    val z: List[(String, SExpr)] = params.zip(args)
    val a = Record.create(z)
    next(a)
  }

  def next(bindings: IRecord): Universe = {
    val newbb = current.next(bindings)
    copy(history = history :+ newbb, stack = stack.push(newbb), muteValue = None)
  }

  def next(): Universe = {
    val newbb = current.next()
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

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
  val empty = Universe(
    Space.empty,
    Space.empty,
    Space.empty,
    Vector(Blackboard.empty),
    Stack(Blackboard.empty),
    None
  )

  def apply(config: Space, setup: Space, parameters: Space, init: Blackboard): Universe =
    Universe(config, setup, parameters, Vector(init), Stack(init), None)
}
