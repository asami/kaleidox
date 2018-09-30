package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.collection.immutable.Stack
import org.goldenport.record.v3.Record
import org.goldenport.sexpr._

/*
 * @since   Sep.  9, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class Universe(
  config: Space,
  parameters: Space,
  history: Vector[Blackboard],
  stack: Stack[Blackboard]
) {
  def init = history.head
  def current = stack.head
  def getValue = current.getValue
  def getValueSExpr = current.getValueSExpr
  def bindings = current.bindings

  def show = s"Universe(${stack.map(_.show)})"

  def pop = copy(stack = stack.pop)

  // push and append
  def apply(p: SExpr, bindings: Record) = {
    val newbb = current(p, bindings)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply(p: SExpr) = {
    val newbb = current(p)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply(bindings: Record) = {
    val newbb = current(bindings)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply() = {
    val newbb = current()
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }
}

object Universe {
  val empty = Universe(
    Space.empty,
    Space.empty,
    Vector(Blackboard.empty),
    Stack(Blackboard.empty)
  )

  def apply(config: Space, parameters: Space, init: Blackboard): Universe =
    Universe(config, parameters, Vector(init), Stack(init))
}
