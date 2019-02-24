package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.collection.immutable.Stack
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.sexpr._

/*
 * @since   Sep.  9, 2018
 *  version Sep. 30, 2018
 * @version Oct. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class Universe(
  config: Space,
  setup: Space,
  parameters: Space,
  history: Vector[Blackboard],
  stack: Stack[Blackboard]
) {
  def init = history.head
  def current = stack.head
  def getValue = current.getValue
  def getValueSExpr = current.getValueSExpr
  def bindings = current.bindings
  def getPipelineIn: Option[SExpr] = {
    // println(s"Universe: $stack")
    // println(s"Universe: ${stack.headOption}")
    // println(s"Universe: ${stack.headOption.flatMap(_.getValueSExpr)}")
    // println(s"Universe: ${stack.headOption.flatMap(_.getValueSExpr).flatMap(_.getInputSource)}")
    stack.headOption.flatMap(_.getValueSExpr)
  }

  def show = s"Universe(${stack.map(_.show)})"

  def pop = copy(stack = stack.pop)

  // push and append
  def apply(p: SExpr, bindings: IRecord) = {
    val newbb = current(p, bindings)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply(p: SExpr) = {
    val newbb = current(p)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply(bindings: IRecord) = {
    val newbb = current(bindings)
    copy(history = history :+ newbb, stack = stack.push(newbb))
  }

  def apply() = {
    val newbb = current()
    copy(history = history :+ newbb, stack = stack.push(newbb))
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
    Stack(Blackboard.empty)
  )

  def apply(config: Space, setup: Space, parameters: Space, init: Blackboard): Universe =
    Universe(config, setup, parameters, Vector(init), Stack(init))
}
