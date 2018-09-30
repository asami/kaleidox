package org.goldenport.kaleidox

import org.goldenport.bag.BufferBag
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Aug. 11, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class Kaleidox(
  config: Config
) {
  val context = ExecutionContext(
    config, 
    config.serviceLogic,
    config.storeLogic,
    config.scriptContext
  )
  val interpreter = Interpreter.create(context)
  val configspace = Space.empty // TODO
  val parameterspace = Space.empty // TODO
  val blackboard = Blackboard.empty // TODO
  val universe = Universe(configspace, parameterspace, blackboard)
  val engine = Engine(context, universe, interpreter)

  def apply(p: Model): Vector[Expression] = engine.apply(p)
  def apply(p: Script): Vector[Expression] = engine.apply(p)
}

object Kaleidox {
  def main(args: Array[String]) = {
    val config = Config.default
    val source = BufferBag.fromUri(args(0))
    val model = Model.parse(source.toText)
    // println(s"Kaleidox: $model")
    val rs = Kaleidox(config)(model)
    println("*** RESULT ***")
    rs.foreach(r => println(r.print))
  }
}
