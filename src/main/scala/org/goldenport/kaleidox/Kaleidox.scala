package org.goldenport.kaleidox

import org.goldenport.cli._
import org.goldenport.bag.BufferBag
import org.goldenport.record.v3.{IRecord, Record}
import org.goldenport.parser.LogicalLines
import org.goldenport.kaleidox.interpreter.Interpreter
import org.goldenport.util.StringUtils

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Oct. 21, 2018
 * @version Feb. 14, 2019
 * @author  ASAMI, Tomoharu
 */
case class Kaleidox(
  config: Config,
  environment: Environment
) {
  val context = ExecutionContext(
    config, 
    config.serviceLogic,
    config.storeLogic,
    config.scriptContext
  )
  val interpreter = Interpreter.create(context)
  val configspace = Space.create(config.cliConfig.properties)

  def repl(call: OperationCall) {
    val (universe, model) = _build_universe(call)
    val engine0 = Engine(context, universe, interpreter)
    engine0.initialize()
    val engine = model.map(engine0.setup).getOrElse(engine0)
    val state = Kaleidox.KaleidoxState(engine, universe)
    val repl = new ReadEvalPrintLoop(environment, state, LogicalLines.Config.lisp)
    repl.execute()
  }

  def execute(call: OperationCall) {
    val (universe, model) = _build_universe(call)
    val engine = Engine(context, universe, interpreter)
    engine.initialize()
    model.map { x =>
      val rs = engine.apply(x)
      // println(s"Kaleidox: $model")
      rs.foreach(r => println(r.print))
    }
  }

  private def _build_universe(call: OperationCall): (Universe, Option[Model]) = {
    val (setupspace, model) = call.request.arguments match {
      case Nil => (Space.empty, None)
      case args => 
        val source = args(0).toInputText
        val model = Model.parse(source)
        val u = model.getEnvironmentProperties.
          map(Space.create).
          getOrElse(Space.empty)
        (u, Some(model))
    }
    val parameterspace = Space.create(_to_parameter_record(call.request))
    val blackboard = Blackboard.empty // TODO
    (Universe(configspace, setupspace, parameterspace, blackboard), model)
  }

  private def _to_parameter_record(p: Request): IRecord = {
    val as = p.arguments.map(_.value)
    val ps: Seq[(String, Any)] = p.properties.map(x => x.name -> x.value.value)
    Record.data(
      "request" -> (
        Record.data(
          "argument" -> as
        ) + Record.createDataSeq(ps)
      )
    )
  }
}

object Kaleidox {
  case class KaleidoxState(
    engine: Engine,
    universe: Universe
  ) extends ReadEvalPrintLoop.IState {
    import org.goldenport.cli.ReadEvalPrintLoop._
    def apply(p: ReplEvent): (IState, String) = {
      val prompt = "kaleidox> "
      p match {
        case ReplStart => (this, s"$prompt")
        case ReplEnd => ???
        case ReplLine(l) =>
          val s = l.text
          val model = Model.parse(s)
          val (_, r, newuniverse) = engine.run(universe, model)
          val o = r.map(x => s"${x.resolve.print}\n").mkString
          val output = StringUtils.showConsole(o, engine.context.newline)
          val newstate = copy(universe = newuniverse)
          (newstate, s"$output\n$prompt")
      }
    }
  }

  def main(args: Array[String]) = {
    val env = Environment.create(args)
    val config = Config.create(env)
    val kal = Kaleidox(config, env)
    val req = spec.Request.empty // TODO
    val res = spec.Response() // TODO
    val op = spec.Operation("kal", req, res)
    val call = OperationCall.create(op, args)
    if (call.request.arguments.isEmpty)
      kal.repl(call)
    else
      kal.execute(call)
  }
}
