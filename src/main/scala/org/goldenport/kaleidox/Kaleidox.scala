package org.goldenport.kaleidox

import scalaz._, Scalaz._
import java.io.File
import org.goldenport.RAISE
import org.goldenport.log.{LogContext, LogLevel}
import org.goldenport.cli._
import org.goldenport.bag.BufferBag
import org.goldenport.record.v3.Record
import org.goldenport.parser.LogicalLines
import org.goldenport.kaleidox.interpreter.Interpreter
import org.goldenport.util.StringUtils

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Oct. 21, 2018
 *  version Feb. 28, 2019
 *  version Mar. 24, 2019
 *  version Apr. 18, 2019
 * @version May. 20, 2019
 * @author  ASAMI, Tomoharu
 */
case class Kaleidox(
  config: Config,
  environment: Environment
) {
  // val context = ExecutionContext(
  //   config, 
  //   config.serviceLogic,
  //   config.storeLogic,
  //   config.scriptContext,
  //   config.sqlContext,
  //   config.feature
  // )
  // val interpreter = Interpreter.create(context)
  val configspace = Space.create(config.cliConfig.properties)

  def repl(call: OperationCall) {
    val engine = _build_world(call)
    val state = Kaleidox.KaleidoxState(engine, engine.universe)
    val repl = new ReadEvalPrintLoop(environment, state, LogicalLines.Config.lisp)
    repl.execute()
  }

  def execute(call: OperationCall) {
    val engine = _build_world(call)
    // model.map { x =>
    //   val rs = engine.apply(x)
    //   // println(s"Kaleidox: $model")
    //   rs.foreach(r => println(r.print))
    // }
    engine.universe.getValue.map(r => println(r.print))
  }

  private def _build_world(call: OperationCall): Engine = {
    LogContext.setRootLevel(LogLevel.Info)
    val (universe, model) = _build_universe(call)
    val sqlcontext = config.sqlContext.
      addProperties(universe.setup.bindings).
      addProperties(universe.parameters.bindings)
    val context = ExecutionContext(
      config,
      config.serviceLogic,
      config.storeLogic,
      config.scriptContext,
      sqlcontext,
      config.feature
    )
    val interpreter = Interpreter.create(context)
    val engine0 = Engine(context, universe, interpreter)
    engine0.initialize()
    engine0.setup(model)
  }

  private def _build_universe(call: OperationCall): (Universe, Model) = {
    val confmodel = _conf_model
    val inmodels = call.request.arguments.map(_.asString).map(Model.load(config, _))
    val model = inmodels./:(confmodel)(_+_)
    val setupspace = _space(model)
    // val (setupspace, model) = call.request.arguments match {
    //   case Nil => (Space.empty, None)
    //   case args => 
    //     val source = args(0).toInputText
    //     val model = Model.parse(config, source)
    //     val space0 = model.getEnvironmentProperties.
    //       map(Space.create).
    //       getOrElse(Space.empty)
    //     val space1 = model.getVoucherModel.
    //       map(_.setup(space0)).
    //       getOrElse(space0)
    //     // println(s"XXX: $space1")
    //     val space = space1
    //     (space, Some(model))
    // }
    val parameterspace = Space.create(_to_parameter_record(call.request))
    val blackboard = Blackboard.empty // TODO
    (Universe(configspace, setupspace, parameterspace, blackboard), model)
  }

  private def _conf_model: Model = _implicit_model_files.foldMap(Model.load(config, _))

  private def _implicit_model_files: List[File] = {
    val home = config.homeDirectory.map(x => new File(x, ".init.kld"))
    val homeindir = config.homeDirectory.map(x => new File(x, ".kaleidox/init.kld"))
    val projectimplicit = config.getProjectDirectory.map(x => new File(x, ".init.kld"))
    val project = config.getProjectDirectory.map(x => new File(x, "init.kld"))
    val workimplicit = config.workDirectory.map(x => new File(x, ".init.kld"))
    val work = config.workDirectory.map(x => new File(x, "init.kld"))
    List(
      home,
      homeindir,
      projectimplicit,
      project,
      workimplicit,
      work
    ).flatten.distinct.filter(_.exists)
  }

  private def _space(model: Model) = {
    val space0 = model.getEnvironmentProperties.
      map(Space.create).
      getOrElse(Space.empty)
    val space1 = model.getVoucherModel.
      map(_.setup(space0)).
      getOrElse(space0)
    // println(s"XXX: $space1")
    val space = space1
    space
  }

  private def _to_parameter_record(p: Request): Record = {
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
        case ReplEnd => RAISE.noReachDefect
        case ReplLine(l) =>
          val s = l.text
          val model = Model.parse(engine.context.config, s)
          val (_, r, newuniverse) = engine.run(universe, model)
          val o = r.map(x => s"${x.print}\n").mkString
          val output = StringUtils.showConsole(o, engine.context.newline, 100)
          val newstate = copy(universe = newuniverse)
          (newstate, s"$output\n$prompt")
      }
    }
  }

  def main(args: Array[String]) = {
    val env = Environment.create(args)
    val config = Config.create(env)
    val kal = Kaleidox(config, env)
    val req = spec.Request.empty
    val res = spec.Response()
    val op = spec.Operation("kaleidox", req, res)
    val call = OperationCall.create(op, args)
    // println(s"Call: ${call.request}")
    if (call.request.arguments.isEmpty || call.request.isInteractive)
      kal.repl(call)
    else
      kal.execute(call)
  }
}
