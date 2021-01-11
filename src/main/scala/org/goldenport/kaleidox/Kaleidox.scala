package org.goldenport.kaleidox

import scalaz._, Scalaz._
import java.io.File
import org.goldenport.RAISE
import org.goldenport.monitor.Monitor
import org.goldenport.i18n.I18NContext
import org.goldenport.log.{LogContext, LogLevel, LogConfig}
import org.goldenport.cli._
import org.goldenport.bag.BufferBag
import org.goldenport.record.v3.Record
import org.goldenport.parser.LogicalLines
import org.goldenport.parser.ParseMessage
import org.goldenport.console.{ConsoleManager, MessageSequence, Message}
import org.goldenport.console.{StandardMessage, ErrorMessage, WarningMessage, Prompt}
import org.goldenport.kaleidox.interpreter.Interpreter
import org.goldenport.util.StringUtils

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Oct. 21, 2018
 *  version Feb. 28, 2019
 *  version Mar. 24, 2019
 *  version Apr. 18, 2019
 *  version May. 20, 2019
 *  version Jun. 23, 2019
 *  version Jul. 29, 2019
 *  version Aug. 18, 2019
 *  version Sep.  8, 2019
 *  version Oct. 27, 2019
 *  version Nov.  9, 2019
 *  version May. 30, 2020
 * @version Jan. 11, 2021
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
    val console = ConsoleManager.createColorHilight(environment)
    val repl = new ReadEvalPrintLoop(console, state, LogicalLines.Config.lisp)
    repl.execute()
    engine.epilogue()
  }

  def execute(call: OperationCall) {
    val engine = _build_world(call)
    val u = engine.universe
    engine.epilogue()
    u.getValue.map(r => println(r.print))
  }

  private def _build_world(call: OperationCall): Engine = {
    LogContext.setRootLevel(LogLevel.Info) // suppress boot sequence logging.
    val (universe, model) = _build_universe(call)
    val i18nconfig = _i18n_context(model, config)
    val logconfig = _log_config(model, config)
    val sqlcontext = config.sqlContext.
      addProperties(universe.setup.bindings).
      addProperties(universe.parameters.bindings)
    val resourcemanager = config.resourceManager // TODO
    val context = ExecutionContext(
      environment,
      config,
      i18nconfig,
      logconfig,
      config.serviceLogic,
      config.storeLogic,
      config.scriptContext,
      sqlcontext,
      resourcemanager,
      config.feature
    )
    val interpreter = Interpreter.create(context)
    val engine0 = Engine(context, universe, interpreter)
    engine0.initialize()
    val engine1 = engine0.setup(model)
    engine1.execute()
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
    val universe = Universe(
      configspace,
      setupspace,
      parameterspace,
      blackboard,
      model.errors,
      model.warnings
    )
    (universe, model)
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
    val space2 = model.getDataSet.map(_.setup(space1)).getOrElse(space1)
    val space = space2
    space
  }

  private def _i18n_context(model: Model, config: Config): I18NContext = {
    config.i18nContext // TODO model
  }

  private def _log_config(model: Model, config: Config): LogConfig = {
    val c = config.logConfig
    // TODO model : conf file, level
    val logurl = c.confFile orElse _get_implicit_log_conf_file.map(_.toURI.toURL)
    logurl match {
      case Some(l) => c.withConfFile(l)
      case None => c
    }
  }

  private def _get_implicit_log_conf_file: Option[File] = {
    def homeindir = config.homeDirectory.flatMap(_ensure_file(_, ".kaleidox/logback.xml"))
    def project = config.getProjectDirectory.flatMap(_ensure_file(_, "logback.xml"))
    def work = config.workDirectory.flatMap(_ensure_file(_, "logback.xml"))
    work orElse project orElse homeindir
  }

  private def _ensure_file(parent: File, name: String): Option[File] = {
    val a = new File(parent, name)
    if (a.exists)
      Some(a)
    else
      None
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

    val environment = engine.context.environment
    val newline = engine.context.newline
    val consoleOutputLineLength = 100

    def apply(p: ReplEvent): (IState, MessageSequence) = {
      val prompt = "kaleidox> "
      p match {
        case ReplStart =>
          val msgs = _errors_warnings_messages(universe)
          (this, msgs :+ Prompt(prompt))
        case ReplEnd => RAISE.noReachDefect
        case ReplLine(l) =>
          val s = l.text
          val model = Model.parseExpression(engine.context.config, s)
          val (_, r, newuniverse) = engine.run(universe, model)
          val o = r.map(x => s"${_output(x)}${newline}").mkString
          val output = StringUtils.printConsole(o, newline, consoleOutputLineLength)
          val newstate = copy(universe = newuniverse)
          (newstate, MessageSequence(Message(output), Prompt(prompt)))
      }
    }

    private def _errors_warnings_messages(p: Universe): MessageSequence = {
      val errors = MessageSequence.createErrorNormalized(universe.errors.map(_to_message))
      val warnings = MessageSequence.createWarningNormalized(universe.warnings.map(_to_message))
      errors + warnings
    }

    private def _to_message(p: ParseMessage): String = p.en(environment)

    // private def _normalize_newline_with_newline(p: String) =
    //   StringUtils.normalizeConsoleMessageWithTrailingNewline(newline)(p)

    private def _output(p: Expression): String = p.display // TODO customizable
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
