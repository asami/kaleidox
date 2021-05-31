package org.goldenport.kaleidox

import scalaz._, Scalaz._
import java.io.File
import org.goldenport.RAISE
import org.goldenport.monitor.Monitor
import org.goldenport.i18n.I18NContext
import org.goldenport.log.{LogContext, LogLevel, LogConfig}
import org.goldenport.trace.TraceContext
import org.goldenport.cli._
import org.goldenport.bag.BufferBag
import org.goldenport.record.v3.Record
import org.goldenport.parser.LogicalLines
import org.goldenport.parser.ParseMessage
import org.goldenport.parser.{ErrorMessage, WarningMessage}
import org.goldenport.console.{ConsoleManager, MessageSequence, Message}
import org.goldenport.statemachine.StateMachineSpace
import org.goldenport.kaleidox.interpreter.Interpreter
import org.goldenport.kaleidox.http.HttpHandle
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
 *  version Jan. 22, 2021
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 *  version Apr. 25, 2021
 * @version May. 21, 2021
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

  def http(call: OperationCall): HttpHandle = {
    val engine = _build_world(call)
    new HttpHandle(engine)
  }

  private def _build_world(call: OperationCall): Engine = {
    LogContext.setRootLevel(LogLevel.Info) // suppress boot sequence logging.
    val (universe, model) = _build_universe(call)
    val i18nconfig = _i18n_context(model, config)
    val logconfig = _log_config(model, config)
    val tracecontext = TraceContext.create()
    val statemachinespace = StateMachineSpace.create(model.takeStateMachineClasses)
    val sqlcontext = config.sqlContext.
      addProperties(universe.setup.bindings).
      addProperties(universe.parameters.bindings)
    val resourcemanager = config.resourceManager // TODO
    val context = ExecutionContext(
      environment,
      config,
      i18nconfig,
      logconfig,
      tracecontext,
      statemachinespace,
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
    val inmodel = _in_model(call)
    // val inmodels = call.request.arguments.map(_.asString).map(Model.load(config, _))
    // val model = inmodels./:(confmodel)(_+_)
    val model = confmodel + inmodel
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
      model
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

  private def _in_model(call: OperationCall): Model = {
    val args = call.argumentsAsString
    val libraries = call.asUrlList('library)
    val lib = libraries.foldMap(Model.load(config, _))
    if (args.isEmpty) {
      lib
    } else {
      val s = args.mkString(" ")
      val expr = Model.parseExpression(config, s)
      lib + expr
    }
  }

  // private def _space(model: Model) = {
  //   val space0 = model.getEnvironmentProperties.
  //     map(Space.create).
  //     getOrElse(Space.empty)
  //   val space1 = model.getVoucherModel. // obsolated
  //     map(_.setup(space0)).
  //     getOrElse(space0)
  //   val space2 = model.getSchemaModel.
  //     map(_.setup(space1)).
  //     getOrElse(space1)
  //   val space3 = model.getDataSet.map(_.setup(space2)).getOrElse(space2) // obsolated
  //   val space4 = model.getDataBag.map(_.setup(space3)).getOrElse(space3)
  //   val space = space4
  //   space
  // }

  private def _space(model: Model) = Space.empty.addModel(model)

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
          (this, msgs :+ Message.prompt(prompt))
        case ReplEnd => RAISE.noReachDefect
        case ReplLine(l) =>
          val s = l.text
          val model = Model.parseExpression(engine.context.config, s)
          val (report, r, newuniverse) = engine.run(universe, model)
          // val o = r.map(x => s"${_output(x)}${newline}").mkString
          // val output = StringUtils.printConsole(o, newline, consoleOutputLineLength)
          // val newstate = copy(universe = newuniverse)
          // (newstate, msgs + MessageSequence(Message(output), Prompt(prompt)))
          val o = _to_messages(r)
          val output = _errors_to_messages(model.errors) + _warnings_to_messages(model.warnings) + report.messages + o
          val newstate = copy(universe = newuniverse)
          (newstate, output :+ Message.prompt(prompt))
      }
    }

    private def _errors_warnings_messages(p: Universe): MessageSequence = {
      val errors = MessageSequence.createErrorNormalized(universe.errors.map(_to_message))
      val warnings = MessageSequence.createWarningNormalized(universe.warnings.map(_to_message))
      errors + warnings
    }

    private def _to_message(p: ParseMessage): String = p.en(environment)

    private def _errors_to_messages(p: Seq[ErrorMessage]) = 
      MessageSequence.createErrorNormalized(p.map(_to_message))

    private def _warnings_to_messages(p: Seq[WarningMessage]) = 
      MessageSequence.createWarningNormalized(p.map(_to_message))

    // private def _normalize_newline_with_newline(p: String) =
    //   StringUtils.normalizeConsoleMessageWithTrailingNewline(newline)(p)

    private def _to_messages(p: Vector[Expression]): MessageSequence =
      MessageSequence(p.map(_to_message))

    private def _to_message(p: Expression): Message = {
      import org.goldenport.sexpr._
      def s = StringUtils.printConsole(_output(p), newline, consoleOutputLineLength)
      p.asSExpr match {
        case m: SAtom => _keyword(s)
        case m: SKeyword => _keyword(s)
        case m: SNumber => _number(s)
        case m: SRational => _number(s)
        case m: SComplex => _number(s)
        case m: SBoolean => _number(s)
        case m: SRange => _number(s)
        case m: SInterval => _number(s)
        case m: SString => _string(s)
        case m: SList => _sexpr(s)
        case m: SLambda => _special(s)
        case m: SError => _error(s)
        case m: SConsoleOutput => _console(m.output)
        case m: SBinary => _special(s)
        case m: SI18NString => _special(s)
        case m: SI18NTemplate => _special(s)
        case m: SRegex => _special(s)
        case m: SClob => _special(s)
        case m: SBlob => _special(s)
        case m: SRecord => _record(s)
        case m: STable => _record(s)
        case m: SVector => _record(s)
        case m: SMatrix => _record(s)
        case m: SDataFrame => _record(s)
        case m: SLxsv => _record(s)
        case m: SUrl => _special(s)
        case m: SUrn => _special(s)
        case m: SUri => _special(s)
        case m: SExpression => _special(s)
        case m: SScript => _special(s)
        case m: SProcess => _control(s)
        case m: SWindow => _control(s)
        case m: SBean => _special(s)
        case m: SXml => _special(s)
        case m: SHtml => _special(s)
        case m: SXPath => _special(s)
        case m: SXsl => _special(s)
        case m: SPug => _special(s)
        case m: SJson => _special(s)
        case m: SDateTime => _datetime(s)
        case m: SLocalDateTime => _datetime(s)
        case m: SLocalDate => _datetime(s)
        case m: SLocalTime => _datetime(s)
        case m: SMonthDay => _datetime(s)
        case m: SDateTimeInterval => _datetime(s)
        case m: SDuration => _datetime(s)
        case m: SPeriod => _datetime(s)
        case m: SMoney => _value(s)
        case m: SPercent => _value(s)
        case m: SUnit => _value(s)
        case m: SChart => _special(s)
        case m: SChartSpace => _special(s)
        case m: SChartSeries => _special(s)
        case m: SMute => _special(s)
        case m: SFuture => _special(s)
        case m: SLazy => _special(s)
        case m: SLazyFuture => _special(s)
        case m: SWait => _special(s)
        case m: SFutureWait => _special(s)
        case m => _unknown(s)
      }
    }

    private def _keyword(p: String): Message = Message(p)

    private def _number(p: String): Message = Message.blue(p)

    private def _string(p: String): Message = Message.black(p)

    private def _datetime(p: String): Message = Message.cyan(p)

    private def _value(p: String): Message = Message.cyan(p)

    private def _sexpr(p: String): Message = Message.magenta(p)

    private def _record(p: String): Message = Message.white(p)

    private def _control(p: String): Message = Message.green(p).withBlink()

    private def _error(p: String): Message = Message.red(p).withUnderline()

    private def _special(p: String): Message = Message.underline(p)

    private def _unknown(p: String): Message = Message.yellow(p).withUnderline()

    private def _console(m: Message): Message = m

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
