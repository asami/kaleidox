package org.goldenport.kaleidox

import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.log.LogContext
import org.goldenport.context.Conclusion
import org.goldenport.console.Message
import org.goldenport.console.MessageSequence
import org.goldenport.collection.NonEmptyVector
import org.goldenport.record.v3.ITable
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.cli
import org.goldenport.cli._
import org.goldenport.parser.CommandParser
import org.goldenport.javafx.{JavaFXWindow => LibJavaFXWindow, HtmlWindow}
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{LispFunction, FunctionSpecification}
import org.goldenport.record.util.AnyUtils

/*
 * @since   Feb. 17, 2019
 *  version Feb. 24, 2019
 *  version Mar. 10, 2019
 *  version Apr. 21, 2019
 *  version Jun. 23, 2019
 *  version Jul. 24, 2019
 *  version Aug. 20, 2019
 *  version Oct. 15, 2019
 *  version Nov. 28, 2019
 *  version Feb. 23, 2020
 *  version May. 30, 2020
 *  version Jan. 24, 2021
 *  version Feb. 25, 2021
 *  version Mar. 28, 2021
 * @version Apr.  5, 2021
 * @author  ASAMI, Tomoharu
 */
trait CommandPart { self: Engine =>
  protected def execute_command(ctx: ExecutionContext, u: Universe, p: SMetaCommand): UnitOfWorkFM[RWSOutput] = {
    // val env: Environment = Environment(
    //   ctx.monitor,
    //   ctx.config.cliConfig,
    //   CommandPart.KaleidoxEnvironment(ctx, u, CommandPart.engine.commandParser)
    // )
    val env = ctx.environment.copy(
      appEnvironment = CommandPart.KaleidoxEnvironment(ctx, u, CommandPart.engine.commandParser)
    )
    val r = CommandPart.engine.apply(env, p.command, p.args)
    val ru = r match {
      case m: CommandPart.UniverseResponse => m.universe
      case _ => u
    }
    val a = LispExpression(SConsoleOutput(Message(r.text)))
    uow_lift((EvalReport.create(ctx), Vector(a), ru))
  }
}

object CommandPart {
  val services = Services(
    ShowServiceClass,
    ViewServiceClass,
    LogServiceClass,
    RecorderServiceClass,
    PropertyServiceClass,
    StackServiceClass,
    HistoryServiceClass,
    TraceServiceClass,
    UniverseServiceClass,
    EvalServiceClass,
    LoadServiceClass,
    SaveServiceClass,
    HelpServiceClass,
    ManualServiceClass
  )
  val operations = Operations(ExitClass, VersionClass)
  val engine = cli.Engine.terse(services, operations)

  case class UniverseResponse(universe: Universe) extends ApplicationResponse {
  }

  case class KaleidoxEnvironment(
    context: ExecutionContext,
    universe: Universe,
    commandParser: CommandParser[cli.Engine.Candidate]
  ) extends Environment.AppEnvironment {
    private lazy val _evaluator = lisp.Evaluator(context, universe)

    lazy val functions: CommandParser[LispFunction] = _evaluator.functionParser
  }

  trait KaleidoxMethod extends Method {
    lazy val kaleidoxEnvironment: KaleidoxEnvironment = toAppEnvironment[KaleidoxEnvironment]
    def config = kaleidoxEnvironment.context.config
    def universe = kaleidoxEnvironment.universe
  }

  case object ExitClass extends OperationClass {
    def specification: spec.Operation = spec.Operation(
      "exit",
      spec.Request.empty,
      spec.Response.empty
    )
    def operation(req: Request): Operation = Exit
  }

  case object Exit extends Operation {
    def apply(env: Environment, req: Request): Response = {
      System.exit(0)
      Response.empty
    }
  }

  case object VersionClass extends OperationClass {
    def specification: spec.Operation = spec.Operation(
      "version",
      spec.Request.empty,
      spec.Response.string
    )
    def operation(req: Request): Operation = Version
  }

  case object Version extends Operation {
    def apply(env: Environment, req: Request): Response =
      Response(s"${BuildInfo.version} (${BuildInfo.build})")
  }

  case object ShowServiceClass extends ServiceClass {
    def name = "show"
    def defaultOperation = Some(ShowClass)
    def operations = Operations(
      ShowClass,
      ShowPrintClass,
      ShowDisplayClass,
      ShowDescriptionClass,
      ShowFullClass,
      ShowLiteralClass,
      ShowMarshallClass,
      ShowPrettyClass,
      ShowViewClass
    )

    // def specification: spec.Service = spec.Service(
    //   "show",
    //   Some(spec.Operation.default),
    //   List(
    //     Show.specification,
    //     ShowShow.specification,
    //     ShowDescription.specification,
    //     ShowView.specification
    //   )
    // )

    // def operation(req: Request): Operation = req.operation match {
    //   case spec.Operation.OP_DEFAULT => Show
    // }

    case object ShowClass extends OperationClass {
      val specification = spec.Operation.default("show")

      def operation(req: Request): Operation = Show
    }

    case object Show extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowMethod(OperationCall(env, ShowClass.specification, req, Response())).run
    }

    case class ShowMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        // println("ShowMethod")
        val s = universe.current.getValue.map(_.show).getOrElse("#Empty")
        to_response(s)
      }
    }

    case object ShowPrintClass extends OperationClass {
      val specification = spec.Operation.default("print")

      def operation(req: Request): Operation = ShowPrint
    }

    case object ShowPrint extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowPrintMethod(OperationCall(env, ShowPrintClass.specification, req, Response())).run
    }

    case class ShowPrintMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        // println("ShowPrintMethod")
        val s = universe.current.getValue.map(_.print).getOrElse("")
        to_response(s)
      }
    }

    case object ShowDisplayClass extends OperationClass {
      val specification = spec.Operation.default("display")

      def operation(req: Request): Operation = ShowDisplay
    }

    case object ShowDisplay extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowDisplayMethod(OperationCall(env, ShowDisplayClass.specification, req, Response())).run
    }

    case class ShowDisplayMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val s = universe.current.getValue.map(_.display).getOrElse("#Empty")
        to_response(s)
      }
    }

    case object ShowDescriptionClass extends OperationClass {
      val specification = spec.Operation.default("description")

      def operation(req: Request): Operation = ShowDescription
    }

    case object ShowDescription extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowDescriptionMethod(OperationCall(env, ShowDescriptionClass.specification, req, Response())).run
    }

    case class ShowDescriptionMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val a = universe.current.getValue.map(_.description).getOrElse(Nil)
        val s = build_lines_string(a)
        to_response(s)
      }
    }

    case object ShowFullClass extends OperationClass {
      val specification = spec.Operation.default("full")

      def operation(req: Request): Operation = ShowFull
    }

    case object ShowFull extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowFullMethod(OperationCall(env, ShowFullClass.specification, req, Response())).run
    }

    case class ShowFullMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val a = universe.current.getValue.map(_.full).getOrElse(Nil)
        val s = build_lines_string(a)
        to_response(s)
      }
    }

    case object ShowMarshallClass extends OperationClass {
      val specification = spec.Operation.default("marshall")

      def operation(req: Request): Operation = ShowMarshall
    }

    case object ShowMarshall extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowMarshallMethod(OperationCall(env, ShowMarshallClass.specification, req, Response())).run
    }

    case class ShowMarshallMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val s = universe.current.getValue.map(_.marshall).getOrElse("")
        to_response(s)
      }
    }

    case object ShowLiteralClass extends OperationClass {
      val specification = spec.Operation.default("literal")

      def operation(req: Request): Operation = ShowLiteral
    }

    case object ShowLiteral extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowLiteralMethod(OperationCall(env, ShowLiteralClass.specification, req, Response())).run
    }

    case class ShowLiteralMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val s = universe.current.getValue.map(_.literal).getOrElse("#Empty")
        to_response(s)
      }
    }

    case object ShowPrettyClass extends OperationClass {
      val specification = spec.Operation.default("pretty")

      def operation(req: Request): Operation = ShowPretty
    }

    case object ShowPretty extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowPrettyMethod(OperationCall(env, ShowPrettyClass.specification, req, Response())).run
    }

    case class ShowPrettyMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val a = universe.current.getValue.map(_.pretty).getOrElse(Nil)
        val s = build_lines_string(a)
        to_response(s)
      }
    }

    case object ShowViewClass extends OperationClass {
      val specification = spec.Operation.default("view")

      def operation(req: Request): Operation = ShowView
    }

    case object ShowView extends Operation {
      def apply(env: Environment, req: Request): Response =
        ViewServiceClass.ViewMethod(OperationCall(env, ShowViewClass.specification, req, Response())).run
    }
  }

  case object ViewServiceClass extends ServiceClass {
    import org.smartdox._

    def name = "view"
    def defaultOperation = Some(ViewClass)
    def operations = Operations(
      ViewClass
    )

    case object ViewClass extends OperationClass {
      val specification = spec.Operation.default("view")

      def operation(req: Request): Operation = View
    }

    case object View extends Operation {
      def apply(env: Environment, req: Request): Response =
        ViewMethod(OperationCall(env, ViewClass.specification, req, Response())).run
    }

    case class ViewMethod(call: OperationCall) extends KaleidoxMethod {
      import org.smartdox.Dox

      def execute = {
        universe.current.getValue.map(_.asSExpr).map {
          case m: STable => _table(m)
          case m: SRecord => _record(m)
          case m: SXml => _xml(m)
          case m: SHtml => _html(m)
          case m: SJson => _json(m)
          case m: SError => _error(m)
          case m: SBlob => _blob(m)
          case m: SClob => _clob(m)
          case m => _sexpr(m)
        }.map(_view_html).getOrElse {
          to_response("ShowViewMethod")
        }
        // println("ShowViewMethod")
        // // val hello = new JavaFxHello
        // // hello.run(Array())
        // Test.initAndShowGUI()
        // to_response("ShowViewMethod")
      }

      // private def _table(p: STable) = ITable.HtmlBuilder().text(p.table)

      private def _table(p: STable): String = {
        val t = p.table
        val head: Option[List[String]] = t.head.map(_.names.map(_.text))
        val doxtable = head.map { h =>
          val data = t.data
          val builder = Table.Builder.headerString(h)
          for (y <- 0 until data.height) {
            val xs = for (x <- 0 until data.width) yield {
              data.get(x, y) match {
                case Some(cell) => cell.text
                case None => ""
              }
            }
            builder.appendString(xs)
          }
          builder.apply()
        }.getOrElse(RAISE.noReachDefect)
        _to_html(doxtable)
      }

      private def _record(p: SRecord): String = {
        import org.smartdox.Table
        val head: List[String] = List("Key", "Value") // TODO
        val builder = Table.Builder.headerString(head)
        for (key <- p.record.keys) {
          val data = p.record.get(key).map(AnyUtils.toString).getOrElse("")
          val kv = List(key, data)
          builder.appendString(kv)
        }
        _to_html(builder.apply())
      }

      private def _xml(p: SXml): String = {
        _to_html_program(p, "xml")
      }

      private def _html(p: SHtml): String = {
        _to_html_program(p, "html")
      }

      private def _json(p: SJson): String = {
        _to_html_program(p, "json")
      }

      private def _error(p: SError): String = {
        _to_html_program(p, "error")
      }

      private def _blob(p: SBlob): String = {
        _to_html_program(p, "blob")
      }

      private def _clob(p: SClob): String = {
        _to_html_program(p, "clob")
      }

      private def _sexpr(p: SExpr): String = {
        _to_html_program(p, "lisp")
      }

      private def _to_html_program(p: SExpr, lang: String) = {
        val s = p.pretty
        val prog = Program(s, "program" -> lang)
        _to_html(prog)
      }

      private def _to_html(p: Dox): String = {
        import org.goldenport.parser._
        import org.smartdox.generator.Context
        import org.smartdox.transformers.Dox2HtmlTransformer

        val ctx = Context.create()
        val tx = Dox2HtmlTransformer(ctx)
        tx.transform(p) match {
          case m: ParseSuccess[_] => m.ast
          case m: EmptyParseResult[_] => RAISE.noReachDefect
          case m: ParseFailure[_] => RAISE.noReachDefect
        }
      }

      private def _view_html(p: String): Response = {
        val c = LibJavaFXWindow.Config("HTML View", 640, 480)
        val a = new HtmlWindow(c, p) // TODO manage
        a.start()
        to_response("View")
      }
    }
  }

  case object LogServiceClass extends ServiceClass {
    def name = "log"
    def defaultOperation = Some(LogLevelClass)
    def operations = Operations(
      LogLevelClass,
      LogDebugClass,
      LogTraceClass
    )

    case object LogLevelClass extends OperationClass {
      val specification = spec.Operation.default("level")

      def operation(req: Request): Operation = LogLevel
    }

    case object LogLevel extends Operation {
      def apply(env: Environment, req: Request): Response =
        LogLevelMethod(OperationCall(env, LogLevelClass.specification, req, Response())).run
    }

    case class LogLevelMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = head_string_option.map(_store).getOrElse(_fetch)

      private def _fetch = {
        val s = LogContext.takeRootLevel.name
        to_response(s)
      }

      private def _store(p: String) = {
        val level = org.goldenport.log.LogLevel.apply(p)
        LogContext.setRootLevel(level)
        _fetch
      }
    }

    case object LogDebugClass extends OperationClass {
      val specification = spec.Operation.default("debug")

      def operation(req: Request): Operation = LogDebug
    }

    case object LogDebug extends Operation {
      def apply(env: Environment, req: Request): Response =
        LogDebugMethod(OperationCall(env, LogDebugClass.specification, req, Response())).run
    }

    case class LogDebugMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        LogContext.setRootLevel(org.goldenport.log.LogLevel.Debug)
        val s = LogContext.takeRootLevel.name
        to_response(s)
      }
    }

    case object LogTraceClass extends OperationClass {
      val specification = spec.Operation.default("trace")

      def operation(req: Request): Operation = LogTrace
    }

    case object LogTrace extends Operation {
      def apply(env: Environment, req: Request): Response =
        LogTraceMethod(OperationCall(env, LogTraceClass.specification, req, Response())).run
    }

    case class LogTraceMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        LogContext.setRootLevel(org.goldenport.log.LogLevel.Trace)
        val s = LogContext.takeRootLevel.name
        to_response(s)
      }
    }
  }

  case object RecorderServiceClass extends ServiceClass {
    def name = "recorder"
    def defaultOperation = Some(RecorderReportClass)
    def operations = Operations(
      RecorderReportClass
    )

    case object RecorderReportClass extends OperationClass {
      val specification = spec.Operation.default("report")

      def operation(req: Request): Operation = RecorderReport
    }

    case object RecorderReport extends Operation {
      def apply(env: Environment, req: Request): Response =
        RecorderReportMethod(OperationCall(env, RecorderReportClass.specification, req, Response())).run
    }

    case class RecorderReportMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val recorder = call.environment.recorder
        val file = RAISE.notImplementedYetDefect
        recorder.setReportFile(file)
        RAISE.notImplementedYetDefect
      }
    }
  }

  case object PropertyServiceClass extends ServiceClass {
    def name = "property"
    def defaultOperation = Some(PropertyClass)
    def operations = Operations(
      PropertyClass
    )
  }

  case object PropertyClass extends OperationClass {
    val specification = spec.Operation.default("property")

    def operation(req: Request): Operation = Property
  }

  case object Property extends Operation {
    def apply(env: Environment, req: Request): Response =
      PropertyMethod(OperationCall(env, PropertyClass.specification, req, Response())).run
  }

  case class PropertyMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      val props = universe.current.bindings
      val s = props.toString // TODO
      to_response(s)
    }
  }

  case object StackServiceClass extends ServiceClass {
    def name = "stack"
    def defaultOperation = Some(StackClass)
    def operations = Operations(
      StackClass
    )
  }

  case object StackClass extends OperationClass {
    val specification = spec.Operation.default("stack")

    def operation(req: Request): Operation = Stack
  }

  case object Stack extends Operation {
    def apply(env: Environment, req: Request): Response =
      StackMethod(OperationCall(env, StackClass.specification, req, Response())).run
  }

  case class StackMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      val xs = universe.stack.map(_.getValueSExpr.getOrElse(SNil))
      val s = build_lines_string_with_number(xs.map(_.display))
      to_response(s)
    }
  }

  case object HistoryServiceClass extends ServiceClass {
    def name = "history"
    def defaultOperation = Some(HistoryClass)
    def operations = Operations(
      HistoryClass,
      CommandHistoryClass,
      IncidentHistoryClass
    )
  }

  case object HistoryClass extends OperationClass {
    val specification = spec.Operation.default("history")

    def operation(req: Request): Operation = History
  }

  case object History extends Operation {
    def apply(env: Environment, req: Request): Response =
      HistoryMethod(OperationCall(env, HistoryClass.specification, req, Response())).run
  }

  case class HistoryMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      val xs = universe.history.map(_.getValueSExpr.getOrElse(SNil))
      val s = build_lines_string_with_number(xs.map(_.display))
      to_response(s)
    }
  }

  case object CommandHistoryClass extends OperationClass {
    val specification = spec.Operation.default("command")

    def operation(req: Request): Operation = CommandHistory
  }

  case object CommandHistory extends Operation {
    def apply(env: Environment, req: Request): Response =
      CommandHistoryMethod(OperationCall(env, HistoryClass.specification, req, Response())).run
  }

  case class CommandHistoryMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      val xs = universe.history.map(_.getStimulusSExpr.getOrElse(SNil))
      val s = build_lines_string_with_number(xs.map(_.display))
      to_response(s)
    }
  }

  case object IncidentHistoryClass extends OperationClass {
    val specification = spec.Operation.default("incident")

    def operation(req: Request): Operation = IncidentHistory
  }

  case object IncidentHistory extends Operation {
    def apply(env: Environment, req: Request): Response =
      IncidentHistoryMethod(OperationCall(env, HistoryClass.specification, req, Response())).run
  }

  case class IncidentHistoryMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      val xs = universe.history.map(_.getIncident).map {
        case Some(s) => s.print
        case None => "N/A"
      }
      val s = build_lines_string_with_number(xs)
      to_response(s)
    }
  }

  case object TraceServiceClass extends ServiceClass {
    def name = "trace"
    def defaultOperation = Some(TraceClass)
    def operations = Operations(
      TraceClass,
      HistoryClass
    )

    case object TraceClass extends OperationClass {
      val specification = spec.Operation.default("trace")

      def operation(req: Request): Operation = Trace
    }

    case object Trace extends Operation {
      def apply(env: Environment, req: Request): Response =
        TraceMethod(OperationCall(env, TraceClass.specification, req, Response())).run
    }

    case class TraceMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val f: Universe.HistorySlot => String = _show
        val xs = universe.history
        val s = xs.lastOption.map(f).getOrElse("")
        to_response(s)
      }
    }

    case object HistoryClass extends OperationClass {
      val specification = spec.Operation.default("history")

      def operation(req: Request): Operation = History
    }

    case object History extends Operation {
      def apply(env: Environment, req: Request): Response =
        HistoryMethod(OperationCall(env, HistoryClass.specification, req, Response())).run
    }

    case class HistoryMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val xs = universe.history
        val s = build_lines_string_with_number(xs.map(_display))
        to_response(s)
      }
    }

    private def _print(p: Universe.HistorySlot): String = _print(p.conclusion)

    private def _print(p: Conclusion): String = {
      // TODO
      org.goldenport.trace.Trace.Printer.print(p.trace)
    }

    private def _display(p: Universe.HistorySlot): String = _display(p.conclusion)

    private def _display(p: Conclusion): String = {
      // TODO
      org.goldenport.trace.Trace.Printer.display(p.trace)
    }

    private def _show(p: Universe.HistorySlot): String = _show(p.conclusion)

    private def _show(p: Conclusion): String = {
      // TODO
      org.goldenport.trace.Trace.Printer.show(p.trace)
    }
  }

  case object UniverseServiceClass extends ServiceClass {
    def name = "universe"
    def defaultOperation = Some(UniverseClass)
    def operations = Operations(
      UniverseClass,
      ConfigClass,
      SetupClass,
      ParameterClass,
      BindingsClass,
      StackClass,
      HistoryClass
    )

    case object UniverseClass extends OperationClass {
      val specification = spec.Operation.default("universe")

      def operation(req: Request): Operation = Universe
    }

    case object Universe extends Operation {
      def apply(env: Environment, req: Request): Response =
        UniverseMethod(OperationCall(env, UniverseClass.specification, req, Response())).run
    }

    case class UniverseMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val props = universe.bindings
        val xs = Space.show(props)
        val s = build_lines_string(xs)
        to_response(s)
      }
    }

    case object ConfigClass extends OperationClass {
      val specification = spec.Operation.default("config")

      def operation(req: Request): Operation = Config
    }

    case object Config extends Operation {
      def apply(env: Environment, req: Request): Response =
        ConfigMethod(OperationCall(env, ConfigClass.specification, req, Response())).run
    }

    case class ConfigMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val xs = universe.config.show
        val s = build_lines_string(xs)
        to_response(s)
      }
    }

    case object SetupClass extends OperationClass {
      val specification = spec.Operation.default("setup")

      def operation(req: Request): Operation = Setup
    }

    case object Setup extends Operation {
      def apply(env: Environment, req: Request): Response =
        SetupMethod(OperationCall(env, SetupClass.specification, req, Response())).run
    }

    case class SetupMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val xs = universe.setup.show
        val s = build_lines_string(xs)
        to_response(s)
      }
    }

    case object ParameterClass extends OperationClass {
      val specification = spec.Operation.default("parameter")

      def operation(req: Request): Operation = Parameter
    }

    case object Parameter extends Operation {
      def apply(env: Environment, req: Request): Response =
        ParameterMethod(OperationCall(env, ParameterClass.specification, req, Response())).run
    }

    case class ParameterMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val xs = universe.parameters.show
        val s = build_lines_string(xs)
        to_response(s)
      }
    }

    case object BindingsClass extends OperationClass {
      val specification = spec.Operation.default("bindings")

      def operation(req: Request): Operation = Bindings
    }

    case object Bindings extends Operation {
      def apply(env: Environment, req: Request): Response =
        BindingsMethod(OperationCall(env, BindingsClass.specification, req, Response())).run
    }

    case class BindingsMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val xs = Space.show(universe.current.bindings)
        val s = build_lines_string(xs)
        to_response(s)
      }
    }
  }

  case object EvalServiceClass extends ServiceClass {
    def name = "eval"
    def defaultOperation = Some(EvalClass)
    def operations = Operations(
      EvalClass
    )

    case object EvalClass extends OperationClass {
      val specification = spec.Operation.default("eval")

      def operation(req: Request): Operation = Eval
    }

    case object Eval extends Operation {
      def apply(env: Environment, req: Request): Response =
        EvalMethod(OperationCall(env, EvalClass.specification, req, Response())).run
    }

    case class EvalMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        RAISE.notImplementedYetDefect
      }
    }
  }

  case object LoadServiceClass extends ServiceClass {
    def name = "load"
    def defaultOperation = Some(LoadClass)
    def operations = Operations(
      LoadClass
    )

    case object LoadClass extends OperationClass {
      val specification = spec.Operation.default("load")

      def operation(req: Request): Operation = Load
    }

    case object Load extends Operation {
      def apply(env: Environment, req: Request): Response =
        LoadMethod(OperationCall(env, LoadClass.specification, req, Response())).run
    }

    case class LoadMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val url = call.request.arg1Url
        val model = Model.load(config, url)
        val ru = universe.addSetupModel(model)
        UniverseResponse(ru)
      }
    }
  }

  case object SaveServiceClass extends ServiceClass {
    def name = "save"
    def defaultOperation = Some(SaveClass)
    def operations = Operations(
      SaveClass
    )

    case object SaveClass extends OperationClass {
      val specification = spec.Operation.default("save")

      def operation(req: Request): Operation = Save
    }

    case object Save extends Operation {
      def apply(env: Environment, req: Request): Response =
        SaveMethod(OperationCall(env, SaveClass.specification, req, Response())).run
    }

    case class SaveMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        RAISE.notImplementedYetDefect
      }
    }
  }

  case object HelpServiceClass extends ServiceClass {
    def name = "help"
    def defaultOperation = Some(HelpClass)
    def operations = Operations(
      HelpClass
    )

    case object HelpClass extends OperationClass {
      val specification = spec.Operation.default("help")

      def operation(req: Request): Operation = Help
    }

    case object Help extends Operation {
      def apply(env: Environment, req: Request): Response =
        HelpMethod(OperationCall(env, HelpClass.specification, req, Response())).run
    }

    case class HelpMethod(call: OperationCall) extends KaleidoxMethod {
      import CommandParser._
      import cli.Engine._

      def execute = call.request.arguments.headOption.map(_.asString).
        map(_help).
        getOrElse(_introduction)

      private def _introduction() = {
        val s = """# : history
? : stack
! : command

% : format
. : expression"""
        to_response_lines_string(s)
      }

      private def _help(p: String) = {
        Strings.totokens(p, ":") match {
          case Nil => _introduction
          case x :: Nil =>
            val parser = kaleidoxEnvironment.commandParser
            parser(x) match {
              case _: NotFound[_] => _notfound(p)
              case Found(c) => _found(c)
              case Candidates(cs) => _candidates(cs)
            }
          case x :: xs =>
            RAISE.notImplementedYetDefect
        }
      }

      private def _notfound(name: String) = {
        val s = s"Command not found: $name"
        to_response_lines_string(s)
      }

      private def _found(c: Candidate) = {
        val s = c match {
          case ServiceCandidate(srv) => s"${srv}" // TODO
          case OperationCandidate(op) => s"${op}" // TODO
        }
        to_response_lines_string(s)
      }

      private def _candidates(cs: NonEmptyVector[Slot[Candidate]]) = {
        val s = cs.vector.map(_.name).mkString(" ") // TODO
        to_response_lines_string(s)
      }
    }
  }

  case object ManualServiceClass extends ServiceClass {
    def name = "manual"
    def defaultOperation = Some(ManualClass)
    def operations = Operations(
      ManualClass
    )

    case object ManualClass extends OperationClass {
      val specification = spec.Operation.default("manual")

      def operation(req: Request): Operation = Manual
    }

    case object Manual extends Operation {
      def apply(env: Environment, req: Request): Response =
        ManualMethod(OperationCall(env, ManualClass.specification, req, Response())).run
    }

    case class ManualMethod(call: OperationCall) extends KaleidoxMethod {
      import CommandParser._

      def execute = call.request.arguments.headOption.
        map(x => _man(x.asString)).
        getOrElse(_not_specified)

      private def _man(name: String) = {
        kaleidoxEnvironment.functions(name) match {
          case NotFound() => _not_found(name)
          case Found(c) => _found(c.specification)
          case Candidates(cs) => _ambiguous(name, cs.map(_.command.specification))
        }
      }

      private def _found(p: FunctionSpecification) = {
        val s = _spec_to_man(p)
        to_response_lines_string(s)
      }

      private def _not_found(name: String) = {
        val s = "not found" // TODO
        to_response_lines_string(s)
      }

      private def _ambiguous(name: String, ps: NonEmptyVector[FunctionSpecification]) = {
        val s = s"""Ambiguos command '$name': ${ps.vector.map(_.name).mkString(", ")}"""
        to_response_lines_string(s)
      }

      private def _not_specified = {
        val s = "Missing parameter" // TODO
        to_response_lines_string(s)
      }

      private def _spec_to_man(p: FunctionSpecification): String = {
        p.name // TODO
      }
    }
  }
}
