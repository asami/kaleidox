package org.goldenport.kaleidox

import org.goldenport.log.LogContext
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.cli
import org.goldenport.cli._
import org.goldenport.sexpr.{SExpr, SMetaCommand, SString, SNil, SConsoleOutput}

/*
 * @since   Feb. 17, 2019
 *  version Feb. 24, 2019
 *  version Mar. 10, 2019
 *  version Apr. 21, 2019
 *  version Jun. 23, 2019
 * @version Jul. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait CommandPart { self: Engine =>
  protected def execute_command(ctx: ExecutionContext, u: Universe, p: SMetaCommand): UnitOfWorkFM[RWSOutput] = {
    val env: Environment = Environment(ctx.config.cliConfig, CommandPart.KaleidoxEnvironment(u))
    val r = CommandPart.engine.apply(env, p.command, p.args)
    val a = LispExpression(SConsoleOutput(r.text))
    uow_lift((Vector.empty, Vector(a), u))
  }
}

object CommandPart {
  val services = Services(
    ShowServiceClass,
    LogServiceClass,
    PropertyServiceClass,
    StackServiceClass,
    HistoryServiceClass,
    UniverseServiceClass,
    EvalServiceClass
  )
  val operations = Operations(ExitClass, VersionClass)
  val engine = cli.Engine(services, operations)

  case class KaleidoxEnvironment(
    universe: Universe
  ) extends Environment.AppEnvironment {
  }

  trait KaleidoxMethod extends Method {
    lazy val kaleidoxEnvironment: KaleidoxEnvironment = toAppEnvironment[KaleidoxEnvironment]
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
      ShowMarshallClass,
      ShowViewClass,
      ShowLiteralClass,
      ShowMuarshallClass // TODO
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
        val s = universe.current.getValue.map(_.print).getOrElse("#Empty")
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

    case object ShowViewClass extends OperationClass {
      val specification = spec.Operation.default("view")

      def operation(req: Request): Operation = ShowView
    }

    case object ShowView extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowViewMethod(OperationCall(env, ShowViewClass.specification, req, Response())).run
    }

    case class ShowViewMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        println("ShowViewMethod")
        // val hello = new JavaFxHello
        // hello.run(Array())
        Test.initAndShowGUI()
        to_response("ShowViewMethod")
      }
    }

    case object ShowMuarshallClass extends OperationClass {
      val specification = spec.Operation.default("muarshall")

      def operation(req: Request): Operation = ShowMuarshall
    }

    case object ShowMuarshall extends Operation {
      def apply(env: Environment, req: Request): Response =
        ShowMuarshallMethod(OperationCall(env, ShowMuarshallClass.specification, req, Response())).run
    }

    case class ShowMuarshallMethod(call: OperationCall) extends KaleidoxMethod {
      def execute = {
        val s = universe.current.getValue.map(_.marshall).getOrElse("")
        to_response(s)
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
        ???
      }
    }
  }
}

//
import javafx.application.Application
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.input.MouseEvent
import javafx.scene.layout.VBox
import javafx.stage.Stage

class JavaFxHello extends Application {
  def run(args: Array[String]) {
    Application.launch(args: _*)
  }

  override def start(stage: Stage) {
    val button: Button = new Button("Hello World")

    val handler = new EventHandler[MouseEvent] {
      override def handle(t: MouseEvent) {
        println("Hello!")
      }
    }

    button.setOnMouseClicked(handler)
    val vbox: VBox = new VBox(button)
    val scene: Scene = new Scene(vbox)
    stage.setScene(scene)
    stage.setTitle("Hello")
    stage.show()
  }
}

object Test {
  import javafx.application.Platform
  import javafx.embed.swing.JFXPanel
  import javafx.scene.Group
  import javafx.scene.Scene
  import javafx.scene.paint.Color
  import javafx.scene.text.Font
  import javafx.scene.text.Text
  import javafx.scene.web.WebView
  import javax.swing.JFrame
  import javax.swing.SwingUtilities

  def initAndShowGUI() {
    val frame = new JFrame("Swing and JavaFX")
    val fxPanel = new JFXPanel()
    frame.add(fxPanel)
    frame.setSize(300, 200)
    frame.setVisible(true)
//    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    Platform.runLater(new Runnable() {
      def run() {
        initFX(fxPanel)
      }
    })
  }

  def initFX(fxPanel: JFXPanel) {
    val scene = createScene()
    fxPanel.setScene(scene)
  }

  // def createScene() = {
  //   val root = new Group()
  //   val scene = new Scene(root, Color.ALICEBLUE)
  //   val text = new Text()
  //   text.setX(40)
  //   text.setY(100)
  //   text.setFont(new Font(25))
  //   text.setText("Welcome JavaFX!")

  //   root.getChildren().add(text)
  //   scene
  // }

  def createScene() = {
    val root = new Group()
    val scene = new Scene(root)
    val view = new WebView()
    view.getEngine.load("http://www.yahoo.com")
    root.getChildren().add(view)
    scene
  }
}
