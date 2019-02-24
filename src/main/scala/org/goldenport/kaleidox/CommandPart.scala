package org.goldenport.kaleidox

import org.goldenport.log.LogContext
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.cli
import org.goldenport.cli._
import org.goldenport.sexpr.{SExpr, SMetaCommand, SString}

/*
 * @since   Feb. 17, 2019
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
trait CommandPart { self: Engine =>
  protected def execute_command(ctx: ExecutionContext, u: Universe, p: SMetaCommand): UnitOfWorkFM[RWSOutput] = {
    val env: Environment = Environment(ctx.config.cliConfig, CommandPart.KaleidoxEnvironment(u))
    val r = CommandPart.engine.apply(env, p.command, p.args)
    val a = LispExpression(SString(r.text))
    uow_lift((Vector.empty, Vector(a), u))
  }
}

object CommandPart {
  val services = Services(ShowServiceClass, LogServiceClass)
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
      ShowShowClass,
      ShowDetailClass,
      ShowVisualClass
    )

    // def specification: spec.Service = spec.Service(
    //   "show",
    //   Some(spec.Operation.default),
    //   List(
    //     Show.specification,
    //     ShowShow.specification,
    //     ShowDetail.specification,
    //     ShowVisual.specification
    //   )
    // )

    // def operation(req: Request): Operation = req.operation match {
    //   case spec.Operation.OP_DEFAULT => Show
    // }
  }

  case object ShowClass extends OperationClass {
    val specification = spec.Operation.default("print")

    def operation(req: Request): Operation = Show
  }

  case object Show extends Operation {
    def apply(env: Environment, req: Request): Response =
      ShowMethod(OperationCall(env, ShowClass.specification, req, Response())).run
  }

  case class ShowMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      // println("ShowMethod")
      val s = universe.current.getValue.map(_.print).getOrElse("#Empty")
      to_response(s)
    }
  }

  case object ShowShowClass extends OperationClass {
    val specification = spec.Operation.default("show")

    def operation(req: Request): Operation = ShowShow
  }

  case object ShowShow extends Operation {
    def apply(env: Environment, req: Request): Response =
      ShowShowMethod(OperationCall(env, ShowShowClass.specification, req, Response())).run
  }

  case class ShowShowMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      // println("ShowShowMethod")
      val s = universe.current.getValue.map(_.show).getOrElse("#Empty")
      to_response(s)
    }
  }

  case object ShowDetailClass extends OperationClass {
    val specification = spec.Operation.default("detail")

    def operation(req: Request): Operation = ShowDetail
  }

  case object ShowDetail extends Operation {
    def apply(env: Environment, req: Request): Response =
      ShowDetailMethod(OperationCall(env, ShowDetailClass.specification, req, Response())).run
  }

  case class ShowDetailMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      // println("ShowDetailMethod")
      val s = universe.current.getValue.map(_.detail).getOrElse("#Empty")
      to_response(s)
    }
  }

  case object ShowVisualClass extends OperationClass {
    val specification = spec.Operation.default("visual")

    def operation(req: Request): Operation = ShowVisual
  }

  case object ShowVisual extends Operation {
    def apply(env: Environment, req: Request): Response =
      ShowVisualMethod(OperationCall(env, ShowVisualClass.specification, req, Response())).run
  }

  case class ShowVisualMethod(call: OperationCall) extends KaleidoxMethod {
    def execute = {
      // println("ShowVisualMethod")
      val s = universe.current.getValue.map(_.show).getOrElse("#Empty") // TODO
      to_response(s)
    }
  }

  case object LogServiceClass extends ServiceClass {
    def name = "log"
    def defaultOperation = Some(LogLevelClass)
    def operations = Operations(
      LogLevelClass
    )
  }

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
}
