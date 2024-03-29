package org.goldenport.kaleidox

import scala.language.higherKinds
import scalaz.{Store => _, _}, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.exception.RAISE
import org.goldenport.log._
import org.goldenport.console.MessageSequence
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.record.unitofwork.interpreter.UnitOfWorkInterpreter
import org.goldenport.record.v2.unitofwork.UnitOfWorkHelper
import org.goldenport.record.v2.Schema
import org.goldenport.sexpr._
import org.goldenport.sexpr.eval.{EvalContext, LispBinding}

/*
 * @since   Aug. 11, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Feb. 25, 2019
 *  version Mar.  8, 2019
 *  version May. 21, 2019
 *  version Jul. 16, 2019
 *  version Oct. 27, 2019
 *  version Dec.  7, 2019
 *  version Jan. 12, 2021
 *  version Feb. 26, 2021
 *  version Mar. 28, 2021
 *  version Apr. 25, 2021
 *  version May. 14, 2021
 *  version Aug. 29, 2021
 *  version Sep. 20, 2021
 *  version Nov. 28, 2022
 *  version Dec. 31, 2022
 * @version Jan. 22, 2023
 * @author  ASAMI, Tomoharu
 */
case class Engine(
  context: ExecutionContext,
  universe: Universe,
  interpreter: UnitOfWorkInterpreter[Task],
  model: Option[Model] = None
) extends UnitOfWorkHelper with CommandPart {
  import UnitOfWorkReaderWriterState._

  // Reader : ExecutionContext
  // Writer : RWSWriter (currently unused)
  // State : Universe
  // A: Expression
  // B: Vector[Expression]
  type RWSWriter = EvalReport // MessageSequence
  type RWSA = Expression
  type RWSB = Vector[Expression]
  type RWSOutput = (RWSWriter, RWSB, Universe)

//  lazy val context = _context.withEngine(this)

  implicit val uowMonad = new Monad[UnitOfWorkFM] {
    def point[A](a: => A): UnitOfWorkFM[A] = UnitOfWork.lift(a)
    // def ap[A, B](fa: => UnitOfWorkFM[A])(f: => UnitOfWorkFM[A => B]): UnitOfWorkFM[B] = fa.flatMap(x => f.map(f => f(x)))
    def bind[A, B](fa: UnitOfWorkFM[A])(f: A => UnitOfWorkFM[B]): UnitOfWorkFM[B] = fa.flatMap(f)
  }

  def initialize(): Engine = {
    LispBinding.initialize()
    val logurl = context.logConfig.confFile
    val level = context.logConfig.level
    LogContext.init(logurl, level)
    this
  }

  // See Space#build
  def setup(p: Model): Engine = {
    _setup_store(p)
    _setup_entity(p)
    _setup_model_prologue(p)
  }

  private def _setup_store(p: Model): Unit = {
    // p.getVoucherModel.foreach { vm =>
    //   vm.classes.foreach {
    //     case (name, x) => context.feature.store.define(Symbol(name), x.schema)
    //   }
    // }
    // p.getSchemaModel.foreach { model =>
    //   model.classes.foreach {
    //     case (name, x) => context.feature.store.define(Symbol(name), x.schema)
    //   }
    // }
    p.getDataStore.foreach { model =>
      val schemas = p.getSchemaModel
      model.classes.foreach {
        case (name, x) =>
          val schema: Schema = x.schema match {
            case Right(r) => schemas.flatMap(_.getSchema(r)) getOrElse {
              RAISE.notImplementedYetDefect
            }
            case Left(l) => l
          }
          context.feature.store.define(Symbol(name), x.table, schema)
          x.data.foreach(x => context.feature.store.setup(Symbol(name), STable(x)))
      }
    }
  }

  private def _setup_entity(p: Model): Unit =
    p.getEntityModel.foreach { model =>
      model.classes.foreach {
        case (name, x) => context.feature.entity.defineCollection(Symbol(name), x)
      }
    }

  private def _setup_model_prologue(p: Model): Engine = {
    val (written, result, state) = run(universe, p.getPrologue(context.config))
    copy(model = Some(p), universe = state)
  }

  def execute(): Engine = model.
    map { x =>
      val (written, result, state) = run(universe, x.getWholeScript)
      copy(universe = state)
    }.getOrElse(this)

  def epilogue(): Engine = model.
    map { x =>
      val (written, result, state) = run(universe, x.getEpilogue(context.config))
      copy(universe = state)
    }.getOrElse(this)

  // def apply(p: Model): RWSB = {
  //   // TODO environment and model.
  //   p.getScript.map(apply).getOrElse(RAISE.noReachDefect)
  // }

  // for test
  def applySExpr(p: Script): SExpr = apply(p).head.asSExpr

  // for test
  def apply(p: Script): RWSB = {
    val (written, result, state) = run(universe, p)
    result.map(_.resolve)
  }

  def applyModelScript(model: Model, p: String): SExpr = {
    val s = Script.parse(context.config, p)
    applyModelScript(model, s)
  }

  def applyModelScript(model: Model, p: SExpr): SExpr = {
    applyModelScript(model, Script(p))
  }

  def applyModelScript(model: Model, p: Script): SExpr = {
    val u = universe.addSetupModel(model)
    val (written, result, state) = run(u, p)
    val rs = result.map(_.resolve)
    rs.head.asSExpr
  }

  def run(pstate: Universe, p: Model): RWSOutput = {
    val state = p.getEnvironmentProperties match {
      case Some(s) => pstate.next(s, context.traceContext.toHandle)
      case None => pstate
    }
    p.getWholeScript.map(run(state, _)).getOrElse((EvalReport.create(_new_context), Vector.empty, state))
  }

  def run(state: Universe, p: Option[Script]): RWSOutput =
    p.map(run(state, _)).getOrElse((EvalReport.create(_new_context), Vector.empty, state))

  def run(state: Universe, p: Script): RWSOutput = {
    val r = for {
      _ <- urws[ExecutionContext, RWSWriter, Universe]
      r <- execute(p)
    } yield r
    val rr: UnitOfWorkFM[RWSOutput] = r.run(_new_context, state)
    runTask(rr)(interpreter).run
  }

  def run(ctx: ExecutionContext, state: Universe, p: Script): RWSOutput = {
    val r = for {
      _ <- urws[ExecutionContext, RWSWriter, Universe]
      r <- execute(p)
    } yield r
    val rr: UnitOfWorkFM[RWSOutput] = r.run(ctx, state)
    runTask(rr)(interpreter).run
  }

  private def _new_context = context.newContext(this)

  private def execute(p: Script): ReaderWriterStateT[UnitOfWorkFM, ExecutionContext, RWSWriter, Universe, RWSB] = {
    val c: ReaderWriterStateT[UnitOfWorkFM, ExecutionContext, RWSWriter, Universe, RWSB] = urwso[ExecutionContext, RWSWriter, Universe, RWSB]
    p.expressions./:(c)((z, x) =>
      for {
        _ <- z
        r <- _eval(x)
      } yield r
    )
  }

  private def _eval(
    p: Expression
  )(implicit M: Functor[UnitOfWorkFM], W: Monoid[RWSWriter]): ReaderWriterStateT[UnitOfWorkFM, ExecutionContext, RWSWriter, Universe, RWSB] =
    ReaderWriterStateT { (r, s) =>
      M.map(_eval(r, s, p))(identity)
    }

  private def _eval(r: ExecutionContext, s: Universe, p: Expression): UnitOfWorkFM[RWSOutput] = {
    p match {
      case LispExpression(sexpr) => sexpr match {
        case m: SMetaCommand => _execute_command(r, s, m)
        case _ => _eval_lisp(r, s, sexpr)
      }
      case _ => RAISE.noReachDefect
    }
  }

  private def _execute_command(ctx: ExecutionContext, u: Universe, p: SMetaCommand): UnitOfWorkFM[RWSOutput] =
    execute_command(ctx, u, p)

  private def _eval_lisp(reader: ExecutionContext, state: Universe, p: SExpr): UnitOfWorkFM[RWSOutput] = {
    val evaluator = lisp.Evaluator(reader, state)
    // val sexpr = evaluator.normalize(_normalize(p))
    val sexpr = p // _normalize(p)
    // println(s"Engine#_eval_lisp: $p => $sexpr")
    val newstate = evaluator.evalScript(sexpr)
    val b = newstate.getValue.toVector
    uow_lift((EvalReport.create(reader), b, newstate))
  }

  // private def _normalize(p: SExpr): SExpr = context.promotion(p).
  //   map(x => SList(SAtom(x), p)).
  //   getOrElse(p)
}

object Engine {
}

object UnitOfWorkReaderWriterState extends UnitOfWorkHelper {
  def urws_init[A, W, S](
    a: A, w: W, s: S
  )(implicit M: Functor[UnitOfWorkFM], W: Monoid[W]): ReaderWriterStateT[UnitOfWorkFM, A, W, S, Unit] = ReaderWriterStateT(
    (r, s) => M.map(uow_unit)(b => (W.zero, b, s))
  )

  def rwst[M[_], A, W, S, B](f: A => M[B])(implicit M: Functor[M], W: Monoid[W]): ReaderWriterStateT[M, A, W, S, B] = ReaderWriterStateT(
    (r, s) => M.map(f(r))(b => (W.zero, b, s))
  )

  def urws[A, W, S](implicit M: Functor[UnitOfWorkFM], W: Monoid[W]): ReaderWriterStateT[UnitOfWorkFM, A, W, S, Unit] = ReaderWriterStateT(
    (r, s) => M.map(uow_unit)(b => (W.zero, b, s))
  )

  def urwso[A, W, S, B](implicit M: Functor[UnitOfWorkFM], W: Monoid[W], B: Monoid[B]): ReaderWriterStateT[UnitOfWorkFM, A, W, S, B] = ReaderWriterStateT(
    (r, s) => M.map(uow_lift(B.zero))(b => (W.zero, b, s))
  )

  def urws_store_get[A, W, S](store: Store, id: String)(implicit M: Functor[UnitOfWorkFM], W: Monoid[W]): ReaderWriterStateT[UnitOfWorkFM, A, W, S, GetResult] = ReaderWriterStateT(
    (r, s) => M.map(store_get(store, id))(b => (W.zero, b, s))
  )
}
