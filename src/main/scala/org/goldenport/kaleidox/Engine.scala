package org.goldenport.kaleidox

import scala.language.higherKinds
import scalaz.{Store => _, _}, Scalaz._
import scalaz.concurrent.Task
import org.goldenport.exception.RAISE
import org.goldenport.record.unitofwork._, UnitOfWork._
import org.goldenport.record.unitofwork.interpreter.UnitOfWorkInterpreter
import org.goldenport.record.v2.unitofwork.UnitOfWorkHelper
import org.goldenport.sexpr.SExpr
import org.goldenport.sexpr.eval.EvalContext

/*
 * @since   Aug. 11, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class Engine(
  context: ExecutionContext,
  universe: Universe,
  interpreter: UnitOfWorkInterpreter[Task]
) extends UnitOfWorkHelper {
  import UnitOfWorkReaderWriterState._

  // Reader : ExecutionContext
  // Writer : RWSWriter (currently unused)
  // State : Universe
  // A: Expression
  // B: Vector[Expression]
  type RWSWriter = Vector[String]
  type RWSA = Expression
  type RWSB = Vector[Expression]
  type RWSOutput = (RWSWriter, RWSB, Universe)

  implicit val uowMonad = new Monad[UnitOfWorkFM] {
    def point[A](a: => A): UnitOfWorkFM[A] = UnitOfWork.lift(a)
    // def ap[A, B](fa: => UnitOfWorkFM[A])(f: => UnitOfWorkFM[A => B]): UnitOfWorkFM[B] = fa.flatMap(x => f.map(f => f(x)))
    def bind[A, B](fa: UnitOfWorkFM[A])(f: A => UnitOfWorkFM[B]): UnitOfWorkFM[B] = fa.flatMap(f)
  }

  def apply(p: Model): RWSB = {
    // TODO environment and model.
    p.getScript.map(apply).getOrElse(???)
  }

  def apply(p: Script): RWSB = {
    val r = for {
      _ <- urws[ExecutionContext, RWSWriter, Universe]
      r <- execute(p)
    } yield r
    val rr: UnitOfWorkFM[RWSOutput] = r.run(context, universe)
    val (written, result, state) = runTask(rr)(interpreter).run
    result.map(_.resolve)
  }

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
      case LispExpression(sexpr) => _eval_lisp(r, s, sexpr)
      case _ => RAISE.noReachDefect
    }
  }

  private def _eval_lisp(reader: ExecutionContext, state: Universe, a: SExpr): UnitOfWorkFM[RWSOutput] = {
    val evaluator = lisp.Evaluator(reader, state)
    val newstate = evaluator.eval(a) // TODO UnitOfWorkFM
    println(s"Engine#_eval_lisp: $a => $newstate")
    val b = newstate.current.getValue.toVector
    uow_lift((Vector.empty, b, newstate))
  }
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
