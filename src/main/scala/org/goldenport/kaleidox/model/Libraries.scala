package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.extension.Showable
import org.goldenport.kaleidox._

/*
 * @since   Apr. 18, 2021
 * @version Apr. 18, 2021
 * @author  ASAMI, Tomoharu
 */
case class Libraries(
  libraries: Vector[Library] = Vector.empty
) extends Showable {
  def print = toString
  def display = print
  def show = display
  def embed = show

  def isExists(p: Locator) = libraries.exists(_.isMatch(p))
  def isExists(p: Signature) = libraries.exists(_.isMatch(p))

  def +(rhs: Libraries): Libraries = {
    case class Z(ls: Vector[Library]) {
      def r = Libraries(ls)

      def +(rhs: Library) = {
        ls match {
          case Vector() => copy(ls = ls :+ rhs)
          case Vector(x) => copy(ls = _go(Vector.empty, x, Vector.empty, rhs))
          case ms: Vector[_] => copy(ls = _go(Vector.empty, ms.head, ms.tail, rhs))
        }
      }

      @annotation.tailrec
      private def _go(prev: Vector[Library], o: Library, next: Vector[Library], p: Library): Vector[Library] =
        o.getResolved(p) match {
          case Some(s) => (prev :+ s) ++ next
          case None => next.headOption match {
            case Some(h) => _go(prev :+ o, h, next.tail, p)
            case None => prev :+ o :+ p
          }
        }
    }
    rhs.libraries./:(Z(libraries))(_+_).r
  }

  def +(p: Library): Libraries = this.+(Libraries(p))

  def models: Vector[Model] = libraries.map(_.model)

  def getScript: Option[Script] = models.flatMap(_.getScript).concatenate.toOption
}

object Libraries {
  implicit object LibrariesMonoid extends Monoid[Libraries] {
    def zero = Libraries.empty
    def append(lhs: Libraries, rhs: => Libraries) = lhs + rhs
  }

  val empty = Libraries()

  def apply(p: Library, ps: Library*): Libraries = Libraries(p +: ps)
  def apply(ps: Seq[Library]): Libraries = Libraries(ps.toVector)
}
