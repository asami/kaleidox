package org.goldenport.kaleidox.model

import java.net.URI
import org.goldenport.parser._
import org.goldenport.kaleidox._

/*
 * @since   Apr. 18, 2021
 * @version Apr. 18, 2021
 * @author  ASAMI, Tomoharu
 */
case class LibraryHangar(
  libraries: Libraries = Libraries.empty,
  warnings: Vector[LibraryHangar.Message[WarningMessage]] = Vector.empty,
  errors: Vector[LibraryHangar.Message[ErrorMessage]] = Vector.empty
) {
  def isExists(p: Locator): Boolean =
    libraries.isExists(p) || warnings.exists(_.isMatch(p)) || errors.exists(_.isMatch(p))

  def add(uri: URI, p: Model) = {
    val locator = UriLocator(uri)
    if (libraries.isExists(locator) || libraries.isExists(p.signature))
      this
    else
      _add(locator, p)
  }

  private def _add(locator: Locator, p: Model) = {
    val main = ModelLibrary(locator, p)
    case class Z(ls: Libraries) {
      def r = {
        LibraryHangar.this.copy(
          libraries = ls,
          warnings = warnings ++ p.warnings.map(_to_message(locator, _)),
          errors = errors ++ p.errors.map(_to_message(locator, _))
        )
      }

      def +(rhs: Library) = copy(ls = ls + rhs)
    }
    p.libraries.libraries./:(Z(libraries + main))(_+_).r
  }

  private def _to_message(locator: Locator, p: WarningMessage) =
    LibraryHangar.Message(locator, p)

  private def _to_message(locator: Locator, p: ErrorMessage) =
    LibraryHangar.Message(locator, p)

  def toLibraries: Libraries = libraries
  def toErrorMessages: Vector[ErrorMessage] = errors.map(_.message)
  def toWarningMessages: Vector[WarningMessage] = warnings.map(_.message)
}

object LibraryHangar {
    case class Message[T <: ParseMessage](
      locator: Locator,
      message: T
    ) {
      def isMatch(p: Locator): Boolean = locator == p
    }

  val empty = LibraryHangar()
}
