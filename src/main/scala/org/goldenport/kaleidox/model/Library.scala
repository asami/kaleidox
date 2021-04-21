package org.goldenport.kaleidox.model

import org.goldenport.extension.Showable
import org.goldenport.kaleidox._

/*
 * @since   Apr. 18, 2021
 * @version Apr. 18, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Library extends Showable {
  def print = toString
  def display = print
  def show = display
  def embed = show

  def locator: Locator

  def isMatch(p: Locator) = p == locator
  def isMatch(p: Signature) = signature.isMatch(p)

  def signature: Signature
  def model: Model

  def getResolved(p: Library): Option[Library]
}

object Library {
}

case class ModelLibrary(
  locator: Locator,
  model: Model,
  usedBy: Vector[Signature] = Vector.empty // TODO
) extends Library {
  def signature = model.signature
  def getResolved(p: Library): Option[Library] = None // TODO
}
