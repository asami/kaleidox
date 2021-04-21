package org.goldenport.kaleidox.model

import java.net.URI
import org.goldenport.extension.Showable

/*
 * @since   Apr. 18, 2021
 * @version Apr. 18, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Locator extends Showable {
  def print = toString
  def display = print
  def show = display
  def embed = show

  def uri: URI
}

object Locator {
  def create(p: String): Locator = UriLocator(new URI(p)) // TODO
}

case class UriLocator(uri: URI) extends Locator {
}
