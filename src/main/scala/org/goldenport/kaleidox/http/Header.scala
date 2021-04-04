package org.goldenport.kaleidox.http

/*
 * @since   Mar. 24, 2021
 * @version Mar. 24, 2021
 * @author  ASAMI, Tomoharu
 */
case class Header(key: String, value: String) {
  def encodedValue = value // TODO
}
