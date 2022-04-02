package org.goldenport.kaleidox.model

import org.goldenport.extension.Showable

/*
 * @since   Apr. 18, 2021
 * @version Mar. 19, 2022
 * @author  ASAMI, Tomoharu
 */
sealed trait Signature extends Showable {
  def print = toString
  def display = print
  def show = display

  def isMatch(p: Signature): Boolean
}

case object NoneSignature extends Signature {
  def isMatch(p: Signature): Boolean =false
}
