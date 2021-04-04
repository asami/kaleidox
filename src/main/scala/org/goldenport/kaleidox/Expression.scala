package org.goldenport.kaleidox

import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Feb. 24, 2019
 *  version Mar.  5, 2019
 *  version Apr. 19, 2019
 *  version Jun. 23, 2019
 *  version Jul. 24, 2019
 * @version Apr.  3, 2021
 * @author  ASAMI, Tomoharu
 */
sealed trait Expression {
  def asSExpr: SExpr
  def resolve: Expression
  /*
   * Natural representation for data. Show as-is even large data.
   */
  def print: String

  /*
   * 1 line representation for interaction representation (e.g. REPL).
   */
  def display: String

  /*
   * Show a shortened natural representation with some information. 
   */
  def show: String

  /*
   * Show natural description of the data.
   */
  def description: Vector[String]

  /*
   * Show full description of the data.
   */
  def full: Vector[String]

  /*
   * Show pretty representation of the data.
   */
  def pretty: Vector[String]

  /*
   * Literal representation in Programming Language.
   */
  def literal: String

  /*
   * Exchange format over media like file or network.
   */
  def marshall: String
}

case class LispExpression(sexpr: SExpr) extends Expression {
  override def toString() = sexpr.toString
  def asSExpr = sexpr
  def resolve = LispExpression(sexpr.resolve)
  def print = sexpr.print
  def display = sexpr.display
  def show = sexpr.show
  def description = sexpr.description.detail
  def full = sexpr.fullDescription.detail
  def pretty = sexpr.pretty.toVector
  def literal = sexpr.literal
  def marshall = sexpr.marshall
}
