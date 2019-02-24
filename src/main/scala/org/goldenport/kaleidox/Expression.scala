package org.goldenport.kaleidox

import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 * @version Feb. 24, 2019
 * @author  ASAMI, Tomoharu
 */
sealed trait Expression {
  def asSExpr: SExpr
  def resolve: Expression
  def print: String
  def show: String
  def detail: String
}

case class LispExpression(sexpr: SExpr) extends Expression {
  override def toString() = sexpr.toString
  def asSExpr = sexpr
  def resolve = LispExpression(sexpr.resolve)
  def print = sexpr.print
  def show = sexpr.show
  def detail = sexpr.detail
}
