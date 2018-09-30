package org.goldenport.kaleidox

import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
case class Space(
  getValue: Option[Expression],
  bindings: Record
) {
  def apply(v: SExpr, b: Record): Space = apply(LispExpression(v), b)

  def apply(v: SExpr): Space = apply(LispExpression(v), bindings)

  def apply(b: Record): Space = copy(bindings = bindings.update(bindings))

  def apply(): Space = copy()

  def apply(p: Expression, bindings: Record): Space = Space(
    Some(p),
    bindings.update(bindings)
  )
}

object Space {
  val empty = Space(None, Record.empty)
}
