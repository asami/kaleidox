package org.goldenport.kaleidox

import org.goldenport.record.v3.{IRecord, Record, HoconRecord}
import org.goldenport.util.HoconUtils.RichConfig
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 30, 2018
 * @version Oct. 21, 2018
 * @author  ASAMI, Tomoharu
 */
case class Space(
  getValue: Option[Expression],
  bindings: IRecord
) {
  def apply(v: SExpr, b: IRecord): Space = apply(LispExpression(v), b)

  def apply(v: SExpr): Space = apply(LispExpression(v), bindings)

  def apply(b: IRecord): Space = copy(bindings = bindings.update(bindings))

  def apply(): Space = copy()

  def apply(p: Expression, bindings: IRecord): Space = Space(
    Some(p),
    bindings.update(bindings)
  )

  def getBinding(p: String): Option[SExpr] = bindings.get(p).map(SExpr.create)
}

object Space {
  val empty = Space(None, Record.empty)

  def create(p: RichConfig): Space = Space(None, HoconRecord(p))
  def create(ps: Seq[RichConfig]): Space = ???
  def create(p: IRecord): Space = Space(None, p)
}
