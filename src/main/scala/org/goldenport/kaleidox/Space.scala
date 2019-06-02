package org.goldenport.kaleidox

import org.goldenport.record.v3._
import org.goldenport.record.util.AnyUtils
import org.goldenport.hocon.RichConfig
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar. 24, 2019
 * @version Apr. 18, 2019
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

  def updateBindings(p: Record): Space = copy(bindings = bindings + p)

  def show: List[String] = Space.show(bindings)
}

object Space {
  val empty = Space(None, Record.empty)

  def create(p: RichConfig): Space = Space(None, HoconRecord(p))
  def create(ps: Seq[RichConfig]): Space = ???
  def create(p: IRecord): Space = Space(None, p)

  def show(p: IRecord): List[String] = p.fields.sortBy(_.key.name).map(x => s"${x.key.name}: ${_print(x.value)}").toList

  private def _print(p: FieldValue): String = p match {
    case SingleValue(v) => _print_value(v)
    case MultipleValue(vs) => vs.map(_print_value).mkString("[", ",", "]")
    case EmptyValue => "#EMPTY"
  }

  private def _print_value(p: Any): String = p match {
    case m: SExpr => m.print
    case m => AnyUtils.toString(m)
  }
}
