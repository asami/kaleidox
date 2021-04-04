package org.goldenport.kaleidox

import org.goldenport.RAISE
import org.goldenport.record.v3._
import org.goldenport.record.util.AnyUtils
import org.goldenport.hocon.RichConfig
import org.goldenport.incident.Incident
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar. 24, 2019
 *  version Apr. 18, 2019
 *  version Jun.  9, 2019
 *  version Sep. 28, 2019
 * @version Apr.  4, 2021
 * @author  ASAMI, Tomoharu
 */
case class Space(
  getValue: Option[Expression],
  getStimulus: Option[Expression],
  getIncident: Option[Incident],
  bindings: IRecord // inherit
) {
  def addModel(model: Model): Space = {
    val space0 = model.getEnvironmentProperties.
      map(updateBindings).
      getOrElse(this)
    val space1 = model.getVoucherModel. // obsolated
      map(_.setup(space0)).
      getOrElse(space0)
    val space2 = model.getSchemaModel.
      map(_.setup(space1)).
      getOrElse(space1)
    val space3 = model.getDataSet.map(_.setup(space2)).getOrElse(space2) // obsolated
    val space4 = model.getDataBag.map(_.setup(space3)).getOrElse(space3)
    val space = space4
    space
  }

  def next(v: SExpr, b: IRecord, s: SExpr, i: Option[Incident]): Space = next(LispExpression(v), b, LispExpression(s), i)

  def next(v: SExpr, s: SExpr, i: Option[Incident]): Space = next(LispExpression(v), bindings, LispExpression(s), i)

  def next(b: IRecord, s: SExpr): Space = Space(
    None,
    Some(LispExpression(s)),
    None,
    bindings.update(b)
  )

  def next(b: IRecord): Space = Space(
    None,
    None,
    None,
    bindings.update(b)
  )

  def next(): Space = copy(None, None, None)

  def next(p: Expression, b: IRecord, s: Expression, i: Option[Incident]): Space = Space(
    Some(p),
    Some(s),
    i,
    bindings.update(b)
  )

  def getBinding(p: String): Option[SExpr] = bindings.get(p).map(SExpr.create)

  def updateBindings(p: RichConfig): Space = copy(bindings = bindings + HoconRecord(p))

  def updateBindings(p: Record): Space = copy(bindings = bindings + p)

  def show: List[String] = Space.show(bindings)
}

object Space {
  val empty = Space(None, None, None, Record.empty)

  def create(p: RichConfig): Space = Space(None, None, None, HoconRecord(p))
  def create(ps: Seq[RichConfig]): Space = RAISE.notImplementedYetDefect
  def create(p: IRecord): Space = Space(None, None, None, p)

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
