package org.goldenport.kaleidox

import org.goldenport.RAISE
import org.goldenport.record.v3._
import org.goldenport.record.util.AnyUtils
import org.goldenport.hocon.RichConfig
import org.goldenport.incident.Incident
import org.goldenport.sexpr.SExpr
import org.goldenport.sexpr.eval.IncidentSequence
import org.goldenport.kaleidox.model.{Libraries, Library}

/*
 * @since   Aug. 11, 2018
 *  version Sep. 30, 2018
 *  version Oct. 21, 2018
 *  version Mar. 24, 2019
 *  version Apr. 18, 2019
 *  version Jun.  9, 2019
 *  version Sep. 28, 2019
 *  version Apr. 18, 2021
 *  version Aug.  8, 2021
 * @version Apr. 24, 2022
 * @author  ASAMI, Tomoharu
 */
case class Space(
  getValue: Option[Expression],
  getStimulus: Option[Expression],
  getIncident: IncidentSequence,
  bindings: IRecord // , // inherit
//   importedModels: Set[Model.ImportedModel.Locator] = Set.empty
) {
  def addLibraries(libraries: Libraries): Space = 
    libraries.models./:(this)(Space.build)

  def addModel(model: Model): Space = Space.build(this, model)

  // def addModel(model: Model): Space = {
  //   val a = model.importedModels.models./:(this)(Space.build)
  //   Space.build(a, model)
  // }

  // def withImportedModel(model: Model.ImportedModel): Space =
  //   copy(importedModels = importedModels + model.locator)

  // def isDefined(model: Model.ImportedModel): Boolean = importedModels.exists(_.isMatch(model.locator))

  def next(v: SExpr, b: IRecord, s: SExpr, i: IncidentSequence): Space = next(LispExpression(v), b, LispExpression(s), i)

  // def next(v: SExpr, b: IRecord, s: SExpr, i: Vector[Incident]): Space = next(LispExpression(v), b, LispExpression(s), i)

  def next(v: SExpr, s: SExpr, i: IncidentSequence): Space = next(LispExpression(v), bindings, LispExpression(s), i)

  // def next(v: SExpr, s: SExpr, i: Vector[Incident]): Space = next(LispExpression(v), bindings, LispExpression(s), i)

  def next(b: IRecord, s: SExpr): Space = Space(
    None,
    Some(LispExpression(s)),
    IncidentSequence.empty,
    bindings.update(b)
  )

  def next(b: IRecord): Space = Space(
    None,
    None,
    IncidentSequence.empty,
    bindings.update(b)
  )

  def next(): Space = copy(None, None, IncidentSequence.empty)

  def next(p: Expression, b: IRecord, s: Expression, i: IncidentSequence): Space = Space(
    Some(p),
    Some(s),
    i,
    bindings.update(b)
  )

  // def next(p: Expression, b: IRecord, s: Expression, i: Seq[Incident]): Space = Space(
  //   Some(p),
  //   Some(s),
  //   IncidentSequence(i),
  //   bindings.update(b)
  // )

  def getBinding(p: String): Option[SExpr] = bindings.get(p).map(SExpr.create)

  def updateBindings(p: RichConfig): Space = copy(bindings = bindings + HoconRecord(p))

  def updateBindings(p: Record): Space = copy(bindings = bindings + p)

  def show: List[String] = Space.show(bindings)
}

object Space {
  val empty = Space(None, None, IncidentSequence.empty, Record.empty)

  def create(p: RichConfig): Space = Space(None, None, IncidentSequence.empty, HoconRecord(p))
  def create(ps: Seq[RichConfig]): Space = RAISE.notImplementedYetDefect
  def create(p: IRecord): Space = Space(None, None, IncidentSequence.empty, p)

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

  def build(p: Space, model: /*Model.ImportedModel*/Library): Space =
    // if (p.isDefined(model))
    //   p
    // else
    //   _build(p, model)
    _build(p, model)

  private def _build(p: Space, model: /*Model.ImportedModel*/Library): Space = {
    // val a = model.model.importedModels.models./:(p)(build)
    // build(a, model.model).withImportedModel(model)
    build(p, model.model)
  }

  // See Engine#setup
  def build(p: Space, model: Model): Space = {
    val space0 = model.getEnvironmentProperties.
      map(p.updateBindings).
      getOrElse(p)
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
}
