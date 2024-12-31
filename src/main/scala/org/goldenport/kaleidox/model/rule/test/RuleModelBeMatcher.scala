package org.goldenport.kaleidox.model.rule.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.goldenport.kaleidox.model.rule._
import org.goldenport.kaleidox.model.test.ModelMatchResultHelper

/*
 * @since   Dec. 23, 2024
 * @version Dec. 29, 2024
 * @author  ASAMI, Tomoharu
 */
case class RuleModelBeMatcher(exptected: RuleModel) extends BeMatcher[RuleModel] with ModelMatchResultHelper {
  def apply(actual: RuleModel): MatchResult = {
    val d = match_be_description(exptected.description, actual.description)
    val s = match_be_dox_list("sections", exptected.sections, actual.sections)
    val e = match_be_entity_model(exptected.entities, actual.entities)
    val evt = match_be_event_model(exptected.events, actual.events)
    match_result("RuleModel", d, s)
  }
}
object RuleModelBeMatcher {
  trait Matchers {
    def consequence_rule_model(p: RuleModel) = ConsequenceBeMatcher(RuleModelBeMatcher(p))
  }
}
