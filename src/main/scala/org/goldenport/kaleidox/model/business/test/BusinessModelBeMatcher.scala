package org.goldenport.kaleidox.model.business.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model.business._
import org.goldenport.kaleidox.model.test.EntityModelBeMatcher
import org.goldenport.kaleidox.model.test.EventModelBeMatcher
import org.goldenport.kaleidox.model.vision.test.VisionModelBeMatcher
import org.goldenport.kaleidox.model.actor.test.ActorModelBeMatcher
import org.goldenport.kaleidox.model.process.test.ProcessModelBeMatcher
import org.goldenport.kaleidox.model.rule.test.RuleModelBeMatcher

/*
 * @since   Dec.  2, 2024
 * @version Dec. 23, 2024
 * @author  ASAMI, Tomoharu
 */
case class BusinessModelBeMatcher(exptected: BusinessModel) extends BeMatcher[BusinessModel] with DoxMatchResultHelper {
  def apply(actual: BusinessModel): MatchResult = {
    val d = DescriptionBeMatcher(exptected.description)(actual.description)
    val s = match_be_dox_list("sections", exptected.sections, actual.sections)
    val v = VisionModelBeMatcher(exptected.vision)(actual.vision)
    val a = ActorModelBeMatcher(exptected.actor)(actual.actor)
    val p = ProcessModelBeMatcher(exptected.process)(actual.process)
    val e = EntityModelBeMatcher(exptected.entity)(actual.entity)
    val evt = EventModelBeMatcher(exptected.event)(actual.event)
    val rule = RuleModelBeMatcher(exptected.rule)(actual.rule)
    match_result("BusinessModel", d, s, v, a, p, e, evt, rule)
  }
}
object BusinessModelBeMatcher {
  trait Matchers {
    def consequence_business_model(p: BusinessModel) = ConsequenceBeMatcher(BusinessModelBeMatcher(p))
  }
}
