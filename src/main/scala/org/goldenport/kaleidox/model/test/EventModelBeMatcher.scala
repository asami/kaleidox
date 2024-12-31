package org.goldenport.kaleidox.model.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.goldenport.event._
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._

/*
 * @since   Dec. 23, 2024
 * @version Dec. 29, 2024
 * @author  ASAMI, Tomoharu
 */
case class EventModelBeMatcher(expected: EventModel) extends ModelBeMatcher[EventModel] {
  protected def apply_Match(actual: EventModel): MatchResult = {
    val d = match_be_description(expected.description, actual.description)
    val c = match_be_rule(expected.rule, actual.rule)
    match_result("EventModel", d, c)
  }

  protected def match_be_rule(expected: EventRule, actual: EventRule) = {
    match_be_map(event_class_matcher, expected.classes, actual.classes)
  }

  protected def event_class_matcher(expected: EventClazz, actual: EventClazz) = {
    ???
  }
}

object EventModelBeMatcher {
  trait Matchers {
    def consequence_event_model(p: EventModel) = ConsequenceBeMatcher(EventModelBeMatcher(p))
  }
}
