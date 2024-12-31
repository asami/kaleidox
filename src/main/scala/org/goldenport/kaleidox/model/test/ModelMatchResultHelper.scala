package org.goldenport.kaleidox.model.test

import org.scalatest.matchers.MatchResult
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._

/*
 * @since   Dec. 22, 2024
 * @version Dec. 29, 2024
 * @author  ASAMI, Tomoharu
 */
trait ModelMatchResultHelper extends DoxMatchResultHelper {
  protected final def match_be_entity_model(
    exptected: EntityModel,
    actual: EntityModel
  ): MatchResult = EntityModelBeMatcher(exptected)(actual)

  protected final def match_be_event_model(
    exptected: EventModel,
    actual: EventModel
  ): MatchResult = EventModelBeMatcher(exptected)(actual)
}
