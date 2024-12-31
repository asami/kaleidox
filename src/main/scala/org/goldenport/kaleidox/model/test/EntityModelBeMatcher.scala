package org.goldenport.kaleidox.model.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._

/*
 * @since   Dec. 23, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class EntityModelBeMatcher(exptected: EntityModel) extends BeMatcher[EntityModel] with DoxMatchResultHelper {
  def apply(actual: EntityModel): MatchResult = {
    val d = match_be_description(exptected.description, actual.description)
    val c = match_be_map(EntityClassBeMatcher.run, exptected.classes, actual.classes)
    match_result("EntityModel", d, c)
  }
}
object EntityModelBeMatcher {
  trait Matchers {
    def consequence_entity_model(p: EntityModel) = ConsequenceBeMatcher(EntityModelBeMatcher(p))
  }
}
