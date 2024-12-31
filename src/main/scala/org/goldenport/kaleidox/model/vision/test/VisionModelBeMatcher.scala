package org.goldenport.kaleidox.model.vision.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.goldenport.kaleidox.model.test.ModelMatchResultHelper
import org.goldenport.kaleidox.model.test.ModelMatchers
import org.goldenport.kaleidox.model.vision._

/*
 * @since   Dec. 23, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class VisionModelBeMatcher(exptected: VisionModel)
    extends BeMatcher[VisionModel]
    with ModelMatchResultHelper with ModelMatchers {
  def apply(actual: VisionModel): MatchResult = {
    val d = match_be_description(exptected.description, actual.description)
    val s = match_be_option("statement")(VisionStatementBeMatcher.run, exptected.statement, actual.statement)
    match_result("VisionModel", d, s)
  }
}

object VisionModelBeMatcher {
  trait Matchers {
    def consequence_vision_model(p: VisionModel) = ConsequenceBeMatcher(VisionModelBeMatcher(p))
  }
}
