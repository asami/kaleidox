package org.goldenport.kaleidox.model.vision.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.goldenport.kaleidox.model.test.ModelMatchResultHelper
import org.goldenport.kaleidox.model.test.ModelMatchers
import org.goldenport.kaleidox.model.vision.VisionModel.VisionStatement

/*
 * @since   Dec. 24, 2024
 * @version Dec. 24, 2024
 * @author  ASAMI, Tomoharu
 */
case class VisionStatementBeMatcher(exptected: VisionStatement)
    extends BeMatcher[VisionStatement]
    with ModelMatchResultHelper with ModelMatchers {
  def apply(actual: VisionStatement): MatchResult = {
    ???
  }
}

object VisionStatementBeMatcher {
  trait Matchers {
    def consequence_vision_model(p: VisionStatement) = ConsequenceBeMatcher(VisionStatementBeMatcher(p))
  }

  def run(expected: VisionStatement, actual: VisionStatement): MatchResult =
    VisionStatementBeMatcher(expected)(actual)
}
