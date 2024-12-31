package org.goldenport.kaleidox.model.actor.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model.actor._

/*
 * @since   Dec. 23, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class ActorModelBeMatcher(exptected: ActorModel) extends BeMatcher[ActorModel] with DoxMatchResultHelper {
  def apply(actual: ActorModel): MatchResult = {
    val d = match_be_description(exptected.description, actual.description)
    val s = match_be_dox_list("sections", exptected.sections, actual.sections)
    match_result("ActorModel", d, s)
  }
}
object ActorModelBeMatcher {
  trait Matchers {
    def consequence_actor_model(p: ActorModel) = ConsequenceBeMatcher(ActorModelBeMatcher(p))
  }
}
