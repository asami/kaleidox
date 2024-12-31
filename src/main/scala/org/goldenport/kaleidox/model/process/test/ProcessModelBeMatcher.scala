package org.goldenport.kaleidox.model.process.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model.process._
import org.goldenport.kaleidox.model.test._

/*
 * @since   Dec. 23, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class ProcessModelBeMatcher(exptected: ProcessModel) extends BeMatcher[ProcessModel] with DoxMatchResultHelper {
  def apply(actual: ProcessModel): MatchResult = {
    val d = match_be_description(exptected.description, actual.description)
    val s = match_be_dox_list("sections", exptected.sections, actual.sections)
    val e = EntityModelBeMatcher(exptected.entities)(actual.entities)
    val evt = EventModelBeMatcher(exptected.events)(actual.events)
    match_result("ActorModel", d, s, e, evt)
  }
}
object ProcessModelBeMatcher {
  trait Matchers {
    def consequence_process_model(p: ProcessModel) = ConsequenceBeMatcher(ProcessModelBeMatcher(p))
  }
}
