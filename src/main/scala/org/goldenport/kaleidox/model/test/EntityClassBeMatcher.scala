package org.goldenport.kaleidox.model.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._
import org.goldenport.kaleidox.model.EntityModel.EntityClass

/*
 * @since   Dec. 25, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class EntityClassBeMatcher(exptected: EntityClass) extends BeMatcher[EntityClass] with DoxMatchResultHelper {
  def apply(actual: EntityClass): MatchResult = {
    val s = SchemaClassBeMatcher(exptected.schemaClass)(actual.schemaClass)
    val p = match_be_seq(ParentRefBeMatcher.run, exptected.parents, actual.parents)
    val pkg = match_be("package", exptected.packageName, actual.packageName)
    match_result("EntityClass", s, p, pkg)
  }
}

object EntityClassBeMatcher {
  trait Matchers {
    def consequence_entity_class(p: EntityClass) = ConsequenceBeMatcher(EntityClassBeMatcher(p))
  }

  def run(exptected: EntityClass, actual: EntityClass): MatchResult =
    EntityClassBeMatcher(exptected)(actual)
}
