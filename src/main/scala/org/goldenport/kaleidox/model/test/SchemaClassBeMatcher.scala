package org.goldenport.kaleidox.model.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass

/*
 * @since   Dec. 25, 2024
 * @version Dec. 26, 2024
 * @author  ASAMI, Tomoharu
 */
case class SchemaClassBeMatcher(exptected: SchemaClass) extends BeMatcher[SchemaClass] with DoxMatchResultHelper {
  def apply(actual: SchemaClass): MatchResult = {
    // val s = SchemaClassBeMatcher(exptected.schemaClass)(actual.schemaClass)
    // val p = match_be_option(ParentRefBeMatcher.run, exptected.parents, actual.parents)
    // val pkg = match_be("package", exptected.packageName, actual.packageName)
    ???
      // match_result("SchemaClass", s, p, pkg)
  }
}
object SchemaClassBeMatcher {
  trait Matchers {
    def consequence_entity_class(p: SchemaClass) = ConsequenceBeMatcher(SchemaClassBeMatcher(p))
  }
}
