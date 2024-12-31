package org.goldenport.kaleidox.model.test

import scalaz._, Scalaz._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.context.Consequence
import org.goldenport.context.test.ConsequenceBeMatcher
import org.smartdox.test.DescriptionBeMatcher
import org.smartdox.test.DoxMatchResultHelper
import org.goldenport.kaleidox.model._
import org.goldenport.kaleidox.model.EntityModel.EntityClass.ParentRef

/*
 * @since   Dec. 25, 2024
 * @version Dec. 26, 2024
 * @author  ASAMI, Tomoharu
 */
case class ParentRefBeMatcher(expected: ParentRef) extends ModelBeMatcher[ParentRef] {
  protected def apply_Match(actual: ParentRef): MatchResult =
    expected match {
      case ParentRef.Name(name) => actual match {
        case ParentRef.Name(namea) => match_condition(name == namea, "ParentRef", name, namea)
        case ParentRef.EntityKlass(ca) => match_error("ParentRef", expected, actual)
      }
      case ParentRef.EntityKlass(c) => actual match {
        case ParentRef.Name(namea) => match_error("ParentRef", expected, actual)
        case ParentRef.EntityKlass(ca) => EntityClassBeMatcher(c)(ca)
      }
    }
}

object ParentRefBeMatcher {
  trait Matchers {
    def consequence_entity_class(p: ParentRef) = ConsequenceBeMatcher(ParentRefBeMatcher(p))
  }

  def run(exptected: ParentRef, actual: ParentRef): MatchResult =
    ParentRefBeMatcher(exptected)(actual)
}
