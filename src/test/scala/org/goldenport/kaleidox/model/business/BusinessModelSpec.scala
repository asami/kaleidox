package org.goldenport.kaleidox.model.business

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.goldenport.parser.LogicalSection
import org.goldenport.sexpr.SScript
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.Model._
import org.goldenport.kaleidox.model.EntityModel
import org.goldenport.kaleidox.model.EventModel

/*
 * @since   Nov.  2, 2024
 *  version Nov.  7, 2024
 * @version Dec. 22, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class BusinessModelSpec extends WordSpec with Matchers with GivenWhenThen with test.BusinessModelMatchers {
  val config = {
    Config.log.debug // trace // warn // debug
  }

  def section(title: String, content: String): LogicalSection = 
    LogicalSection.create(title, content)

  "BusinessModel" should {
    "plain" which {
      "plain" ignore {
        val s = """# Identification

scope=business
"""
        val model = Model.parseWitoutLocation(config, s)
        val target = Model(
          config.withoutLocation,
          IdentificationDivision(section("Identification", "id division"))
        )
        model should be(target)
      }
    }
    "vision" which {
      "plain" ignore {
        val s = """# Identification

scope=business

# Vision

This is our vision.
"""
        val model = Model.parseWitoutLocation(config, s)
        val target = Model(
          config.withoutLocation,
          IdentificationDivision(section("Identification", "id division"))
        )
        model should be(target)
      }
      "business" in {
        val s = """# Identification

scope=business

# Vision

This is our vision.
"""
        val model = BusinessModel.parseWitoutLocation(config, s)
        val target = BusinessModel.empty(config)
        model should be (consequence_business_model(target))
      }
    }
  }
}
