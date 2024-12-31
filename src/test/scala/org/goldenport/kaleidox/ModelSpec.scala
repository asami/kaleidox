package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.parser.LogicalSection
import org.goldenport.sexpr.SScript
import org.goldenport.kaleidox.Model._

/*
 * @since   Sep. 24, 2018
 *  version Oct. 25, 2018
 *  version Feb.  6, 2019
 *  version Apr.  6, 2019
 *  version Oct.  2, 2019
 *  version Apr.  5, 2021
 * @version Oct. 23, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ModelSpec extends WordSpec with Matchers with GivenWhenThen {
  val config = {
    Config.log.debug // trace // warn // debug
  }

  def section(title: String, content: String): LogicalSection = 
    LogicalSection.create(title, content)

  "Model" should {
//     "quote" in {
//       val s = """* main

// '
// """
//       val model = Model.parse(s)
//     }
  }
  "Division" should {
    "Division" in {
      val s = """* Identification

id division

* Environment

a.b.c="abc"

* Data

data division

* Procedure

${1 + 2 + 3}
"""
      val model = Model.parseWitoutLocation(config, s)
      val target = Model(
        config.withoutLocation,
        IdentificationDivision(section("Identification", "id division")),
        EnvironmentDivision.create(section("Environment", """a.b.c="abc"""")),
        DataDivision(section("Data", "data division")),
        Script(SScript("1 + 2 + 3"))
      )
      model.divisions(0) should be(target.divisions(0))
      model.divisions(1) should be(target.divisions(1))
      model.divisions(2) should be(target.divisions(2))
      model.divisions(3) should be(target.divisions(3))
      model should be(target)
    }
  }
}
