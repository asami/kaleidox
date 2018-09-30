package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.parser.LogicalSection
import org.goldenport.sexpr.SScript
import org.goldenport.kaleidox.Model._

/*
 * @since   Sep. 24, 2018
 * @version Sep. 24, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ModelSpec extends WordSpec with Matchers with GivenWhenThen {
  def section(title: String, content: String): LogicalSection = 
    LogicalSection.create(title, content)

  "a" should {
    "b" in {
      val s = """* Identification

id division

* Environment

env division

* Data

data division

* Procedure

[1 + 2 + 3]
"""
      val model = Model.parse(s)
      model should be(Model(
        IdentificationDivision(section("Identification", "id division")),
        EnvironmentDivision(section("Environment", "env division")),
        DataDivision(section("Data", "data division")),
        Script(SScript("1 + 2 + 3"))
      ))
    }
  }
}
