package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.GivenWhenThen
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitRunner
import org.goldenport.parser.LogicalSection
import org.smartdox.Description
import org.goldenport.sexpr.SScript
import org.goldenport.kaleidox.Model._
import org.goldenport.kaleidox.model.DataTypeModel
import org.goldenport.record.v2.{XInt, XString}

/*
 * @since   Sep. 24, 2018
 *  version Oct. 25, 2018
 *  version Feb.  6, 2019
 *  version Apr.  6, 2019
 *  version Oct.  2, 2019
 *  version Apr.  5, 2021
 *  version Oct. 23, 2024
 *  version Feb.  8, 2025
 * @version Jul.  9, 2026
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ModelSpec extends AnyWordSpec with Matchers with GivenWhenThen {
  val config = {
    Config.log.debug // trace // warn // debug
  }

  def section(title: String, content: String): LogicalSection = 
    LogicalSection.createOrg(title, content)

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
  "DataTypeModel" should {
    "parse scalar and complex datatype definitions" in {
      Given("a CML model with scalar and complex DATATYPE definitions")
      val source = """# DATATYPE

## ExhibitionDate

### ATTRIBUTE

| name  | type   | multiplicity |
|-------+--------+--------------|
| value | string | 1            |

## DisplayPeriod

### ATTRIBUTE

| name  | type   | multiplicity |
|-------+--------+--------------|
| start | string | 1            |
| score | int    | 1            |
"""

      When("Kaleidox parses the DATATYPE model")
      val model = Model.parseWitoutLocation(config, source)
      val datatypes = model.takeDataTypeModel

      Then("a single value datatype becomes a scalar plain datatype")
      val exhibitiondate = datatypes.classes.get("ExhibitionDate").getOrElse(fail("ExhibitionDate datatype missing"))
      exhibitiondate match {
        case m: DataTypeModel.DataTypeClass.Plain =>
          m.name shouldBe "ExhibitionDate"
          m.datatype shouldBe XString
          m.packageName shouldBe "domain"
        case other => fail(s"ExhibitionDate should be plain datatype: $other")
      }

      And("a multi-attribute datatype remains a complex datatype")
      val displayperiod = datatypes.classes.get("DisplayPeriod").getOrElse(fail("DisplayPeriod datatype missing"))
      displayperiod match {
        case m: DataTypeModel.DataTypeClass.Complex =>
          m.constitutes.keySet.toVector shouldBe Vector("start", "score")
          m.constitutes("start") match {
            case x: DataTypeModel.DataTypeClass.Plain => x.datatype shouldBe XString
            case other => fail(s"start should be plain datatype: $other")
          }
          m.constitutes("score") match {
            case x: DataTypeModel.DataTypeClass.Plain => x.datatype shouldBe XInt
            case other => fail(s"score should be plain datatype: $other")
          }
        case other => fail(s"DisplayPeriod should be complex datatype: $other")
      }
    }
  }

}
