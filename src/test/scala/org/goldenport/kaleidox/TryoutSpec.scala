package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers._
import scala.util.matching.Regex
import org.goldenport.RAISE
import org.goldenport.log.LogLevel
import org.goldenport.sexpr._
import org.goldenport.record.v3._
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Jan.  2, 2019
 *  version Feb. 25, 2019
 *  version Oct.  2, 2019
 *  version Nov. 20, 2020
 *  version Jan. 17, 2021
 *  version Sep.  6, 2024
 * @version Oct. 23, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen with SpecEnvironment {
  import LiteralSpec.matchers._

  override def logLevel = LogLevel.Trace

  "Division" should {
    import org.goldenport.parser.LogicalSection
    import org.goldenport.kaleidox.Model._
    val config = {
      Config.log.debug // trace // warn // debug
    }
    def section(title: String, content: String): LogicalSection =
      LogicalSection.create(title, content)
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
//      model.config should be(config)
      model.config should be(target.config)
      model.divisions(0) should be(target.divisions(0))
      model.divisions(1) should be(target.divisions(1))
      model.divisions(2) should be(target.divisions(2))
      model.divisions(3) should be(target.divisions(3))
      model.libraries should be(target.libraries)
      model should be(target)
    }
  }

  // "token" should {
  //   "url" in {
  //     val s = "http://www.yahoo.com"
  //     val script = parse(s)
  //     val r = engine.applySExpr(script)
  //     println(s"XXX: $r")
  //     println(s"XXX: ${r.asString}")
  //     println(s"XXX: ${r.display}")
  //     println(s"XXX: ${r.print}")
  //     println(s"XXX: ${r.show}")
  //     r should be(SHtml("OK"))
  //   }
  // }
//   "forth" should {
//     "fetch and go" in {
//       val s = """http://www.yahoo.com
// length
// """
// // //div[@class='value']
//       val script = Script.parse(s)
//       val r = engine.apply(script)
//       result(r) should be(SNumber(2))
//     }
//   }
  // "xpath" should {
//     "html" in {
//       val s = """path-get /html/div[@id='city'] <html><div id="user">taro</div><div id="city">yokohama</div></html>"""
//       val script = Script.parse(s)
//       val r = engine.apply(script)
//       result(r) should be(SString("yokohama"))
//     }
//     "not found" in {
//       val s = """path-get /html/div[id='city'] <html><div id="user">taro</div><div id="city">yokohama</div></html>"""
//       val script = Script.parse(s)
//       val r = engine.apply(script)
//       result(r) shouldBe a [SError]
//     }
    // "record" in {
    //   val s = """path-get xpath"@city" record"name:taro\tcity:yokohama""""
    //   val script = Script.parse(s)
    //   val r = engine.apply(script)
    //   result(r) should be(SString("yokohama"))
    // }
    // "record 2" in {
    //   val s = """path-get @city record"name:taro\tcity:yokohama""""
    //   val script = Script.parse(s)
    //   val r = engine.apply(script)
    //   result(r) should be(SString("yokohama"))
    // }
  // }
}
