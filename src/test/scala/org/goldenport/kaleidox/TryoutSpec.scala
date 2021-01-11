package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.goldenport.RAISE
import org.goldenport.sexpr._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork.interpreter.MockUnitOfWorkLogic
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Jan.  2, 2019
 *  version Feb. 25, 2019
 *  version Oct.  2, 2019
 * @version Nov. 20, 2020
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  val mocklogic = MockUnitOfWorkLogic.build(
    "http://www.yahoo.com" -> "OK"
  )
  val monitor = ???
  val config = {
    val a = Config.log.trace // trace // warn // debug
    a.withServiceLogic(mocklogic)
  }
  val context = ExecutionContext(monitor, config)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)
  engine.initialize()

  def result(p: Vector[Expression]): SExpr =
    p.head match {
      case LispExpression(sexpr) => sexpr
      case m => RAISE.noReachDefect
    }

  // "token" should {
  //   "url" in {
  //     val s = "http://www.yahoo.com"
  //     val script = Script.parse(s)
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
