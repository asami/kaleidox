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
 * @version Feb.  8, 2019
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class TryoutSpec extends WordSpec with Matchers with GivenWhenThen {
  val mocklogic = MockUnitOfWorkLogic.build(
    "http://www.yahoo.com" -> "OK"
  )
  val config = {
    val a = Config.log.debug // trace // warn // debug
    a.withServiceLogic(mocklogic)
  }
  val context = ExecutionContext(config)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)
  engine.initialize()

  def result(p: Vector[Expression]): SExpr =
    p.head match {
      case LispExpression(sexpr) => sexpr
      case m => RAISE.noReachDefect
    }
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
