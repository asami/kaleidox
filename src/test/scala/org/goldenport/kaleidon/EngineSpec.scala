package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.sexpr._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork.interpreter.MockUnitOfWorkLogic
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Aug. 17, 2018
 * @version Sep. 30, 2018
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class EngineSpec extends WordSpec with Matchers with GivenWhenThen {
  val mocklogic = MockUnitOfWorkLogic.build(
    "http://www.yahoo.com" -> "OK"
  )
  val config = {
    val a = Config.log.debug // warn // debug
    a.withServiceLogic(mocklogic)
  }
  val context = ExecutionContext(config)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)

  def result(p: Vector[Expression]): SExpr =
    p.head match {
      case LispExpression(sexpr) => sexpr
      case m => RAISE.noReachDefect
    }

  "lisp" should {
    "typical" in {
      val s = "(+ 1 2 3)"
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(6))
    }
  }
  "token" should {
    "string" in {
      val s = "\"hello\""
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("hello"))
    }
    "url" in {
      val s = "http://www.yahoo.com"
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SHtml("OK"))
    }
  }
  "javascript" should {
    "math expression" in {
      val s = "[1 + 2 + 3]"
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(6))
    }
  }
  "jexl" should {
    "math expression" in {
      val s = "jexl[1 + 2 + 3]"
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(6))
    }
  }
  "forth" should {
    "simple" in {
      val s = """1
2
+
"""
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(3))
    }
    "fetch and go" in {
      val s = """http://www.yahoo.com
length
"""
// //div[@class='value']
      val script = Script.parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(2))
    }
  }
}
