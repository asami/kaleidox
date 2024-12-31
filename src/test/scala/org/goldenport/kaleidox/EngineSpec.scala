package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.cli.Environment
import org.goldenport.sexpr._
import org.goldenport.record.v2._
import org.goldenport.record.unitofwork.interpreter.MockUnitOfWorkLogic
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Aug. 17, 2018
 *  version Sep. 30, 2018
 *  version Oct. 25, 2018
 *  version Dec. 31, 2018
 *  version Jan.  1, 2019
 *  version Feb. 25, 2019
 *  version Oct.  2, 2019
 *  version Nov. 20, 2020
 *  version Jan. 17, 2021
 *  version Feb. 13, 2021
 * @version Oct. 23, 2024
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class EngineSpec extends WordSpec with Matchers with GivenWhenThen {
  val mocklogic = MockUnitOfWorkLogic.build(
    "http://www.yahoo.com" -> "OK"
  )
  val env = Environment.create()
  val config = {
    val a = Config.log.debug // trace // warn // debug
    a.withServiceLogic(mocklogic)
  }
  val context = ExecutionContext(env, config)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)
  engine.initialize()

  def parse(p: String): Script = Script.parse(config, p)

  def result(p: Vector[Expression]): SExpr =
    p.head match {
      case LispExpression(sexpr) => sexpr
      case m => RAISE.noReachDefect
    }

  "lisp" should {
    "typical" in {
      val s = "(+ 1 2 3)"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(6))
    }
    "quote" in {
      val s = "'(+ 1 2 3)"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SList(SAtom("+"), SNumber(1), SNumber(2), SNumber(3)))
    }
  }
  "token" should {
    "string" in {
      val s = "\"hello\""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("hello"))
    }
    "url" in {
      val s = "http://www.yahoo.com"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SUrl(s))
    }
    "xml" in {
      val s = "<user><name>taro</name><city>yokohama</city></user>"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SXml(s))
    }
  }
  "javascript" should {
    "math expression" in {
      val s = "${1 + 2 + 3}"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(6))
    }
  }
  "jexl" should {
    "math expression" in {
      val s = "jexl${1 + 2 + 3}"
      val script = parse(s)
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
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(3))
    }
    "fetch and go" in {
      val s = """http://www.yahoo.com
length
"""
// //div[@class='value']
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SNumber(109))
    }
  }
  "xpath" should {
    "not found" in {
      val s = """path-get /html/div[id='city'] <html><div id="user">taro</div><div id="city">yokohama</div></html>"""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SNil)
    }
    "xml" in {
      val s = "path-get /user/city <user><name>taro</name><city>yokohama</city></user>"
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("yokohama"))
    }
    "html" in {
      val s = """path-get /html/div[@id='city'] <html><div id="user">taro</div><div id="city">yokohama</div></html>"""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("yokohama"))
    }
    "json" in {
      val s = """path-get /user/city {"user": {"name":"taro", "city":"yokohama"}}"""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("yokohama"))
    }
    "record" in {
      val s = """path-get xpath"@city" record"name:taro\tcity:yokohama""""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("yokohama"))
    }
    "record with raw xpath" in {
      val s = """path-get @city record"name:taro\tcity:yokohama""""
      val script = parse(s)
      val r = engine.apply(script)
      result(r) should be(SString("yokohama"))
    }
  }
}
