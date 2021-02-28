package org.goldenport.kaleidox

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import org.scalatest.matchers._
import scala.util.matching.Regex
import org.goldenport.cli.Environment
import org.goldenport.sexpr._
import org.goldenport.record.v3._
import org.goldenport.record.unitofwork.interpreter.MockUnitOfWorkLogic
import org.goldenport.kaleidox.interpreter.Interpreter

/*
 * @since   Jan. 19, 2021
 *  version Jan. 31, 2021
 * @version Feb. 14, 2021
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class LiteralSpec extends WordSpec with Matchers with GivenWhenThen {
  def literal(p: SExpr) = ScriptValueMatcher(p)
  def result(p: SExpr) = ResultValueMatcher(p)

  case class ScriptValueMatcher(o: SExpr) extends Matcher[Script] {
    def apply(p: Script) = p.expressions match {
      case x +: Seq() => MatchResult(x.asSExpr == o, s"$x was not equal to $o", s"$x was equal to $o")
      case x +: xs => MatchResult(false, s"Too many expressions: ${p.expressions}", "")
      case _ => MatchResult(false, "Empty expressions", "")
    }
  }

  case class ResultValueMatcher(o: SExpr) extends Matcher[Vector[Expression]] {
    def apply(p: Vector[Expression]) = p match {
      case x +: Seq() => MatchResult(x.asSExpr == o, s"$x was not equal to $o", s"$x was equal to $o")
      case x +: xs => MatchResult(false, s"Too many expressions: ${p}", "")
      case _ => MatchResult(false, "Empty expressions", "")
    }
  }

  val env = Environment.create()
  val config = Config.log.debug // trace // warn // debug
  val context = ExecutionContext(env, config)
  val interpreter = Interpreter.create(context)
  val engine = Engine(context, Universe.empty, interpreter)
  engine.initialize()

  def parse(p: String): Script = Script.parse(config, p)
  def parseWithoutMetaCommand(p: String): Script = Script.parseWithoutMetaCommand(config, p)

  "string" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "atom" should {
    "eval" in {
      val s = "'hello"
      val script = parse(s)
      val r = engine.apply(script)
      r should result(SAtom("hello"))
    }
  }
  "keyword" should {
    "natural" in {
      val s = ":hello"
      val script = parseWithoutMetaCommand(s)
      val r = engine.apply(script)
      r should result(SKeyword("hello"))
    }
  }
  "number" should {
    "natural" in {
      val s = "10"
      val script = parse(s)
      script should literal(SNumber(10))
    }
  }
  "rational" should {
    "natural" in {
      val s = "2/5"
      val script = parse(s)
      script should literal(SRational(2, 5))
    }
  }
  "complex" should {
    "natural" in {
      val s = "1+5i"
      val script = parse(s)
      script should literal(SComplex(1.0, 5.0))
    }
  }
  "range" should {
    import org.goldenport.values.RepeatRange
    "natural" in {
      val s = "1~3+1"
      val script = parse(s)
      script should literal(SRange(RepeatRange(1, 3, true, true)))
    }
  }
  "interval" should {
    import org.goldenport.values.NumberInterval
    "natural" in {
      val s = "1~3"
      val script = parse(s)
      script should literal(SInterval(NumberInterval.closed(1, 3)))
    }
  }
  "list" should {
    "nil" in {
      val s = "nil"
      val script = parse(s)
      script should literal(SNil)
    }
    "t" in {
      val s = "t"
      val script = parse(s)
      script should literal(SBoolean.TRUE)
    }
    "true" in {
      val s = "true"
      val script = parse(s)
      script should literal(SBoolean.TRUE)
    }
    "false" in {
      val s = "false"
      val script = parse(s)
      script should literal(SBoolean.FALSE)
    }
    "natural" in {
      val s = "(1 2 3)"
      val script = parse(s)
      script should literal(SList(SNumber(1), SNumber(2), SNumber(3)))
    }
  }
  "lambda" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "error" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "binary" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "i18nstring" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "i18ntemplate" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SString("hello"))
    }
  }
  "regex" should {
    "string" in {
      val s = """regex"(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)""""
      val script = parse(s)
      script should literal(SRegex(new Regex(s)))
    }
    "sexpr" in {
      val s = """(regex raw"(\d\d\d\d)-(\d\d)-(\d\d)" "year" "month" "day")"""
      val script = parse(s)
      script should literal(SRegex(new Regex(s, "year", "month", "day")))
    }
  }
  "clob" should {
    "string" in {
      val s = "clob\"hello\""
      val script = parse(s)
      script should literal(SClob("hello"))
    }
  }
  "blob" should {
    "string" in {
      val s = "blob\"hello\""
      val script = parse(s)
      script should literal(SBlob.text("hello"))
    }
  }
  // "document" should { // TODO
  //   "natural" in {
  //     val s = "\"hello\""
  //     val script = Script.parse(s)
  //     script should literal(SString("hello"))
  //   }
  // }
  // "voucher" should { // TODO
  //   "natural" in {
  //     val s = "\"hello\""
  //     val script = Script.parse(s)
  //     script should literal(SString("hello"))
  //   }
  // }
  // "schema" should {
  //   "natural" in {
  //     val s = "\"hello\""
  //     val script = Script.parse(s)
  //     script should literal(SString("hello"))
  //   }
  // }
  "query" should {
    "natural" in {
      val s = "query\"hello\""
      val script = parse(s)
      script should literal(SQuery.create("hello"))
    }
  }
  "record" should {
    "natural" in {
      val s = "a:A;b:B"
      val script = parse(s)
      script should literal(SRecord(Record.data("a" -> "A", "b" -> "B")))
    }
  }
  "table" should {
    "natural" in {
      val s = """???"""
      val script = parse(s)
      script should literal(STable.data(Vector(Record.data("a" -> "A", "b" -> "B"))))
    }
  }
  "matrix" should {
    "natural" in {
      val s = "matrix\"hello\""
      val script = parse(s)
      script should literal(SMatrix.data(List(List(1, 2, 3))))
    }
  }
  // "dataframe" should {
  //   "natural" in {
  //     val s = "\"hello\""
  //     val script = Script.parse(s)
  //     script should literal(SString("hello"))
  //   }
  // }
  "lxsv" should {
    "natural" in {
      val s = "lxsv\"hello\""
      val script = parse(s)
      script should literal(SLxsv("hello"))
    }
  }
  "url" should {
    "natural" in {
      val s = "http://example.com"
      val script = parse(s)
      script should literal(SUrl("http://example.com"))
    }
  }
  "urn" should {
    "natural" in {
      val s = "urn:example:12345"
      val script = parse(s)
      script should literal(SUrn("urn:example:12345"))
    }
  }
  "uri" should {
    "natural" in {
      val s = "example:12345"
      val script = parse(s)
      script should literal(SUri("example:12345"))
    }
  }
  "expression" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SExpression("hello"))
    }
  }
  "script" should {
    "natural" in {
      val s = "${1 + 2}"
      val script = parse(s)
      script should literal(SScript("1 + 2"))
    }
  }
  "bean" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SBean("hello"))
    }
  }
  "xml" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SXml("hello"))
    }
  }
  "html" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SHtml("hello"))
    }
  }
  "xpath" should {
    "natural" in {
      val s = "/a/b"
      val script = parse(s)
      script should literal(SXPath("/a/b"))
    }
  }
  "xsl" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SXsl("hello"))
    }
  }
  "pug" should {
    "natural" in {
      val s = "pug\"hello\""
      val script = parse(s)
      script should literal(SPug("hello"))
    }
  }
  "json" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SJson("hello"))
    }
  }
  "datetime" should {
    "natural" in {
      val s = "2021-01-30T12:01:00+09:00"
      val script = parse(s)
      script should literal(SDateTime.jst(2021, 1, 30, 12, 1, 0))
    }
  }
  "localdatetime" should {
    "natural" in {
      val s = "2021-01-20T07:28:00"
      val script = parse(s)
      script should literal(SLocalDateTime(2021, 1, 20, 7, 28, 0))
    }
  }
  "localdate" should {
    "natural" in {
      val s = "2021-01-20"
      val script = parse(s)
      script should literal(SLocalDate(2021, 1, 20))
    }
  }
  "localtime" should {
    "natural" in {
      val s = "07:28:00"
      val script = parse(s)
      script should literal(SLocalTime(7, 28, 0))
    }
  }
  "monthday" should {
    "natural" in {
      val s = "1-20"
      val script = parse(s)
      script should literal(SMonthDay(1, 20))
    }
  }
  "datetimeinterval" should {
    "natural" in {
      val s = "2021-01-23T06:12:00+09:00~"
      val script = parse(s)
      script should literal(SDateTimeInterval.create(s))
    }
  }
  "localdatetimeinterval" should {
    "natural" in {
      val s = "2021-01-23T06:12:00~"
      val script = parse(s)
      script should literal(SLocalDateTimeInterval.create(s))
    }
  }
  "duration" should {
    "natural" in {
      val s = "DT15H"
      val script = parse(s)
      script should literal(SDuration.hour(15))
    }
  }
  "period" should {
    "natural" in {
      val s = "P5Y2M10D"
      val script = parse(s)
      script should literal(SPeriod.yearMonthDay(5, 2, 10))
    }
  }
  "money" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SMoney.yen(100))
    }
  }
  "percent" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SPercent(35.5))
    }
  }
  "unit" should {
    "natural" in {
      val s = "\"hello\""
      val script = parse(s)
      script should literal(SUnit("hello"))
    }
  }
}
