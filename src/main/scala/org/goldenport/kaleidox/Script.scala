package org.goldenport.kaleidox

import scalaz._, Scalaz._
import org.goldenport.parser._
import org.goldenport.sexpr.SExpr
import org.goldenport.sexpr.SError
import org.goldenport.sexpr.script.Script.StringLiteralTokenizer

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Oct. 27, 2018
 *  version Jan.  1, 2019
 *  version Feb. 16, 2019
 *  version May. 17, 2019
 *  version Jul. 15, 2019
 *  version Jan. 22, 2021
 *  version Feb. 11, 2021
 * @version Mar. 16, 2021
 * @author  ASAMI, Tomoharu
 */
case class Script(
  expressions: Vector[Expression] = Vector.empty
) extends Model.Division {
  import Model._

  def isEmpty = expressions.isEmpty

  def toOption: Option[Script] = if (isEmpty) None else Some(this)

  def +(rhs: Script): Script = Script(expressions ++ rhs.expressions)

  def mergeOption(p: Division): Option[Division] = Option(p) collect {
    case m: Script => Script(expressions ++ m.expressions)
  }

  def listSExpr: List[SExpr] = expressions.map(_.asSExpr).toList
}

object Script extends Model.DivisionFactory {
  val empty = Script()

  implicit object ScriptMonoid extends Monoid[Script] {
    def zero = Script.empty
    def append(lhs: Script, rhs: => Script) = lhs + rhs
  }

  override val name_Candidates = Vector("script", "procedure", "program", "main")
  protected def to_Division(p: LogicalSection): Model.Division = {
    case class Z(
      expressions: Vector[Expression] = Vector.empty,
      sections: Vector[LogicalSection] = Vector.empty // currently not used
    ) {
      def r = Script(expressions)
      def +(rhs: LogicalBlock) = rhs match {
        case StartBlock => this
        case EndBlock => this
        case m: LogicalParagraph => copy(expressions = expressions ++ _expressions(m))
        case m: LogicalSection => copy(sections = sections :+ m)
        case m: LogicalVerbatim => this
      }

      private def _expressions(p: LogicalParagraph): Vector[Expression] =
        for {
          s <- p.getText.map(org.goldenport.sexpr.script.Script.parse).toVector
          e <- s.expressions
        } yield LispExpression(e)
      // private def _expressions(p: LogicalParagraph): Vector[Expression] = {
      //   val a = p.lines.lines.map(x => org.goldenport.sexpr.script.Script.parse(x.text))
      //   a.flatMap(_.expressions.map(LispExpression))
      // }
    }
    p.blocks.blocks./:(Z())(_+_).r
  }

  def apply(p: SExpr, ps: SExpr*): Script = apply(p +: ps.toVector)

  def apply(ps: Seq[SExpr]): Script = Script(ps.map(LispExpression).toVector)

  def parse(config: Config, p: String): Script = {
    // println(s"kaleidox.Script#parse: $p")
    val sexprs = org.goldenport.sexpr.script.Script.parse(config.scriptConfig, p)
    Script(sexprs.expressions.map(LispExpression))
  }

  def parse(config: Config, p: LogicalBlocks): Script = {
    // println(s"kaleidox.Script#parse: $p")
    val sexprs = org.goldenport.sexpr.script.Script.parse(config.scriptConfig, p)
    Script(sexprs.expressions.map(LispExpression))
  }

  def parseWithoutMetaCommand(config: Config, p: String): Script = {
    val sexprs = org.goldenport.sexpr.script.Script.parseWithoutMetaCommand(config.scriptConfig, p)
    Script(sexprs.expressions.map(LispExpression))
  }

  def parseOption(config: Config, p: LogicalSection): Option[Script] = {
    val r = parse(config, p.blocks)
    if (r.isEmpty)
      None
    else
      Some(r)
  }

  case object DoxLiteralTokenizer extends StringLiteralTokenizer {
    import org.goldenport.sexpr.IDocument
    import org.goldenport.sexpr.SDocument
    import org.smartdox._
    import org.smartdox.parser.Dox2Parser

    private val _config = Dox2Parser.Config.orgmodeInline

    case class DoxDocument(dox: Dox) extends IDocument {
    }

    def name = "dox"
    def literal(p: String): SExpr = {
      val parser = new Dox2Parser(_config)
      parser.apply(p) match {
        case ParseSuccess(dox, _) => SDocument(DoxDocument(dox))
        case m: ParseFailure[_] => SError.syntaxError(m)
        case EmptyParseResult() => SDocument(DoxDocument(Dox.empty))
      }
    }
  }
}
