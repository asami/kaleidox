package org.goldenport.kaleidox

import org.goldenport.parser._
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 *  version Sep. 29, 2018
 *  version Oct. 27, 2018
 *  version Jan.  1, 2019
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class Script(
  expressions: Vector[Expression]
) extends Model.Division {
}

object Script extends Model.DivisionFactory {
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

  def apply(p: SExpr, ps: SExpr*): Script = {
    val a = p +: ps.toVector
    Script(a.map(LispExpression))
  }

  def parse(p: String): Script = {
    // println(s"kaleidox.Script#parse: $p")
    val sexprs = org.goldenport.sexpr.script.Script.parse(p)
    Script(sexprs.expressions.map(LispExpression))
  }

  def parse(p: LogicalBlocks): Script = {
    // println(s"kaleidox.Script#parse: $p")
    val sexprs = org.goldenport.sexpr.script.Script.parse(p)
    Script(sexprs.expressions.map(LispExpression))
  }
}
