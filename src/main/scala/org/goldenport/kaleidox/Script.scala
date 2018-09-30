package org.goldenport.kaleidox

import org.goldenport.parser._
import org.goldenport.sexpr.SExpr

/*
 * @since   Aug. 11, 2018
 * @version Sep. 29, 2018
 * @author  ASAMI, Tomoharu
 */
case class Script(
  expressions: Vector[Expression]
) extends Model.Division {
}

object Script extends Model.DivisionFactory {
  override val name_Candidates = Vector("script", "procedure", "program")
  protected def to_Division(p: LogicalSection): Model.Division = {
    case class Z(
      expressions: Vector[Expression] = Vector.empty,
      sections: Vector[LogicalSection] = Vector.empty // currently not used
    ) {
      def r = Script(expressions)
      def +(rhs: LogicalBlock) = rhs match {
        case m: LogicalParagraph => copy(expressions = expressions ++ _expressions(m))
        case m: LogicalSection => copy(sections = sections :+ m)
      }

      private def _expressions(p: LogicalParagraph): Vector[Expression] = {
        val a = p.lines.lines.map(x => org.goldenport.sexpr.script.Script.parse(x.text))
        a.flatMap(_.expressions.map(LispExpression))
      }
    }
    p.blocks.blocks./:(Z())(_+_).r
  }

  def apply(p: SExpr, ps: SExpr*): Script = {
    val a = p +: ps.toVector
    Script(a.map(LispExpression))
  }

  def parse(p: String): Script = {
    val sexprs = org.goldenport.sexpr.script.Script.parse(p)
    Script(sexprs.expressions.map(LispExpression))
  }

  def parse(p: LogicalBlocks): Script = {
    val sexprs = org.goldenport.sexpr.script.Script.parse(p)
    Script(sexprs.expressions.map(LispExpression))
  }
}
