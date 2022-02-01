package org.goldenport.kaleidox.extension.modeler

import org.goldenport.RAISE
import org.goldenport.sexpr._
import org.goldenport.kaleidox.lisp.Context

/*
 * @since   Dec. 18, 2021
 * @version Dec. 18, 2021
 * @author  ASAMI, Tomoharu
 */
trait Modeler {
  def generateStateMachineDiagram(
    c: Context,
    name: String,
    resourceid: Option[String]
  ): SExpr
}

object Modeler {
  object Unsupported extends Modeler {
    def generateStateMachineDiagram(
      c: Context,
      name: String,
      resourceid: Option[String]
    ): SExpr = RAISE.unsupportedOperationFault
  }
}
