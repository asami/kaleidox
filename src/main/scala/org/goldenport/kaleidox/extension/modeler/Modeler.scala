package org.goldenport.kaleidox.extension.modeler

import org.goldenport.RAISE
import org.goldenport.sexpr._
import org.goldenport.kaleidox.lisp.Context

/*
 * @since   Dec. 18, 2021
 *  version Aug.  4, 2023
 * @version Mar. 24, 2026
 * @author  ASAMI, Tomoharu
 */
trait Modeler {
  def generateStateMachineDiagram(
    c: Context,
    name: String,
    resourceid: Option[String]
  ): SExpr

  def generateDiagram(
    c: Context,
    model: SModel
  ): SExpr

  def generateScala(
    c: Context,
    model: SModel
  ): SExpr

  def generateScalaValue(
    c: Context,
    model: SModel
  ): SExpr
}

object Modeler {
  object Unsupported extends Modeler {
    def generateStateMachineDiagram(
      c: Context,
      name: String,
      resourceid: Option[String]
    ): SExpr = RAISE.unsupportedOperationFault

    def generateDiagram(
      c: Context,
      model: SModel
    ): SExpr = RAISE.unsupportedOperationFault

    def generateScala(
      c: Context,
      model: SModel
    ): SExpr = RAISE.unsupportedOperationFault

    def generateScalaValue(
      c: Context,
      model: SModel
    ): SExpr = RAISE.unsupportedOperationFault
  }
}
