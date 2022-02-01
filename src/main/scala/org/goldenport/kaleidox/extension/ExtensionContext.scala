package org.goldenport.kaleidox.extension

import org.goldenport.kaleidox.extension.modeler.Modeler

/*
 * @since   Dec. 18, 2021
 * @version Dec. 18, 2021
 * @author  ASAMI, Tomoharu
 */
case class ExtensionContext(
  modeler: Modeler
) {
  def setModeler(p: Modeler) = copy(modeler = p)
}

object ExtensionContext {
  val default = ExtensionContext(
    Modeler.Unsupported
  )
}
