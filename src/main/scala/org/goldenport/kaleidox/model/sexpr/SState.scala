package org.goldenport.kaleidox.model.sexpr

import org.goldenport.sexpr._
import org.goldenport.sm._

/*
 * @since   May. 29, 2021
 *  version May. 30, 2021
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
case class SState(state: State) extends SExtension with Mutable {
  override def titleInfo = state.status
}
