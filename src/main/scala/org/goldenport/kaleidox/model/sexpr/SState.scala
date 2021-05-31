package org.goldenport.kaleidox.model.sexpr

import org.goldenport.sexpr._
import org.goldenport.statemachine._

/*
 * @since   May. 29, 2021
 * @version May. 30, 2021
 * @author  ASAMI, Tomoharu
 */
case class SState(state: State) extends SExtension with Mutable {
  override def titleInfo = state.status
}
