package org.goldenport.kaleidox.model.sexpr

import org.goldenport.sexpr._
import org.goldenport.event._

/*
 * @since   May. 22, 2021
 * @version May. 29, 2021
 * @author  ASAMI, Tomoharu
 */
case class SEvent(event: Event) extends SExtension {
  override def titleInfo = event.name
}
