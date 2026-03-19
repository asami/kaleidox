package org.goldenport.kaleidox.model

import org.goldenport.sm.{Parcel, SmGuard}

/*
 * CML guard expression holder for MVEL-oriented generation path.
 * Runtime evaluation is delegated to CNCF-generated rule guards.
 */
/*
 * @since   May. 19, 2025
 * @version Mar. 19, 2026
 * @author  ASAMI, Tomoharu
 */
final case class CmlExpressionGuard(expression: String) extends SmGuard {
  def accept(p: Parcel): Boolean = true
}
