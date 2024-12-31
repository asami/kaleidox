package org.goldenport.kaleidox.model

import org.smartdox.test.DoxMatchers
import org.smartdox.test.DoxMatchResultHelper

/*
 * @since   Dec. 22, 2024
 * @version Dec. 29, 2024
 * @author  ASAMI, Tomoharu
 */
package object test {
  trait ModelMatchers extends DoxMatchers
      with EntityModelBeMatcher.Matchers with EventModelBeMatcher.Matchers {
  }
}
