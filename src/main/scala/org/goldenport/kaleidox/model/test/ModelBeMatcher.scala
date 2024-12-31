package org.goldenport.kaleidox.model.test

import org.scalatest.matchers.{BeMatcher, MatchResult}

/*
 * @since   Dec. 26, 2024
 * @version Dec. 26, 2024
 * @author  ASAMI, Tomoharu
 */
trait ModelBeMatcher[T] extends BeMatcher[T] with ModelMatchResultHelper {
  def expected: T

  def apply(actual: T): MatchResult =
    if (expected eq actual)
      match_success
    else
      apply_Match(actual)

  protected def apply_Match(actual: T): MatchResult
}
