package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.Description
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.model.vision.VisionModel
import org.goldenport.kaleidox.model.business.BusinessModel
import org.goldenport.kaleidox.model.requirement.RequirementModel
import org.goldenport.kaleidox.model.analysis.AnalysisModel


/*
 * @since   Jan.  1, 2024
 *  version Jan.  3, 2024
 * @version Jul. 12, 2024
 * @author  ASAMI, Tomoharu
 */
case class PreambleModel(
  description: Description = Description.name("preamble"),
  vision: VisionModel = VisionModel.empty,
  business: BusinessModel,
  requirement: RequirementModel,
  analysis: AnalysisModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = vision.isEmpty && business.isEmpty && requirement.isEmpty && analysis.isEmpty
  def toOption: Option[PreambleModel] = if (isEmpty) None else Some(this)

  def +(rhs: PreambleModel) = ???
}

object PreambleModel {
  def empty(config: Config) = PreambleModel(
    business = BusinessModel.empty(config),
    requirement = RequirementModel.empty(config),
    analysis = AnalysisModel.empty(config)
  )

  // implicit object PreambleModelMonoid extends Monoid[PreambleModel] {
  //   def zero = PreambleModel.empty
  //   def append(lhs: PreambleModel, rhs: => PreambleModel) = lhs + rhs
  // }

  implicit object PreambleModelSemigroup extends Semigroup[PreambleModel] {
    def append(lhs: PreambleModel, rhs: => PreambleModel) = lhs + rhs
  }

  def divisionNames = VisionModel.divisionNames ++ BusinessModel.divisionNames ++ RequirementModel.divisionNames ++ AnalysisModel.divisionNames
}
