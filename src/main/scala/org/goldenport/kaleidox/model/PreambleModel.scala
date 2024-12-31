package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import java.net.URI
import org.smartdox.Description
import org.goldenport.context.Consequence
import org.goldenport.parser.LogicalBlocks
import org.goldenport.parser.LogicalSection
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.Model.Division
import org.goldenport.kaleidox.Model.IdentificationDivision
import org.goldenport.kaleidox.Model.ImportDivision
import org.goldenport.kaleidox.model.vision.VisionModel
import org.goldenport.kaleidox.model.business.BusinessModel
import org.goldenport.kaleidox.model.requirement.RequirementModel
import org.goldenport.kaleidox.model.analysis.AnalysisModel

/*
 * @since   Jan.  1, 2024
 *  version Jan.  3, 2024
 *  version Jul. 12, 2024
 * @version Nov.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class PreambleModel(
  description: Description = Description.name("preamble"),
  business: BusinessModel,
  vision: VisionModel,
  requirement: RequirementModel,
  analysis: AnalysisModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = vision.isEmpty && business.isEmpty && requirement.isEmpty && analysis.isEmpty
  def toOption: Option[PreambleModel] = if (isEmpty) None else Some(this)

  def +(rhs: PreambleModel) = copy(
    business = business + rhs.business,
    vision = vision + rhs.vision,
    requirement = requirement + rhs.requirement,
    analysis = analysis + rhs.analysis
  )
}

object PreambleModel {
  def empty(config: Config) = PreambleModel(
    business = BusinessModel.empty(config),
    vision = VisionModel.empty,
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

  case class Builder(
    config: Config,
    directive: Builder.Directive = Builder.Directive.default
  ) {
    def parse(p: String): Consequence[PreambleModel] = {
      val bconfig = if (config.isLocation)
        LogicalBlocks.Config.easyhtml
      else
        LogicalBlocks.Config.easyhtml.withoutLocation
      val blocks = LogicalBlocks.parse(bconfig, p)
      parse(blocks)
    }

    def parse(p: LogicalBlocks): Consequence[PreambleModel] = {
      val divs = p.blocks collect {
        case m: LogicalSection => Division.take(m)
      }
      val (iddiv, xs) = division_id_elements(divs)
      iddiv match {
        case Some(s) =>
          val d = Builder.Directive.parse(s)
          copy(directive = d).parse(xs)
        case None => parse(xs)
      }
    }

    def parse(ps: Seq[Division]): Consequence[PreambleModel] =
      directive.scope match {
        case Builder.Scope.System => _parse(ps)
        case Builder.Scope.Business =>
          for {
            bm <- BusinessModel.parse(config, ps)
          } yield PreambleModel(config, bm)
      }

    private def _parse(ps: Seq[Division]): Consequence[PreambleModel] = {
      case class Z(
      ) {
        def r = ???

        def +(rhs: Division) = rhs match {
          case m: VisionModel.VisionDivision => ???
          ???
        }
      }
      ps./:(Z())(_+_).r
    }

    protected final def division_id_elements(ps: Seq[Division]): (Option[IdentificationDivision], List[Division]) = {
      val iddiv = ps collect {
        case m: IdentificationDivision => m
      }
      val xs = ps flatMap {
        case m: IdentificationDivision => None
        case m => Some(m)
      }
      (iddiv.headOption, xs.toList)
    }

    // TODO Common
    protected final def import_library(ps: Vector[ImportDivision]): LibraryHangar =
      ps./:(LibraryHangar.empty)((z, x) => _import_library(z, x))

    private def _import_library(h: LibraryHangar, p: ImportDivision): LibraryHangar =
      p.locators./:(h)(_import_library)

    private def _import_library(h: LibraryHangar, p: Locator): LibraryHangar =
      if (h.isExists(p))
        h
      else
        h.add(p.uri, load(p.uri))

    private def load(p: URI): Model = ???
  }
  object Builder {
    sealed trait Scope
    object Scope {
      case object Business extends Scope
      case object System extends Scope
    }

    case class Directive(
      scope: Scope = Scope.System
    )
    object Directive {
      val default = Directive()

      def parse(p: IdentificationDivision): Directive = ???
    }
  }

  def apply(c: Config, p: BusinessModel): PreambleModel = PreambleModel(
    business = p,
    vision = VisionModel.empty,
    requirement = RequirementModel.empty(c),
    analysis = AnalysisModel.empty(c)
  )
  def parse(p: Config, s: String): Consequence[PreambleModel] =
    Builder(p).parse(s)
}
