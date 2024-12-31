package org.goldenport.kaleidox.model.business

import java.net.URI
import org.goldenport.context.Consequence
import org.goldenport.parser.LogicalBlocks
import org.goldenport.parser.LogicalSection
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.Model.Division
import org.goldenport.kaleidox.Model.DivisionFactory
import org.goldenport.kaleidox.Model.ImportDivision
import org.goldenport.kaleidox.model.Locator
import org.goldenport.kaleidox.model.LibraryHangar
import org.goldenport.kaleidox.model.EntityModel
import org.goldenport.kaleidox.model.EventModel
import org.goldenport.kaleidox.model.vision.VisionModel
import org.goldenport.kaleidox.model.actor.ActorModel
import org.goldenport.kaleidox.model.process.ProcessModel
import org.goldenport.kaleidox.model.rule.RuleModel
import org.smartdox.Description
import org.smartdox.Section

/*
 * @since   Jan.  1, 2024
 *  version Jan.  3, 2024
 *  version Jul. 12, 2024
 *  version Aug.  4, 2024
 *  version Nov.  7, 2024
 * @version Dec. 22, 2024
 * @author  ASAMI, Tomoharu
 */
case class BusinessModel(
  description: Description = Description.name("business"),
  sections: List[Section] = Nil,
  vision: VisionModel,
  actor: ActorModel,
  process: ProcessModel,
  entity: EntityModel,
  event: EventModel,
  rule: RuleModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && vision.isEmpty && actor.isEmpty && process.isEmpty && entity.isEmpty && event.isEmpty && rule.isEmpty

  def +(rhs: BusinessModel): BusinessModel = copy(
    vision = vision + rhs.vision,
    actor = actor + rhs.actor,
    process = process + rhs.process,
    entity = entity + rhs.entity,
    event = event + rhs.event,
    rule = rule + rhs.rule
  )
}

object BusinessModel {
  // val empty = BusinessModel()
  def empty(config: Config) = BusinessModel(
    vision = VisionModel.empty,
    actor = ActorModel.empty,
    process = ProcessModel.empty(config),
    entity = EntityModel.empty(config),
    event = EventModel.empty,
    rule = RuleModel.empty(config)
  )

  val divisionNames = Vector("business")

  case class BusinessDivision(section: LogicalSection) extends Division {
    val name = "business"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: BusinessDivision => copy(section + m.section)
    }

    def makeModel(config: Config): BusinessModel = createModel(config).take

    def createModel(config: Config): Consequence[BusinessModel] = {
      val ef = config.entityFactory
      val (desc, sections) = make_description_sections(config, section)
      for {
        model <- Builder(config).parse(section.blocks)
      } yield model.copy(description = desc, sections = sections)
    }
  }
  object BusinessDivision extends DivisionFactory {
    override val name_Candidates = Vector("business")
    protected def to_Division(p: LogicalSection): Division = BusinessDivision(p)
  }

  case class Builder(config: Config) {
    def parse(p: String): Consequence[BusinessModel] = {
      val bconfig = if (config.isLocation)
        LogicalBlocks.Config.easyhtml
      else
        LogicalBlocks.Config.easyhtml.withoutLocation
      val blocks = LogicalBlocks.parse(bconfig, p)
      parse(blocks)
    }

    def parse(p: LogicalBlocks): Consequence[BusinessModel] = {
      val divs = p.blocks collect {
        case m: LogicalSection => Division.take(m)
      }
      parse(divs)
    }

    def parse(ps: Seq[Division]): Consequence[BusinessModel] = Consequence {
      case class Z(
        business: BusinessModel = BusinessModel.empty(config),
        vision: VisionModel = VisionModel.empty,
        actor: ActorModel = ActorModel.empty,
        process: ProcessModel = ProcessModel.empty(config),
        entity: EntityModel = EntityModel.empty(config),
        event: EventModel = EventModel.empty,
        rule: RuleModel = RuleModel.empty(config)
      ) {
        def r = {
          business.copy(
            vision = business.vision + vision,
            actor = business.actor + actor,
            process = business.process + process,
            entity = business.entity + entity,
            event = business.event + event,
            rule = business.rule + rule
          )
        }

        def +(rhs: Division) = rhs match {
          case m: BusinessModel.BusinessDivision =>
            copy(business = business + m.makeModel(config))
          case m: VisionModel.VisionDivision =>
            copy(vision = vision + m.makeModel(config))
          case m: ActorModel.ActorDivision =>
            copy(actor = actor + m.makeModel(config))
          case m: ProcessModel.ProcessDivision =>
            copy(process = process + m.makeModel(config))
          case m: Model.EntityDivision =>
            copy(entity = entity + m.makeModel(config))
          case m: Model.EventDivision =>
            copy(event = event + m.makeModel(config))
          case _ => this
        }
      }
      ps./:(Z())(_+_).r
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

  def parse(config: Config, p: String): Consequence[BusinessModel] =
    Builder(config).parse(p)

  def parse(config: Config, ps: Seq[Division]): Consequence[BusinessModel] =
    Builder(config).parse(ps)

  def parseWitoutLocation(config: Config, p: String): Consequence[BusinessModel] =
    parse(config.withoutLocation, p)

  // object test {
  //   import org.scalatest.matchers.{BeMatcher, MatchResult}
  //   import org.goldenport.context.test.ConsequenceBeMatcher

  //   case class BusinessModelBeMatcher(model: BusinessModel) extends BeMatcher[BusinessModel] {
  //     def apply(left: BusinessModel): MatchResult = {
  //       val d = DescriptionBeMatcher(model.description)(left.description)
  //       ???
  //     }
  //   }
  //   object BusinessModelBeMatcher {
  //     trait Matchers {
  //       def consequence_business_model(p: BusinessModel) = ConsequenceBeMatcher(BusinessModelBeMatcher(p))
  //     }
  //   }

  //   trait BusinessModelMatchers extends BusinessModelBeMatcher.Matchers {
  //   }
  // }
}
