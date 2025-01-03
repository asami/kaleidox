package org.goldenport.kaleidox.model.requirement

import org.goldenport.parser.LogicalSection
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.Model.Division
import org.goldenport.kaleidox.Model.DivisionFactory
import org.goldenport.kaleidox.model.EntityModel
import org.goldenport.kaleidox.model.EventModel
import org.smartdox.Description
import org.smartdox.Section

/*
 * @since   Jan.  1, 2024
 *  version Jan.  3, 2024
 *  version Jul. 12, 2024
 *  version Aug.  4, 2024
 * @version Nov.  4, 2024
 * @author  ASAMI, Tomoharu
 */
case class RequirementModel(
  description: Description = Description.name("business"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty

  def +(rhs: RequirementModel): RequirementModel = copy(
    entities = entities + rhs.entities,
    events = events + rhs.events
  )
}

object RequirementModel {
  def empty(config: Config) = RequirementModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )
  val divisionNames = Vector("requirement")

  case class RequirementDivision(section: LogicalSection) extends Division {
    val name = "requirement"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: RequirementDivision => copy(section + m.section)
    }

    def makeModel(config: Config): RequirementModel = {
      val (desc, sections) = make_description_sections(config, section)
      RequirementModel(desc, sections, ???, ???)
    }
  }
  object RequirementDivision extends DivisionFactory {
    override val name_Candidates = Vector("requirement")
    protected def to_Division(p: LogicalSection): Division = RequirementDivision(p)
  }
}
