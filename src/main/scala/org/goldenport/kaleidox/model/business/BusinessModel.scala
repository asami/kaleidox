package org.goldenport.kaleidox.model.business

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
 * @version Aug.  4, 2024
 * @author  ASAMI, Tomoharu
 */
case class BusinessModel(
  description: Description = Description.name("business"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty
}

object BusinessModel {
  // val empty = BusinessModel()
  def empty(config: Config) = BusinessModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )

  val divisionNames = Vector("business")

  case class BusinessDivision(section: LogicalSection) extends Division {
    val name = "business"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: BusinessDivision => copy(section + m.section)
    }

    def makeModel(config: Config): BusinessModel = {
      val ef = config.entityFactory
      val (desc, sections) = make_description_sections(config, section)
      BusinessModel(desc, sections, ???, ???)
    }
  }
  object BusinessDivision extends DivisionFactory {
    override val name_Candidates = Vector("business")
    protected def to_Division(p: LogicalSection): Division = BusinessDivision(p)
  }
}
