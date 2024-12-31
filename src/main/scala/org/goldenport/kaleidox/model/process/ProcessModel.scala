package org.goldenport.kaleidox.model.process

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
 * @since   Nov.  5, 2024
 * @version Nov.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class ProcessModel(
  description: Description = Description.name("business"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty

  def +(rhs: ProcessModel): ProcessModel = copy(
    entities = entities + rhs.entities,
    events = events + rhs.events
  )
}

object ProcessModel {
  def empty(config: Config) = ProcessModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )
  val divisionNames = Vector("process")

  case class ProcessDivision(section: LogicalSection) extends Division {
    val name = "process"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ProcessDivision => copy(section + m.section)
    }

    def makeModel(config: Config): ProcessModel = {
      val (desc, sections) = make_description_sections(config, section)
      ProcessModel(desc, sections, ???, ???)
    }
  }
  object ProcessDivision extends DivisionFactory {
    override val name_Candidates = Vector("process")
    protected def to_Division(p: LogicalSection): Division = ProcessDivision(p)
  }
}
