package org.goldenport.kaleidox.model.role

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
case class RoleModel(
  description: Description = Description.name("business"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty

  def +(rhs: RoleModel): RoleModel = copy(
    entities = entities + rhs.entities,
    events = events + rhs.events
  )
}

object RoleModel {
  def empty(config: Config) = RoleModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )
  val divisionNames = Vector("role")

  case class RoleDivision(section: LogicalSection) extends Division {
    val name = "role"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: RoleDivision => copy(section + m.section)
    }

    def makeModel(config: Config): RoleModel = {
      val (desc, sections) = make_description_sections(config, section)
      RoleModel(desc, sections, ???, ???)
    }
  }
  object RoleDivision extends DivisionFactory {
    override val name_Candidates = Vector("role")
    protected def to_Division(p: LogicalSection): Division = RoleDivision(p)
  }
}
