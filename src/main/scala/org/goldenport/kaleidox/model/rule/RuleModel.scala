package org.goldenport.kaleidox.model.rule

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
 * @version Dec. 29, 2024
 * @author  ASAMI, Tomoharu
 */
case class RuleModel(
  description: Description = Description.name("rule"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty

  def +(rhs: RuleModel): RuleModel = copy(
    entities = entities + rhs.entities,
    events = events + rhs.events
  )
}

object RuleModel {
  def empty(config: Config) = RuleModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )
  val divisionNames = Vector("rule")

  case class RuleDivision(section: LogicalSection) extends Division {
    val name = "rule"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: RuleDivision => copy(section + m.section)
    }

    def makeModel(config: Config): RuleModel = {
      val (desc, sections) = make_description_sections(config, section)
      RuleModel(desc, sections, ???, ???)
    }
  }
  object RuleDivision extends DivisionFactory {
    override val name_Candidates = Vector("rule")
    protected def to_Division(p: LogicalSection): Division = RuleDivision(p)
  }
}
