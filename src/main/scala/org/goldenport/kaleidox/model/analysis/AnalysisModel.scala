package org.goldenport.kaleidox.model.analysis

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
case class AnalysisModel(
  description: Description = Description.name("analysis"),
  sections: List[Section] = Nil,
  entities: EntityModel,
  events: EventModel
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty && entities.isEmpty && events.isEmpty
}

object AnalysisModel {
  def empty(config: Config) = AnalysisModel(
    entities = EntityModel.empty(config),
    events = EventModel.empty
  )
  val divisionNames = Vector("analysis")
  case class AnalysisDivision(section: LogicalSection) extends Division {
    val name = "analysis"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: AnalysisDivision => copy(section + m.section)
    }

    def makeModel(config: Config): AnalysisModel = {
      val (desc, sections) = make_description_sections(config, section)
      AnalysisModel(desc, sections, ???, ???)
    }
  }
  object AnalysisDivision extends DivisionFactory {
    override val name_Candidates = Vector("analysis")
    protected def to_Division(p: LogicalSection): Division = AnalysisDivision(p)
  }
}
