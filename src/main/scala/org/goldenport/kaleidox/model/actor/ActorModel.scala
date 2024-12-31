package org.goldenport.kaleidox.model.actor

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
 *  version Nov.  6, 2024
 * @version Dec. 25, 2024
 * @author  ASAMI, Tomoharu
 */
case class ActorModel(
  description: Description = Description.name("actor"),
  sections: List[Section] = Nil
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = sections.isEmpty

  def +(rhs: ActorModel): ActorModel = this // TODOÎ
}

object ActorModel {
  val empty = ActorModel()
  val divisionNames = Vector("actor")

  case class ActorDivision(section: LogicalSection) extends Division {
    val name = "actor"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ActorDivision => copy(section + m.section)
    }

    def makeModel(config: Config): ActorModel = {
      val (desc, sections) = make_description_sections(config, section)
      ActorModel(desc, sections)
    }
  }
  object ActorDivision extends DivisionFactory {
    override val name_Candidates = Vector("actor")
    protected def to_Division(p: LogicalSection): Division = ActorDivision(p)
  }
}
