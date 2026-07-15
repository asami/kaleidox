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
 *  version Dec. 25, 2024
 * @version Jul. 16, 2026
 * @author  ASAMI, Tomoharu
 */
case class ActorModel(
  description: Description = Description.name("actor"),
  sections: List[Section] = Nil,
  actors: Vector[ActorModel.ActorDefinition] = Vector.empty
) extends Model.ISubModel {
  protected def display_String: String = actors.map(_.name).mkString("Actor(", ", ", ")")
  protected def print_String: String = display_String
  protected def show_String: String = display_String

  def isEmpty: Boolean = sections.isEmpty && actors.isEmpty
  def toOption: Option[ActorModel] = if (isEmpty) None else Some(this)

  def +(rhs: ActorModel): ActorModel = copy(
    sections = sections ++ rhs.sections,
    actors = ActorModel._deduplicate(actors ++ rhs.actors)
  )
}

object ActorModel {
  val empty = ActorModel()
  val divisionNames = Vector("actor")

  final case class ActorDefinition(
    name: String,
    kind: Option[String] = None,
    summary: Option[String] = None,
    description: Option[String] = None
  )

  case class ActorDivision(section: LogicalSection) extends Division {
    val name = "actor"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ActorDivision => copy(section + m.section)
    }

    def makeModel(config: Config): ActorModel = {
      val (desc, sections) = make_description_sections(config, section)
      ActorModel(desc, sections, _actor_definitions(section.sections.toVector))
    }
  }
  object ActorDivision extends DivisionFactory {
    override val name_Candidates = Vector("actor")
    protected def to_Division(p: LogicalSection): Division = ActorDivision(p)
  }

  private def _actor_definitions(p: Vector[LogicalSection]): Vector[ActorDefinition] =
    _deduplicate(p.map { section =>
      ActorDefinition(
        name = section.nameForModel.trim,
        kind = _section_text(section, "kind"),
        summary = _section_text(section, "summary"),
        description = _section_text(section, "description")
      )
    }.filter(_.name.nonEmpty))

  private def _deduplicate(p: Vector[ActorDefinition]): Vector[ActorDefinition] =
    p.foldLeft(Vector.empty[ActorDefinition]) { (z, x) =>
      if (z.exists(_.name == x.name)) z else z :+ x
    }

  private def _section_text(p: LogicalSection, key: String): Option[String] =
    p.sections.find(_.keyForModel.equalsIgnoreCase(key)).
      map(_.text.trim).
      filter(_.nonEmpty)
}
