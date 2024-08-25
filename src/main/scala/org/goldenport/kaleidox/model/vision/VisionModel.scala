package org.goldenport.kaleidox.model.vision

import org.goldenport.parser.LogicalSection
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.kaleidox.Model.Division
import org.goldenport.kaleidox.Model.DivisionFactory
import org.smartdox.Dox
import org.smartdox.Description
import org.smartdox.parser.Dox2Parser

/*
 * @since   Jan.  1, 2024
 *  version Jan.  3, 2024
 *  version Jul.  7, 2024
 * @version Aug.  4, 2024
 * @author  ASAMI, Tomoharu
 */
case class VisionModel(
  description: Description = Description.name("vision")
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = true // TODO
}

object VisionModel {
  val empty = VisionModel()
  val divisionNames = Vector("vision")

  case class VisionDivision(section: LogicalSection) extends Division {
    val name = "vision"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: VisionDivision => copy(section + m.section)
    }

    def makeModel(c: Config): VisionModel =
      VisionModel(make_description(c, section))
  }
  object VisionDivision extends DivisionFactory {
    override val name_Candidates = Vector("vision")
    protected def to_Division(p: LogicalSection): Division = VisionDivision(p)
  }
}
