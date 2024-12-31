package org.goldenport.kaleidox.model.vision

import org.goldenport.collection.VectorMap
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
 *  version Aug.  4, 2024
 * @version Nov. 20, 2024
 * @author  ASAMI, Tomoharu
 */
case class VisionModel(
  description: Description = Description.name("vision"),
  statement: Option[VisionModel.VisionStatement] = None
) extends Model.ISubModel {
  protected def display_String: String = ???
  protected def print_String: String = ???
  protected def show_String: String = ???

  def isEmpty: Boolean = true // TODO

  def +(rhs: VisionModel): VisionModel = this
}

object VisionModel {
  val empty = VisionModel()
  val divisionNames = Vector("vision")

  case class VisionDivision(section: LogicalSection) extends Division {
    val name = "vision"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: VisionDivision => copy(section + m.section)
    }

    def makeModel(c: Config): VisionModel = {
      make_description_sections(c, section)
      VisionModel()
    }
  }
  object VisionDivision extends DivisionFactory {
    override val name_Candidates = Vector("vision")
    protected def to_Division(p: LogicalSection): Division = VisionDivision(p)
  }

  sealed trait VisionStatement {
  }
  object VisionStatement {
    case class ElevatorPitch(
      target: Option[String], // FOR
      user: Option[String], // WHO
      productName: Option[String], // THE
      productCategory: Option[String], // is a
      keyMerit: Option[String], // THAT
      competitiveProduct: Option[String], // UNLIKE
      differentiatingFactor: Option[String], // OUR PRODUCT
      properties: VectorMap[String, String] = VectorMap.empty
    ) extends VisionStatement
    object ElevatorPitch {

    }
  }
}
