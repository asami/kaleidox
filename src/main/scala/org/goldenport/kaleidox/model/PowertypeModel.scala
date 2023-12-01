package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.goldenport.collection.VectorMap
import org.goldenport.kaleidox.Model

/*
 * @since   Oct. 12, 2023
 * @version Oct. 29, 2023
 * @author  ASAMI, Tomoharu
 */
case class PowertypeModel(
  classes: VectorMap[String, PowertypeModel.PowertypeClass] = VectorMap.empty,
  description: Description = Description.name("powertype")
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def print_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def show_String: String = classes.values.map(x => x.name).mkString("\n")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[PowertypeModel] = if (isEmpty) None else Some(this)

  def +(rhs: PowertypeModel): PowertypeModel = copy(classes ++ rhs.classes)
}

object PowertypeModel {
  val empty = PowertypeModel()

  implicit object PowertypeModelMonoid extends Monoid[PowertypeModel] {
    def zero = PowertypeModel.empty
    def append(lhs: PowertypeModel, rhs: => PowertypeModel) = lhs + rhs
  }

  case class PowertypeClass(
    description: Description,
    packageName: String = "domain" // TODO
  ) extends Description.Holder {
  }
  object PowertypeClass {
    def createOption(p: Section): Option[PowertypeClass] = ???
  }

  def apply(p: PowertypeClass): PowertypeModel = ???
}
