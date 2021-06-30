package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.RAISE
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.sexpr.SXsl
import org.goldenport.kaleidox._

/*
 * @since   Jun. 25, 2021
 * @version Jun. 26, 2021
 * @author  ASAMI, Tomoharu
 */
case class XslModel(
  classes: TreeMap[XslModel.XslClass]
) {
  def isEmpty = classes.isEmpty
  def toOption: Option[XslModel] = if (isEmpty) None else Some(this)

  def +(rhs: XslModel): XslModel = XslModel(classes + rhs.classes)
}

object XslModel {
  val empty = XslModel(TreeMap.empty[XslClass])

  implicit object XslModelMonoid extends Monoid[XslModel] {
    def zero = XslModel.empty
    def append(lhs: XslModel, rhs: => XslModel) = lhs + rhs
  }

  case class XslClass(
    name: String,
    xsl: SXsl
  ) {
  }
  object XslClass {
  }

  def apply(p: XslClass): XslModel = XslModel(TreeMap.create(".", Vector(p.name -> p)))

  def create(config: Config, p: LogicalSection): XslModel =
    Builder(config).build(p)

  case class Builder(config: Config) extends BuilderBase[XslModel, XslClass, XslClass] {
    protected def to_Model(p: Option[XslClass]): XslModel = p.fold(XslModel.empty)(XslModel.apply)

    protected def class_Name(p: XslClass): String = p.name

    protected def to_Model(p: TreeMap[XslClass]): XslModel = XslModel(p)

    protected def parse_Flat(p: String): Option[XslClass] = RAISE.notImplementedYetDefect

    protected def parse_Subsection(name: String, p: String): Option[XslClass] = {
      val a = SXsl(p) // TODO validate
      Some(XslClass(name, a))
    }
  }
}
