package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.event._
import org.goldenport.kaleidox._

/*
 * @since   May. 22, 2021
 * @version May. 23, 2021
 * @author  ASAMI, Tomoharu
 */
trait BuilderBase[M, R, C] {
  def build(p: LogicalSection): M = {
    val xs = p.sections
    if (xs.isEmpty)
      _flat(p)
    else
      _subsections(xs)
  }

  private def _flat(p: LogicalSection) = {
    val r = _create_flat(p)
    to_Model(r)
  }

  protected def to_Model(p: Option[R]): M

  private def _subsections(ps: Seq[LogicalSection]) = {
    val stms = ps.flatMap(_create_subsection)
    _to_model(stms)
  }

  private def _to_model(ps: Seq[C]) = {
    val builder = TreeMap.Builder[C](".")
    val a = ps./:(builder)((z, x) => z.add(class_Name(x), x)).build
    to_Model(a)
  }

  protected def class_Name(p: C): String

  protected def to_Model(p: TreeMap[C]): M

  private def _create_flat(p: LogicalSection): Option[R] = {
    val s = p.text
    parse_Flat(s)
  }

  private def _create_subsection(p: LogicalSection): Option[C] = {
    val name = p.nameForModel
    val s = p.text
    parse_Subsection(name, s)
  }

  protected def parse_Flat(p: String): Option[R]

  protected def parse_Subsection(name: String,p: String): Option[C]
}
