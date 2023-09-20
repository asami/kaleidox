package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.event._
import org.goldenport.kaleidox._

/*
 * @since   May.  5, 2021
 *  version May. 23, 2021
 *  version Jun. 26, 2021
 *  version Oct. 31, 2021
 * @version Aug. 21, 2023
 * @author  ASAMI, Tomoharu
 */
case class EventModel(
  rule: EventRule = EventRule.empty
) extends Model.ISubModel {
  val name = "schema"

  protected def display_String: String = rule.display

  protected def print_String: String = rule.print

  protected def show_String: String = rule.show

  def isEmpty: Boolean = rule.isEmpty
  def toOption: Option[EventModel] = if (isEmpty) None else Some(this)

  def +(rhs: EventModel): EventModel = copy(rule = rule + rhs.rule)

  def createOption(name: String): Option[Event] = rule.createOption(name)

  def createCallOption(name: String, to: ObjectId): Option[Event] = rule.createCallOption(name, to)
}

object EventModel {
  implicit object EventModelMonoid extends Monoid[EventModel] {
    def zero = EventModel.empty
    def append(lhs: EventModel, rhs: => EventModel) = lhs + rhs
  }

  val empty = EventModel()

  def apply(p: EventClazz): EventModel = EventModel(EventRule(p))

  def create(config: Config, p: LogicalSection): EventModel =
    Builder(config).build(p)

  // def createClassOption(config: Config, p: LogicalSection): Option[EventClazz] =
  //   Builder(config).createClassOption(p)

  case class Builder(config: Config) extends BuilderBase[EventModel, EventRule, EventClazz] {
    protected def to_Model(p: Option[EventRule]): EventModel = p.map(EventModel.apply).orZero

    protected def class_Name(p: EventClazz): String = p.name

    protected def to_Model(p: TreeMap[EventClazz]): EventModel = EventModel(EventRule(p))

    protected def parse_Flat(p: String): Option[EventRule] = EventRule.parse(p).toOption

    protected def parse_Subsection(name: String, p: String): Option[EventClazz] = ???

    //   createClassOption(p).
    //     map(EventModel.apply).
    //     getOrElse(empty)

    // def createClassOption(p: Section): Option[EventClazz] = {
    //   val name = p.nameForModel
    //   val s = p.toText
    //   val r = EventClazz.parse(s)
    //   r.toOption // TODO
    // }
  }
}
