package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.Description
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.event._
import org.goldenport.RAISE
import org.goldenport.kaleidox._

/*
 * @since   May.  5, 2021
 *  version May. 23, 2021
 *  version Jun. 26, 2021
 *  version Oct. 31, 2021
 *  version Aug. 21, 2023
 *  version Oct. 15, 2023
 *  version Jul. 12, 2024
 *  version Dec. 28, 2024
 * @version Mar. 24, 2026
 * @author  ASAMI, Tomoharu
 */
case class EventModel(
  rule: EventRule = EventRule.empty,
  receptionDefinitions: Vector[SchemaModel.EventDefinition] = Vector.empty,
  routingDefinitions: Vector[SchemaModel.RoutingDefinition] = Vector.empty,
  subscriptionDefinitions: Vector[SchemaModel.SubscriptionDefinition] = Vector.empty,
  description: Description = Description.name("event")
) extends Model.ISubModel {
  protected def display_String: String = rule.display

  protected def print_String: String = rule.print

  protected def show_String: String = rule.show

  def isEmpty: Boolean =
    rule.isEmpty &&
      receptionDefinitions.isEmpty &&
      routingDefinitions.isEmpty &&
      subscriptionDefinitions.isEmpty
  def toOption: Option[EventModel] = if (isEmpty) None else Some(this)

  def +(rhs: EventModel): EventModel = copy(
    rule = rule + rhs.rule,
    receptionDefinitions = receptionDefinitions ++ rhs.receptionDefinitions,
    routingDefinitions = routingDefinitions ++ rhs.routingDefinitions,
    subscriptionDefinitions = subscriptionDefinitions ++ rhs.subscriptionDefinitions
  )

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
    override def build(p: LogicalSection): EventModel = {
      p.keyForModel match {
        case "event" =>
          if (p.sections.nonEmpty) {
            val defs = p.sections.map(_parse_reception_definition).filter(_.name.nonEmpty).toVector
            EventModel(receptionDefinitions = defs)
          } else {
            super.build(p)
          }
        case "routing" =>
          val defs = p.sections.map(_parse_routing_definition).filter(_.name.nonEmpty).toVector
          EventModel(routingDefinitions = defs)
        case "subscription" =>
          val defs = p.sections.map(_parse_subscription_definition).filter(_.name.nonEmpty).toVector
          EventModel(subscriptionDefinitions = defs)
        case _ =>
          super.build(p)
      }
    }

    protected def to_Model(p: Option[EventRule]): EventModel = p.map(EventModel.apply(_)).orZero

    protected def class_Name(p: EventClazz): String = p.name

    protected def to_Model(p: TreeMap[EventClazz]): EventModel = EventModel(EventRule(p))

    protected def parse_Flat(p: String): Option[EventRule] = EventRule.parse(p).toOption

    protected def parse_Subsection(name: String, p: String): Option[EventClazz] =
      Some(EventClazz(name))

    private def _parse_reception_definition(
      p: LogicalSection
    ): SchemaModel.EventDefinition = {
      val kv = _key_values(p.text)
      val category = kv.collectFirst {
        case (k, v) if k == "category" => v
      }.map(_normalize_event_category(_, p.nameForModel)).getOrElse("NonActionEvent")
      val kind = kv.collectFirst {
        case (k, v) if k == "kind" => v
      }.map(_.trim).filterNot(_.isEmpty)
      val actionname = kv.collectFirst {
        case (k, v) if k == "actionname" || k == "action_name" => v
      }.map(_.trim).filterNot(_.isEmpty)
      val priority = kv.collectFirst {
        case (k, v) if k == "priority" => _to_int_or_zero(v)
      }.getOrElse(0)
      val selectors = kv.collect {
        case (k, v) if k == "selector" =>
          _selector_pair(v)
      }.toMap
      SchemaModel.EventDefinition(
        name = p.nameForModel.trim,
        category = category,
        kind = kind,
        selectors = selectors,
        actionName = actionname,
        priority = priority
      )
    }

    private def _parse_routing_definition(
      p: LogicalSection
    ): SchemaModel.RoutingDefinition = {
      val kv = _merged_key_values(p)
      val when = kv.collectFirst {
        case (k, v) if k == "when" => v
      }.map(_.trim).filterNot(_.isEmpty)
      val topic = kv.collectFirst {
        case (k, v) if k == "topic" => v
      }.map(_.trim).filterNot(_.isEmpty)
      val service = kv.collectFirst {
        case (k, v) if k == "service" => v
      }.map(_.trim).filterNot(_.isEmpty)
      val partition = kv.collectFirst {
        case (k, v) if k == "partition" => v
      }.map(_.trim).filterNot(_.isEmpty)
      SchemaModel.RoutingDefinition(
        name = p.nameForModel.trim,
        when = when,
        topic = topic,
        service = service,
        partition = partition
      )
    }

    private def _parse_subscription_definition(
      p: LogicalSection
    ): SchemaModel.SubscriptionDefinition = {
      val kv = _merged_key_values(p)
      val eventname = _value_opt(kv, "event", "on")
      val route = _value_opt(kv, "route")
      val entityname = _value_opt(kv, "entity")
      val target = _value_opt(kv, "target")
      val targets = _targets(kv)
      val selector = _value_opt(kv, "selector")
      val actionname = _value_opt(kv, "action", "actionname", "action_name")
      val bound = _value_opt(kv, "declaredtargetupperbound", "declared_target_upper_bound", "bound").map(_to_int_or_zero)
      val activation = _value_opt(kv, "activation")
      SchemaModel.SubscriptionDefinition(
        name = p.nameForModel.trim,
        eventName = eventname,
        route = route,
        entityName = entityname,
        target = target,
        targets = targets,
        selector = selector,
        actionName = actionname,
        declaredTargetUpperBound = bound,
        activation = activation
      )
    }

    private def _normalize_event_category(p: String, eventname: String): String =
      p.trim.toLowerCase match {
        case "actionevent" | "action" => "ActionEvent"
        case "nonactionevent" | "non-action" | "nonaction" => "NonActionEvent"
        case s =>
          RAISE.syntaxErrorFault(s"Event '$eventname' has invalid category: '$s'. Use ActionEvent or NonActionEvent.")
      }

    private def _selector_pair(p: String): (String, String) = {
      val i = p.indexOf("=")
      if (i <= 0)
        p.trim -> ""
      else
        p.substring(0, i).trim -> p.substring(i + 1).trim
    }

    private def _to_int_or_zero(p: String): Int =
      scala.util.Try(p.trim.toInt).getOrElse(0)

    private def _value_opt(
      kv: Vector[(String, String)],
      ks: String*
    ): Option[String] =
      kv.collectFirst {
        case (k, v) if ks.contains(k) => v.trim
      }.filterNot(_.isEmpty)

    private def _targets(
      kv: Vector[(String, String)]
    ): Vector[String] =
      _value_opt(kv, "targets").map(_parse_targets).getOrElse(Vector.empty)

    private def _parse_targets(
      p: String
    ): Vector[String] = {
      val s = p.trim.stripPrefix("[").stripSuffix("]")
      s.split(",").toVector.map(_.trim).filter(_.nonEmpty)
    }

    private def _merged_key_values(
      p: LogicalSection
    ): Vector[(String, String)] = {
      val fromtext = _key_values(p.text)
      val fromsections = p.sections.toVector.flatMap { s =>
        val fromsectiontext = _key_values(s.text)
        if (fromsectiontext.nonEmpty)
          fromsectiontext
        else {
          val key = s.keyForModel.toLowerCase
          val body = s.text.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
          if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
        }
      }
      fromtext ++ fromsections
    }

    private def _key_values(p: String): Vector[(String, String)] =
      CmlSectionFormat.keyValues(p)

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
