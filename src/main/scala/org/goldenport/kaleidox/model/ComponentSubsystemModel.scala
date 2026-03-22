package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.smartdox.parser.Dox2Parser
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.parser.LogicalSection

/*
 * @since   Mar. 22, 2026
 * @version Mar. 22, 2026
 * @author  ASAMI, Tomoharu
 */
case class ComponentSubsystemModel(
  components: Vector[ComponentSubsystemModel.ComponentDefinition] = Vector.empty,
  componentlets: Vector[ComponentSubsystemModel.ComponentletDefinition] = Vector.empty,
  extensionPoints: Vector[ComponentSubsystemModel.ExtensionPointDefinition] = Vector.empty,
  subsystems: Vector[ComponentSubsystemModel.SubsystemDefinition] = Vector.empty,
  description: Description = Description.name("component-subsystem")
) extends Model.ISubModel {
  protected def display_String: String =
    (components.map(_.name) ++ subsystems.map(_.name)).mkString(",")

  protected def print_String: String =
    display_String

  protected def show_String: String =
    display_String

  def isEmpty: Boolean =
    components.isEmpty && componentlets.isEmpty && extensionPoints.isEmpty && subsystems.isEmpty

  def toOption: Option[ComponentSubsystemModel] =
    if (isEmpty) None else Some(this)

  def +(rhs: ComponentSubsystemModel): ComponentSubsystemModel =
    copy(
      components = _dedupeByName(components ++ rhs.components),
      componentlets = _dedupeByName(componentlets ++ rhs.componentlets),
      extensionPoints = _dedupeByName(extensionPoints ++ rhs.extensionPoints),
      subsystems = _dedupeByName(subsystems ++ rhs.subsystems)
    )

  private def _dedupeByName[A <: ComponentSubsystemModel.NamedDefinition](
    xs: Vector[A]
  ): Vector[A] =
    xs.foldLeft(Vector.empty[A]) { (z, x) =>
      if (z.exists(_.name == x.name)) z else z :+ x
    }
}

object ComponentSubsystemModel {
  sealed trait NamedDefinition {
    def name: String
  }

  final case class Coordinate(
    group: String,
    artifact: String,
    version: String
  ) {
    def asString: String = s"${group}:${artifact}:${version}"
  }

  final case class ComponentDefinition(
    name: String,
    packageName: Option[String] = None,
    coordinates: Vector[Coordinate] = Vector.empty,
    componentlets: Vector[String] = Vector.empty,
    extensionPoints: Vector[String] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty
  ) extends NamedDefinition

  final case class ComponentletDefinition(
    name: String,
    component: Option[String] = None,
    kind: Option[String] = None
  ) extends NamedDefinition

  final case class ExtensionPointDefinition(
    name: String,
    component: Option[String] = None,
    interfaceType: Option[String] = None,
    defaultBinding: Option[String] = None
  ) extends NamedDefinition

  final case class SubsystemDefinition(
    name: String,
    components: Vector[Coordinate] = Vector.empty,
    extensionBindings: Map[String, String] = Map.empty,
    config: Map[String, String] = Map.empty
  ) extends NamedDefinition

  implicit object ComponentSubsystemModelMonoid extends Monoid[ComponentSubsystemModel] {
    def zero = ComponentSubsystemModel.empty
    def append(lhs: ComponentSubsystemModel, rhs: => ComponentSubsystemModel) = lhs + rhs
  }

  val empty = ComponentSubsystemModel()

  def create(config: Config, p: LogicalSection): ComponentSubsystemModel = {
    val dox = Dox2Parser.parse(config.doxConfig, p)
    _parse_dox(dox)
  }

  private def _parse_dox(p: Dox): ComponentSubsystemModel =
    p match {
      case m: Section => _parse_section(m)
      case m => m.elements.foldMap(_parse_dox)
    }

  private def _parse_section(p: Section): ComponentSubsystemModel =
    p.keyForModel match {
      case "component" =>
        val defs = p.sections.toVector.map(_parse_component_definition)
        val childlets = defs.flatMap { x =>
          x.componentlets.map(n => ComponentletDefinition(n, component = Some(x.name)))
        }
        val childexts = defs.flatMap { x =>
          x.extensionPoints.map(n => ExtensionPointDefinition(n, component = Some(x.name)))
        }
        ComponentSubsystemModel(
          components = defs,
          componentlets = childlets,
          extensionPoints = childexts
        )
      case "componentlet" =>
        ComponentSubsystemModel(
          componentlets = p.sections.toVector.map(_parse_componentlet_definition)
        )
      case "extensionpoint" =>
        ComponentSubsystemModel(
          extensionPoints = p.sections.toVector.map(_parse_extension_point_definition)
        )
      case "subsystem" =>
        ComponentSubsystemModel(
          subsystems = p.sections.toVector.map(_parse_subsystem_definition)
        )
      case _ =>
        p.sections.foldMap(_parse_section)
    }

  private def _parse_component_definition(
    p: Section
  ): ComponentDefinition = {
    val kv = _merged_key_values(p)
    val coordinates = _coordinate_values(p, kv)
    val componentlets = _section_name_values(p, Set("componentlet", "componentlets"))
    val extensionpoints = _section_name_values(p, Set("extensionpoint", "extensionpoints"))
    val extensionbindings =
      _key_values_in_sections(p, Set("extensionbinding", "extensionbindings", "binding", "bindings")).toMap

    ComponentDefinition(
      name = _require_name(p.nameForModel, "component"),
      packageName = _value_opt(kv, "package", "componentpackage", "component_package", "component package"),
      coordinates = coordinates,
      componentlets = componentlets,
      extensionPoints = extensionpoints,
      extensionBindings = extensionbindings
    )
  }

  private def _parse_componentlet_definition(
    p: Section
  ): ComponentletDefinition = {
    val kv = _merged_key_values(p)
    ComponentletDefinition(
      name = _require_name(p.nameForModel, "componentlet"),
      component = _value_opt(kv, "component"),
      kind = _value_opt(kv, "kind")
    )
  }

  private def _parse_extension_point_definition(
    p: Section
  ): ExtensionPointDefinition = {
    val kv = _merged_key_values(p)
    ExtensionPointDefinition(
      name = _require_name(p.nameForModel, "extensionpoint"),
      component = _value_opt(kv, "component"),
      interfaceType = _value_opt(kv, "interface", "interfacetype", "interface_type"),
      defaultBinding = _value_opt(kv, "default", "defaultbinding", "default_binding")
    )
  }

  private def _parse_subsystem_definition(
    p: Section
  ): SubsystemDefinition = {
    val kv = _merged_key_values(p)
    val components =
      _distinct_stable(
        _values_in_sections(p, Set("component", "components")) ++
          _value_multi(kv, "component", "components")
      ).map(_parse_coordinate(_, p.nameForModel))
    val extensionbindings =
      _key_values_in_sections(p, Set("extensionbinding", "extensionbindings", "binding", "bindings")).toMap
    val config =
      _key_values_in_sections(p, Set("config", "configuration")).toMap

    SubsystemDefinition(
      name = _require_name(p.nameForModel, "subsystem"),
      components = components,
      extensionBindings = extensionbindings,
      config = config
    )
  }

  private def _require_name(p: String, kind: String): String = {
    val s = Option(p).map(_.trim).getOrElse("")
    if (s.isEmpty)
      _raise(s"${kind} name is required.")
    else
      s
  }

  private def _parse_coordinate(
    p: String,
    owner: String
  ): Coordinate = {
    val raw = Option(p).map(_.trim).getOrElse("")
    val s0 = _line_value(raw)
    val s = _normalize_coordinate_text(s0)
    val a = s.split(":", -1).toVector
    if (a.lengthCompare(3) != 0 || a.exists(Strings.blankp))
      _raise(s"'${owner}' has invalid coordinate '${s}'. Use 'group:artifact:version'.")
    else
      Coordinate(a(0).trim, a(1).trim, a(2).trim)
  }

  private def _normalize_coordinate_text(
    p: String
  ): String = {
    val s = p.trim
    val lower = s.toLowerCase
    if (lower.startsWith("coordinate"))
      s.drop("coordinate".length).stripPrefix(":").stripPrefix(":").trim
    else if (lower.startsWith("component"))
      s.drop("component".length).stripPrefix(":").stripPrefix(":").trim
    else
      s
  }

  private def _section_name_values(
    p: Section,
    keys: Set[String]
  ): Vector[String] =
    p.sections.toVector.filter(s => keys.contains(s.keyForModel.toLowerCase)).flatMap { s =>
      if (s.sections.nonEmpty)
        s.sections.map(_.nameForModel.trim).filterNot(_.isEmpty).toVector
      else
        _value_lines(s.toText)
    }

  private def _values_in_sections(
    p: Section,
    keys: Set[String]
  ): Vector[String] =
    p.sections.toVector.filter(s => keys.contains(s.keyForModel.toLowerCase)).flatMap { s =>
      if (s.sections.nonEmpty)
        s.sections.map(_.nameForModel.trim).filterNot(_.isEmpty).toVector
      else
        _value_lines(s.toText).map(_line_value)
    }

  private def _coordinate_values(
    p: Section,
    kv: Vector[(String, String)]
  ): Vector[Coordinate] = {
    val fromkv = _value_multi(kv, "coordinate", "coordinates", "dependency", "dependencies")
    val fromsections = _values_in_sections(p, Set("coordinate", "coordinates", "dependency", "dependencies"))
    _distinct_stable(fromkv ++ fromsections).map(_parse_coordinate(_, p.nameForModel))
  }

  private def _line_value(
    p: String
  ): String =
    _key_values(p).headOption.map(_._2).getOrElse(p.trim)

  private def _binding_pairs(
    p: String
  ): Vector[(String, String)] = {
    val kv = _key_values(p)
    if (kv.nonEmpty)
      kv
    else {
      val s = p.trim
      val i = s.indexOf("=")
      if (i <= 0)
        Vector.empty
      else {
        val k = s.substring(0, i).trim
        val v = s.substring(i + 1).trim
        if (k.isEmpty || v.isEmpty)
          Vector.empty
        else
          Vector(k.toLowerCase -> v)
      }
    }
  }

  private def _key_values_in_sections(
    p: Section,
    keys: Set[String]
  ): Vector[(String, String)] =
    p.sections.toVector.filter(s => keys.contains(s.keyForModel.toLowerCase)).flatMap { s =>
      val fromtext = _key_values(s.toText)
      if (fromtext.nonEmpty)
        fromtext
      else {
        val fromchildren = s.sections.toVector.flatMap { c =>
          val key = c.nameForModel.trim
          val value = _line_value(c.toText)
          if (key.isEmpty || value.isEmpty)
            Vector.empty
          else
            Vector(key.toLowerCase -> value)
        }
        if (fromchildren.nonEmpty)
          fromchildren
        else {
          _value_lines(s.toText).flatMap(_binding_pairs)
        }
      }
    }

  private def _value_multi(
    kv: Vector[(String, String)],
    ks: String*
  ): Vector[String] =
    kv.collect {
      case (k, v) if ks.contains(k) => v
    }

  private def _value_opt(
    kv: Vector[(String, String)],
    ks: String*
  ): Option[String] =
    kv.collectFirst {
      case (k, v) if ks.contains(k) => v.trim
    }.filterNot(Strings.blankp)

  private def _merged_key_values(
    p: Section
  ): Vector[(String, String)] = {
    val fromtext = _key_values(p.toText)
    val fromsections = p.sections.toVector.flatMap { s =>
      val fromsectiontext = _key_values(s.toText)
      if (fromsectiontext.nonEmpty)
        fromsectiontext
      else {
        val key = s.keyForModel.toLowerCase
        val body = s.toText.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
        if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
      }
    }
    fromtext ++ fromsections
  }

  private def _key_values(
    p: String
  ): Vector[(String, String)] =
    p.split("\\r?\\n").toVector.flatMap { x =>
      val s0 = x.trim
      if (s0.isEmpty)
        None
      else {
        val s = if (s0.startsWith("-")) s0.drop(1).trim else s0
        val i1 = s.indexOf("::")
        val i2 = s.indexOf("=")
        val i =
          if (i1 > 0) i1
          else if (i2 > 0) i2
          else -1
        if (i <= 0)
          None
        else {
          val k = s.substring(0, i).trim.toLowerCase
          val v = if (i == i1) s.substring(i + 2).trim else s.substring(i + 1).trim
          if (k.isEmpty || v.isEmpty)
            None
          else
            Some(k -> v)
        }
      }
    }

  private def _value_lines(
    p: String
  ): Vector[String] =
    p.split("\\r?\\n").toVector.map(_.trim).map { x =>
      if (x.startsWith("-")) x.drop(1).trim else x
    }.filterNot(_.isEmpty)

  private def _distinct_stable(
    p: Vector[String]
  ): Vector[String] =
    p.foldLeft(Vector.empty[String]) { (z, x) =>
      val s = x.trim
      if (s.isEmpty || z.contains(s))
        z
      else
        z :+ s
    }

  private def _raise(message: String): Nothing =
    RAISE.syntaxErrorFault(message)
}
