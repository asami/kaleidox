package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.smartdox.parser.Dox2Parser
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.kaleidox.CmlSectionFormat
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model
import org.goldenport.parser.LogicalSection

/*
 * @since   Mar. 22, 2026
 *  version Mar. 24, 2026
 *  version Apr.  6, 2026
 * @version Jul. 16, 2026
 * @author  ASAMI, Tomoharu
 */
case class ComponentSubsystemModel(
  visions: Vector[ComponentSubsystemModel.VisionDefinition] = Vector.empty,
  contexts: Vector[ComponentSubsystemModel.ContextDefinition] = Vector.empty,
  systemContexts: Vector[ComponentSubsystemModel.SystemContextDefinition] = Vector.empty,
  contextMaps: Vector[ComponentSubsystemModel.ContextMapDefinition] = Vector.empty,
  useCases: Vector[ComponentSubsystemModel.UseCaseDefinition] = Vector.empty,
  capabilities: Vector[ComponentSubsystemModel.CapabilityDefinition] = Vector.empty,
  qualities: Vector[ComponentSubsystemModel.QualityDefinition] = Vector.empty,
  constraints: Vector[ComponentSubsystemModel.ConstraintDefinition] = Vector.empty,
  components: Vector[ComponentSubsystemModel.ComponentDefinition] = Vector.empty,
  componentlets: Vector[ComponentSubsystemModel.ComponentletDefinition] = Vector.empty,
  extensionPoints: Vector[ComponentSubsystemModel.ExtensionPointDefinition] = Vector.empty,
  subsystems: Vector[ComponentSubsystemModel.SubsystemDefinition] = Vector.empty,
  description: Description = Description.name("component-subsystem")
) extends Model.ISubModel {
  protected def display_String: String =
    ((if (visions.nonEmpty) Vector("vision") else Vector.empty) ++
      (if (contexts.nonEmpty) Vector("context") else Vector.empty) ++
      (if (systemContexts.nonEmpty) Vector("systemcontext") else Vector.empty) ++
      (if (contextMaps.nonEmpty) Vector("contextmap") else Vector.empty) ++
      (if (useCases.nonEmpty) Vector("usecase") else Vector.empty) ++
      (if (capabilities.nonEmpty) Vector("capability") else Vector.empty) ++
      (if (qualities.nonEmpty) Vector("quality") else Vector.empty) ++
      (if (constraints.nonEmpty) Vector("constraint") else Vector.empty) ++
      components.map(_.name) ++ subsystems.map(_.name)).mkString(",")

  protected def print_String: String =
    display_String

  protected def show_String: String =
    display_String

  def isEmpty: Boolean =
    visions.isEmpty && contexts.isEmpty && systemContexts.isEmpty && contextMaps.isEmpty && useCases.isEmpty && capabilities.isEmpty && qualities.isEmpty && constraints.isEmpty && components.isEmpty && componentlets.isEmpty && extensionPoints.isEmpty && subsystems.isEmpty

  def toOption: Option[ComponentSubsystemModel] =
    if (isEmpty) None else Some(this)

  def +(rhs: ComponentSubsystemModel): ComponentSubsystemModel =
    copy(
      visions = _dedupe_by_name(visions ++ rhs.visions),
      contexts = _dedupe_by_name(contexts ++ rhs.contexts),
      systemContexts = _dedupe_by_name(systemContexts ++ rhs.systemContexts),
      contextMaps = _dedupe_by_name(contextMaps ++ rhs.contextMaps),
      useCases = _dedupe_by_name(useCases ++ rhs.useCases),
      capabilities = _dedupe_by_name(capabilities ++ rhs.capabilities),
      qualities = _dedupe_by_name(qualities ++ rhs.qualities),
      constraints = _dedupe_by_name(constraints ++ rhs.constraints),
      components = _dedupe_by_name(components ++ rhs.components),
      componentlets = _dedupe_by_name(componentlets ++ rhs.componentlets),
      extensionPoints = _dedupe_by_name(extensionPoints ++ rhs.extensionPoints),
      subsystems = _dedupe_by_name(subsystems ++ rhs.subsystems)
    )

  private def _dedupe_by_name[A <: ComponentSubsystemModel.NamedDefinition](
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
    extensionBindings: Map[String, String] = Map.empty,
    services: Vector[ComponentServiceDefinition] = Vector.empty,
    description: Option[String] = None,
    useCases: Vector[UseCaseDefinition] = Vector.empty
  ) extends NamedDefinition

  final case class ComponentServiceDefinition(
    name: String,
    spiStandard: Option[String] = None,
    spiDirection: String = "provides",
    spiSocket: Boolean = false,
    spiMultiplicity: Option[String] = None,
    spiRequired: Boolean = false,
    spiApiName: Option[String] = None,
    spiComponentApi: Option[String] = None
  ) extends NamedDefinition

  final case class VisionDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  ) extends NamedDefinition

  final case class ContextDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  ) extends NamedDefinition

  final case class SystemContextDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  ) extends NamedDefinition

  final case class ContextMapDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None
  ) extends NamedDefinition

  final case class UseCaseDefinition(
    name: String,
    id: Option[String] = None,
    summary: Option[String] = None,
    description: Option[String] = None,
    actor: Option[String] = None,
    primaryActor: Option[String] = None,
    secondaryActor: Option[String] = None,
    supportingActor: Option[String] = None,
    stakeholder: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None,
    trigger: Option[String] = None,
    priority: Option[String] = None,
    status: Option[String] = None,
    scenarios: Vector[UseCaseScenario] = Vector.empty
  ) extends NamedDefinition

  final case class UseCaseScenario(
    name: String,
    kind: String = "main",
    summary: Option[String] = None,
    description: Option[String] = None,
    steps: Vector[String] = Vector.empty,
    alternates: Vector[String] = Vector.empty,
    exceptions: Vector[String] = Vector.empty
  )

  final case class CapabilityDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    actor: Option[String] = None,
    primaryActor: Option[String] = None,
    secondaryActor: Option[String] = None,
    supportingActor: Option[String] = None,
    stakeholder: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  ) extends NamedDefinition

  final case class QualityDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
  ) extends NamedDefinition

  final case class ConstraintDefinition(
    name: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    goal: Option[String] = None,
    precondition: Option[String] = None,
    postcondition: Option[String] = None
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
      case "vision" =>
        ComponentSubsystemModel(
          visions = p.sections.toVector.map(_parse_vision_definition)
        )
      case "context" =>
        ComponentSubsystemModel(
          contexts = p.sections.toVector.map(_parse_context_definition)
        )
      case key if _is_system_context_key(key) =>
        ComponentSubsystemModel(
          systemContexts = p.sections.toVector.map(_parse_system_context_definition)
        )
      case key if _is_context_map_key(key) =>
        ComponentSubsystemModel(
          contextMaps = p.sections.toVector.map(_parse_context_map_definition)
        )
      case key if _is_use_case_key(key) =>
        ComponentSubsystemModel(
          useCases = p.sections.toVector.map(_parse_use_case_definition)
        )
      case "capability" =>
        ComponentSubsystemModel(
          capabilities = p.sections.toVector.map(_parse_capability_definition)
        )
      case "quality" =>
        ComponentSubsystemModel(
          qualities = p.sections.toVector.map(_parse_quality_definition)
        )
      case "constraint" =>
        ComponentSubsystemModel(
          constraints = p.sections.toVector.map(_parse_constraint_definition)
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
    val coordinates = _coordinate_values(p, CmlSectionFormat.directKeyValues(p))
    val componentlets = _section_name_values(p, Set("componentlet", "componentlets"))
    val extensionpoints = _section_name_values(p, Set("extensionpoint", "extensionpoints"))
    val extensionbindings =
      _key_values_in_sections(p, Set("extensionbinding", "extensionbindings", "binding", "bindings")).toMap
    val services = _component_service_definitions(p)

    ComponentDefinition(
      name = _require_name(p.nameForModel, "component"),
      packageName = _value_opt(kv, "package", "componentpackage", "component_package", "component package"),
      coordinates = coordinates,
      componentlets = componentlets,
      extensionPoints = extensionpoints,
      extensionBindings = extensionbindings,
      services = services,
      description = _value_opt(kv, "description"),
      useCases = _use_case_definitions(p)
    )
  }

  private def _component_service_definitions(
    p: Section
  ): Vector[ComponentServiceDefinition] =
    p.sections.toVector.filter(_.keyForModel.equalsIgnoreCase("service")).flatMap { section =>
      section.sections.toVector.map(_parse_component_service_definition)
    }

  private def _parse_component_service_definition(
    p: Section
  ): ComponentServiceDefinition = {
    val name = _require_name(p.nameForModel, "component service")
    val kv = _merged_key_values(p)
    val spidirection = _value_opt(kv, "spi-direction").map(_unquote).map(_.toLowerCase).getOrElse("provides")
    val spisocket = _boolean_value(kv, "spi-socket").getOrElse(false)
    val spimultiplicity = _value_opt(kv, "spi-multiplicity").map(_unquote)
    val spirequired = _boolean_value(kv, "spi-required").getOrElse(false)
    val spistandard = _value_opt(kv, "spi-standard").map(_unquote)
    val spiapiname = _value_opt(kv, "spi-api-name").map(_unquote)
    val spicomponentapi = _value_opt(kv, "spi-component-api").map(_unquote)
    val hasspimetadata = kv.exists(_._1.startsWith("spi-"))

    if (!Set("provides", "requires").contains(spidirection))
      _raise(s"Component service '$name' has invalid spi-direction '$spidirection'.")
    if (spidirection == "provides" && spimultiplicity.nonEmpty)
      _raise(s"Component service '$name' provider declaration does not accept spi-multiplicity.")
    if (spidirection == "provides" && spicomponentapi.nonEmpty)
      _raise(s"Component service '$name' provider declaration does not accept spi-component-api.")
    if (spidirection == "requires" && spisocket)
      _raise(s"Component service '$name' consumer declaration does not accept spi-socket=true.")
    if (spidirection == "requires" && spiapiname.nonEmpty)
      _raise(s"Component service '$name' consumer declaration does not accept spi-api-name.")
    if (spidirection == "requires" && spistandard.nonEmpty && spicomponentapi.nonEmpty)
      _raise(s"Component service '$name' cannot require spi-standard and spi-component-api together.")
    if (spidirection == "requires" && spistandard.isEmpty && spicomponentapi.isEmpty)
      _raise(s"Component service '$name' requires spi-standard or spi-component-api.")
    if (spidirection == "requires" && !spimultiplicity.forall(Set("1", "?", "*").contains))
      _raise(s"Component service '$name' has invalid spi-multiplicity '${spimultiplicity.getOrElse("")}'.")
    if (spirequired && spimultiplicity != Some("*"))
      _raise(s"Component service '$name' spi-required=true is valid only with spi-multiplicity '*'.")
    if (spiapiname.nonEmpty && !spisocket)
      _raise(s"Component service '$name' spi-api-name requires spi-socket=true.")
    if (hasspimetadata && spidirection == "provides" && !spisocket && spistandard.isEmpty)
      _raise(s"Component service '$name' provider declaration requires spi-socket=true or spi-standard.")

    ComponentServiceDefinition(
      name = name,
      spiStandard = spistandard,
      spiDirection = spidirection,
      spiSocket = spisocket,
      spiMultiplicity = spimultiplicity,
      spiRequired = spirequired,
      spiApiName = spiapiname,
      spiComponentApi = spicomponentapi
    )
  }

  private def _parse_vision_definition(
    p: Section
  ): VisionDefinition = {
    val kv = _merged_key_values(p)
    VisionDefinition(
      name = _require_name(p.nameForModel, "vision"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      goal = _value_opt(kv, "goal"),
      precondition = _value_opt(kv, "precondition", "pre-condition"),
      postcondition = _value_opt(kv, "postcondition", "post-condition")
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

  private def _parse_context_definition(
    p: Section
  ): ContextDefinition = {
    val kv = _merged_key_values(p)
    ContextDefinition(
      name = _require_name(p.nameForModel, "context"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description")
    )
  }

  private def _parse_system_context_definition(
    p: Section
  ): SystemContextDefinition = {
    val kv = _merged_key_values(p)
    SystemContextDefinition(
      name = _require_name(p.nameForModel, "system context"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description")
    )
  }

  private def _parse_context_map_definition(
    p: Section
  ): ContextMapDefinition = {
    val kv = _merged_key_values(p)
    ContextMapDefinition(
      name = _require_name(p.nameForModel, "context map"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description")
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

  private def _use_case_definitions(
    p: Section
  ): Vector[UseCaseDefinition] =
    p.sections.toVector.filter(s => _is_use_case_key(s.keyForModel)).flatMap { s =>
      s.sections.toVector.map(_parse_use_case_definition)
    }

  private def _parse_use_case_definition(
    p: Section
  ): UseCaseDefinition = {
    val kv = _merged_key_values(p)
    UseCaseDefinition(
      name = _require_name(p.nameForModel, "use case"),
      id = _value_opt(kv, "id"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      actor = _value_opt(kv, "actor"),
      primaryActor = _value_opt(kv, "primary actor", "primaryactor", "primary_actor"),
      secondaryActor = _value_opt(kv, "secondary actor", "secondaryactor", "secondary_actor"),
      supportingActor = _value_opt(kv, "supporting actor", "supportingactor", "supporting_actor"),
      stakeholder = _value_opt(kv, "stakeholder", "stakeholders"),
      goal = _value_opt(kv, "goal"),
      precondition = _value_opt(kv, "precondition", "pre-condition"),
      postcondition = _value_opt(kv, "postcondition", "post-condition"),
      trigger = _value_opt(kv, "trigger"),
      priority = _value_opt(kv, "priority"),
      status = _value_opt(kv, "status"),
      scenarios = _use_case_scenarios(p)
    )
  }

  private def _parse_capability_definition(
    p: Section
  ): CapabilityDefinition = {
    val kv = _merged_key_values(p)
    CapabilityDefinition(
      name = _require_name(p.nameForModel, "capability"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      actor = _value_opt(kv, "actor"),
      primaryActor = _value_opt(kv, "primary_actor", "primary actor"),
      secondaryActor = _value_opt(kv, "secondary_actor", "secondary actor"),
      supportingActor = _value_opt(kv, "supporting_actor", "supporting actor"),
      stakeholder = _value_opt(kv, "stakeholder"),
      goal = _value_opt(kv, "goal"),
      precondition = _value_opt(kv, "precondition", "pre-condition"),
      postcondition = _value_opt(kv, "postcondition", "post-condition")
    )
  }

  private def _parse_quality_definition(
    p: Section
  ): QualityDefinition = {
    val kv = _merged_key_values(p)
    QualityDefinition(
      name = _require_name(p.nameForModel, "quality"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      goal = _value_opt(kv, "goal"),
      precondition = _value_opt(kv, "precondition", "pre-condition"),
      postcondition = _value_opt(kv, "postcondition", "post-condition")
    )
  }

  private def _parse_constraint_definition(
    p: Section
  ): ConstraintDefinition = {
    val kv = _merged_key_values(p)
    ConstraintDefinition(
      name = _require_name(p.nameForModel, "constraint"),
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      goal = _value_opt(kv, "goal"),
      precondition = _value_opt(kv, "precondition", "pre-condition"),
      postcondition = _value_opt(kv, "postcondition", "post-condition")
    )
  }

  private def _use_case_scenarios(
    p: Section
  ): Vector[UseCaseScenario] =
    p.sections.toVector.flatMap { section =>
      _flow_kind(section.keyForModel) match {
        case Some(kind) =>
          val xs = section.sections.toVector
          if (xs.nonEmpty)
            xs.map(_parse_use_case_scenario(_, kind))
          else
            Vector(_parse_use_case_scenario(section, kind))
        case None => Vector.empty
      }
    }

  private def _is_use_case_key(p: String): Boolean = {
    val s = Option(p).map(_.trim.toLowerCase).getOrElse("")
    s == "use case" || s == "usecase"
  }

  private def _is_system_context_key(p: String): Boolean = {
    val s = Option(p).map(_.trim.toLowerCase).getOrElse("")
    s == "system context" || s == "systemcontext"
  }

  private def _is_context_map_key(p: String): Boolean = {
    val s = Option(p).map(_.trim.toLowerCase).getOrElse("")
    s == "context map" || s == "contextmap"
  }

  private def _parse_use_case_scenario(
    p: Section,
    kind: String = "main"
  ): UseCaseScenario = {
    val kv = _merged_key_values(p)
    val steps = _scenario_steps(p)
    UseCaseScenario(
      name = _require_name(p.nameForModel, "use case scenario"),
      kind = kind,
      summary = _value_opt(kv, "summary"),
      description = _value_opt(kv, "description"),
      steps = steps,
      alternates = _scenario_value_sections(p, "alternate"),
      exceptions = _scenario_value_sections(p, "exception")
    )
  }

  private def _flow_kind(p: String): Option[String] =
    Option(p).map(_.trim.toLowerCase(java.util.Locale.ROOT).replace("_", " ").replace("-", " ")).collect {
      case "scenario" | "main flow" | "mainflow" => "main"
      case "alternate flow" | "alternative flow" | "alternateflow" | "alternativeflow" => "alternate"
      case "exception flow" | "exceptionflow" => "exception"
    }

  private def _scenario_steps(
    p: Section
  ): Vector[String] = {
    val explicit = _scenario_value_sections(p, "step")
    if (explicit.nonEmpty)
      explicit
    else {
      val lines = p.toText.linesIterator.map(_.trim).filterNot(_.isEmpty).toVector
      val xs =
        if (lines.size > 1)
          lines.flatMap(_split_scenario_steps)
        else
          lines.headOption.map(_split_scenario_steps).getOrElse(Vector.empty)
      if (xs.nonEmpty) xs else lines
    }
  }

  private def _scenario_value_sections(
    p: Section,
    key: String
  ): Vector[String] =
    p.sections.toVector.filter(_.keyForModel.equalsIgnoreCase(key)).flatMap { s =>
      val lines = s.toText.linesIterator.map(_.trim).filterNot(_.isEmpty).toVector
      val xs =
        if (lines.size > 1)
          lines.flatMap(_split_scenario_steps)
        else
          lines.headOption.map(_split_scenario_steps).getOrElse(Vector.empty)
      if (xs.nonEmpty) xs else lines
    }

  private def _split_scenario_steps(
    p: String
  ): Vector[String] = {
    val s = p.trim
    val numbered = "(?=\\d+\\.\\s+)".r.split(s).toVector.map(_.trim).filterNot(_.isEmpty)
    if (numbered.size > 1)
      numbered
    else
      _split_sentence_steps(s)
  }

  private def _split_sentence_steps(
    p: String
  ): Vector[String] = {
    val xs = "(?<=[.!?])(?=[A-Z])".r.split(p).toVector.map(_.trim).filterNot(_.isEmpty)
    if (xs.size > 1) xs else Vector(p)
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

  private def _boolean_value(
    kv: Vector[(String, String)],
    keys: String*
  ): Option[Boolean] =
    _value_opt(kv, keys: _*).map { value =>
      value.trim.toLowerCase match {
        case "true" | "yes" | "on" | "1" => true
        case "false" | "no" | "off" | "0" => false
        case _ => _raise(s"Invalid boolean value '$value' for ${keys.headOption.getOrElse("property")}.")
      }
    }

  private def _unquote(value: String): String = {
    val s = value.trim
    if (s.length >= 2 && ((s.head == '"' && s.last == '"') || (s.head == '\'' && s.last == '\'')))
      s.substring(1, s.length - 1)
    else
      s
  }

  private def _merged_key_values(
    p: Section
  ): Vector[(String, String)] =
    CmlSectionFormat.keyValues(p)

  private def _key_values(
    p: String
  ): Vector[(String, String)] =
    CmlSectionFormat.keyValues(p)

  private def _value_lines(
    p: String
  ): Vector[String] =
    CmlSectionFormat.valueLines(p)

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
