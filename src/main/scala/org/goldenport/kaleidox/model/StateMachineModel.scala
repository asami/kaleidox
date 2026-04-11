package org.goldenport.kaleidox.model

import scalaz.{State => _, _}, Scalaz._
import org.smartdox.Description
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.context.Consequence
import org.goldenport.sm._
import org.goldenport.kaleidox._

/*
 * @since   May.  4, 2021
 *  version May. 23, 2021
 *  version Jun. 27, 2021
 *  version Aug. 21, 2023
 *  version Oct. 15, 2023
 * @version Apr. 12, 2026
 * @author  ASAMI, Tomoharu
 */
case class StateMachineModel(
  classes: TreeMap[StateMachineClass] = TreeMap.empty,
  description: Description = Description.name("stateMachine")
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString(",")

  protected def print_String: String = classes.values.map(x => x.name).mkString(",")

  protected def show_String: String = classes.values.map(x => x.name).mkString(",")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[StateMachineModel] = if (isEmpty) None else Some(this)

  def +(rhs: StateMachineModel): StateMachineModel = copy(classes = classes + rhs.classes)

  def getClass(name: String): Option[StateMachineClass] = classes.get(name)
}

object StateMachineModel {
  implicit object StateMachineModelMonoid extends Monoid[StateMachineModel] {
    def zero = StateMachineModel.empty
    def append(lhs: StateMachineModel, rhs: => StateMachineModel) = lhs + rhs
  }

  private val _factory = KaleidoxStateMachineLogic.Factory

  val empty = StateMachineModel()

  def apply(p: StateMachineClass): StateMachineModel =
    StateMachineModel(TreeMap.create(p.name -> p))

  def create(config: Config, p: LogicalSection): StateMachineModel =
    Builder(config).build(p)

  // def create(config: Config, p: Section): StateMachineModel =
  //   Builder(config).build(p)

  // def createClassOption(config: Config, p: Section): Option[StateMachineClass] =
  //   Builder(config).createClassOption(p)

  case class Builder(config: Config) {
    private val _narrative_keys = Set(
      "headline",
      "brief",
      "summary",
      "description",
      "lead",
      "content",
      "abstract",
      "remarks",
      "tooltip",
      "overview",
      "background",
      "mapping",
      "note",
      "notes",
      "narrative",
      "example",
      "validation",
      "rationale"
    )

    def build(p: LogicalSection): StateMachineModel = {
      val xs = p.sections
      if (xs.isEmpty)
        _flat(p)
      else
        _subsections(xs)
    }

    private def _flat(p: LogicalSection) = {
      val stms = _create_flat(p).toVector
      _to_model(stms)
    }

    private def _subsections(ps: Seq[LogicalSection]) = {
      val stms = ps.filterNot(x => _narrative_keys.contains(x.keyForModel.toLowerCase)).flatMap(_create_subsection)
      _to_model(stms)
    }

    private def _to_model(ps: Seq[StateMachineClass]) = {
      val builder = TreeMap.Builder[StateMachineClass](".")
      val a = ps.foldLeft(builder)((z, x) => z.add(x.name, x)).build
      StateMachineModel(a)
    }

    private def _create_flat(p: LogicalSection): Option[StateMachineClass] = {
      val s = p.text
      val r = StateMachineClass.parse(_factory, s)
      r.toOption // TODO
    }

    private def _create_subsection(p: LogicalSection): Option[StateMachineClass] = {
      if (_has_state_sections(p))
        _create_subsection_cml(p)
      else {
        val name = p.nameForModel
        val s = p.text
        val r = StateMachineClass.parseBody(_factory, name, s)
        r.toOption // TODO
      }
    }

    private def _has_state_sections(p: LogicalSection): Boolean =
      p.sections.exists(_.keyForModel.equalsIgnoreCase("state"))

    private def _create_subsection_cml(p: LogicalSection): Option[StateMachineClass] = {
      val statesection = p.sections.find(_.keyForModel.equalsIgnoreCase("state"))
      statesection.flatMap { ss =>
        val states = ss.sections.toList.zipWithIndex.map {
          case (s, i) => _state_from_section(p.nameForModel, s, i)
        }
        if (states.isEmpty)
          None
        else {
          val rule = StateMachineRule(
            name = Some(p.nameForModel),
            kind = StateMachineKind.Plain,
            states = states
          )
          Some(StateMachineClass(p.nameForModel, rule, _factory.create(rule)))
        }
      }
    }

    private def _state_from_section(
      machinename: String,
      p: LogicalSection,
      index: Int
    ): StateClass = {
      val ts = p.sections.filter(_.keyForModel.equalsIgnoreCase("transition")).map(_transition_from_section(machinename, p.nameForModel, _))
      StateClass(
        name = p.nameForModel,
        value = _state_value(p, index),
        stateMachinePath = None,
        transitions = Transitions.call(ts)
      )
    }

    private def _state_value(p: LogicalSection, index: Int): Int =
      _key_values(p.text).collectFirst {
        case (k, v) if k == "value" => v.trim.stripPrefix("\"").stripSuffix("\"").toInt
      }.getOrElse(index + 1)

    private def _transition_from_section(
      machinename: String,
      statename: String,
      p: LogicalSection
    ): Transition = {
      val kv = _key_values(p.text)
      val to = _required_transition_key(kv, "to", machinename, statename)
      val on = _required_transition_key(kv, "on", machinename, statename)
      Transition(
        guard = EventNameGuard(on),
        to = NameTransitionTo(to),
        effect = Activity.Empty
      )
    }

    private def _required_transition_key(
      kv: Vector[(String, String)],
      key: String,
      machinename: String,
      statename: String
    ): String =
      kv.collectFirst {
        case (k, v) if k == key => v
      }.getOrElse {
        throw new IllegalArgumentException(s"StateMachine '$machinename' state '$statename' transition requires '$key'.")
      }

    private def _key_values(p: String): Vector[(String, String)] =
      p.linesIterator.toVector.flatMap { line =>
        val s = line.trim
        if (s.isEmpty)
          None
        else
          s.indexOf(':') match {
            case -1 =>
              s.indexOf('=') match {
                case -1 => None
                case n => Some(s.take(n).trim.toLowerCase -> s.drop(n + 1).trim)
              }
            case n => Some(s.take(n).trim.toLowerCase -> s.drop(n + 1).trim)
          }
      }

    // def build(p: Section): StateMachineModel = {
    //   val stms = p.sectionsShallow.flatMap(createClassOption)
    //   StateMachineModel(stms)
    // }

    // def createClassOption(p: Section): Option[StateMachineClass] = {
    //   val name = p.nameForModel
    //   val s = p.toPlainText
    //   val r = StateMachineClass.parse(s)
    //   r.toOption // TODO
    // }
  }
}
