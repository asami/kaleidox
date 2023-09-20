package org.goldenport.kaleidox.model

import scalaz.{State => _, _}, Scalaz._
import org.goldenport.parser.LogicalSection
import org.goldenport.collection.TreeMap
import org.goldenport.context.Consequence
import org.goldenport.statemachine._
import org.goldenport.kaleidox._

/*
 * @since   May.  4, 2021
 *  version May. 23, 2021
 *  version Jun. 27, 2021
 * @version Aug. 21, 2023
 * @author  ASAMI, Tomoharu
 */
case class StateMachineModel(
  classes: TreeMap[StateMachineClass] = TreeMap.empty
) extends Model.ISubModel {
  val name = "stateMachine"

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
      val stms = ps.flatMap(_create_subsection)
      _to_model(stms)
    }

    private def _to_model(ps: Seq[StateMachineClass]) = {
      val builder = TreeMap.Builder[StateMachineClass](".")
      val a = ps./:(builder)((z, x) => z.add(x.name, x)).build
      StateMachineModel(a)
    }

    private def _create_flat(p: LogicalSection): Option[StateMachineClass] = {
      val s = p.text
      val r = StateMachineClass.parse(_factory, s)
      r.toOption // TODO
    }

    private def _create_subsection(p: LogicalSection): Option[StateMachineClass] = {
      val name = p.nameForModel
      val s = p.text
      val r = StateMachineClass.parseBody(_factory, name, s)
      r.toOption // TODO
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
