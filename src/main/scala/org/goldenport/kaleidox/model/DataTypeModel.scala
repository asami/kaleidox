package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.smartdox.Table
import org.goldenport.values.Designation
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.record.v2.DataType
import org.goldenport.record.v3.IRecord
import org.goldenport.kaleidox.Config
import org.goldenport.kaleidox.Model

/*
 * @since   Oct. 12, 2023
 * @version Oct. 22, 2023
 * @author  ASAMI, Tomoharu
 */
case class DataTypeModel(
  description: Description = Description.name("datatype"),
  classes: VectorMap[String, DataTypeModel.DataTypeClass] = VectorMap.empty
) extends Model.ISubModel {
  protected def display_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def print_String: String = classes.values.map(x => x.name).mkString("\n")

  protected def show_String: String = classes.values.map(x => x.name).mkString("\n")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[DataTypeModel] = if (isEmpty) None else Some(this)

  def +(rhs: DataTypeModel): DataTypeModel = copy(classes = classes ++ rhs.classes)
}

object DataTypeModel {
  val empty = DataTypeModel()

  implicit object DataTypeModelMonoid extends Monoid[DataTypeModel] {
    def zero = DataTypeModel.empty
    def append(lhs: DataTypeModel, rhs: => DataTypeModel) = lhs + rhs
  }

  sealed trait DataTypeClass extends Description.Holder {
  }
  object DataTypeClass {
    case class Plain(
      description: Description,
      datatype: DataType,
      packageName: String = "domain" // TODO
    ) extends DataTypeClass {
    }

    case class Complex(
      description: Description,
      constitutes: VectorMap[String, DataTypeClass],
      packageName: String = "domain" // TODO
    ) extends DataTypeClass {
    }

     def createOption(config: Config, p: LogicalSection): Option[DataTypeClass] = ???
    //   Builder().createOption(p)

    case class Builder(
      config: Config
    ) extends Model.ModelBuilderBase {
      type T = DataTypeClass

      protected def is_Accept(p: LogicalSection): Boolean = true

      protected def create_Model(
        p: LogicalSection,
        ps: Vector[LogicalSection],
        desc: Description
      ): T = {
        ???
      }

      protected def create_Model(
        p: LogicalSection,
        desc: Description,
        properties: IRecord
      ): T = ???

      protected def create_Model(
        p: LogicalSection,
        desc: Description,
        tables: List[Table]
      ): T = ???

      case class Z(
        table: Vector[Table] = Vector.empty
      ) {
        def r = ???

        // def +(rhs: LogicalBlock) = rhs match {
        //   case StartBlock => this
        //   case EndBlock => this
        //   case m: LogicalSection => _section(m)
        //   case m: LogicalParagraph => this
        //   case m: LogicalVerbatim => this
        // }

        // private def _section(p: LogicalSection) =
        //   ???
      }
    }
  }

  def apply(p: DataTypeClass): DataTypeModel = ???

  def create(config: Config, p: LogicalSection): DataTypeModel =
    Builder(config).createOption(p) getOrElse DataTypeModel.empty

  case class Builder(config: Config) extends Model.ModelBuilderBase {
    type T = DataTypeModel

    protected def is_Accept(p: LogicalSection): Boolean =
      p.nameForModel equalsIgnoreCase "datatype"

    protected def create_Model(
      p: LogicalSection,
      ps: Vector[LogicalSection],
      desc: Description
    ): T = {
      val classes = ps.flatMap(DataTypeClass.createOption(config, _))
      DataTypeModel(desc, VectorMap(classes.map(x => x.name -> x)))
    }

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      properties: IRecord
    ): T = DataTypeModel.empty

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      tables: List[Table]
    ): T = DataTypeModel.empty
  }
}
