package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.smartdox._
import org.goldenport.RAISE
import org.goldenport.context.Showable
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.collection.VectorMap
import org.goldenport.parser._
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.Table
import org.goldenport.kaleidox._

/*
 * @since   May. 13, 2021
 *  version May. 14, 2021
 *  version Jan. 22, 2023
 * @version Aug. 21, 2023
 * @author  ASAMI, Tomoharu
 */
case class DataStoreModel(
  classes: VectorMap[String, DataStoreModel.CollectionModel] = VectorMap.empty
) extends Model.ISubModel {
  import DataStoreModel._

  val name = "dataStore"


  protected def display_String: String = classes.values.map(x => x.name).mkString(",")

  protected def print_String: String =
    classes.values.map(x => s"${x.print}").mkString("\n")

  protected def show_String: String =
    classes.values.map(x => s"${x.display}").mkString(",")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[DataStoreModel] = if (isEmpty) None else Some(this)

  def +(rhs: DataStoreModel): DataStoreModel = copy(classes = classes ++ rhs.classes)
}

object DataStoreModel {
  implicit object DataStoreModelMonoid extends Monoid[DataStoreModel] {
    def zero = DataStoreModel.empty
    def append(lhs: DataStoreModel, rhs: => DataStoreModel) = lhs + rhs
  }

  val empty = DataStoreModel()

  case class CollectionModel(
    name: String,
    table: Option[String],
    schema: Either[Schema, String],
    data: Option[Table] = None
  ) extends Showable.Base {
    protected def print_String: String = s"$label_string"

    protected def display_String: String = s"$label_string"

    protected def show_String: String = s"$label_string"
  }
  object CollectionModel {
    val PROP_DS_NAME = "name"
    val PROP_DS_TABLE = "table"
    val PROP_DS_SCHEMA = "schema"

    def apply(
      name: String,
      table: Option[String], // table
      schema: Option[Either[Schema, String]]
    ): CollectionModel = CollectionModel(
      name,
      table,
      schema getOrElse Right(name)
    )

    private def parse(p: String): ParseResult[CollectionModel] = {
      val hocon = ConfigFactory.parseString(p)
      for {
        name <- hocon.parseString(PROP_DS_NAME)
        table <- hocon.parseStringOption(PROP_DS_TABLE)
        schema <- _schema(hocon, name)
      } yield {
        CollectionModel(name, table, schema)
      }
    }

    def parse(name: String, p: String): ParseResult[CollectionModel] = {
      val hocon = ConfigFactory.parseString(p)
      for {
        table <- hocon.parseStringOption(PROP_DS_TABLE)
        schema <- _schema(hocon, name)
      } yield {
        CollectionModel(name, table, schema)
      }
    }

    private def _schema(hocon: Hocon, name: String): ParseResult[Option[Either[Schema, String]]] =
      for {
        sc <- hocon.parseStringOrConfigOption(PROP_DS_SCHEMA)
      } yield sc map {
        case Right(r) => RAISE.notImplementedYetDefect
        case Left(l) => Right(l)
      }
  }

  def apply(p: CollectionModel): DataStoreModel =
    DataStoreModel(VectorMap(p.name -> p))

  def create(context: DataSet.Builder.Context, p: LogicalSection): DataStoreModel =
    Builder(context).build(p)

  // def createClassOption(config: Config, p: Section): Option[CollectionModel] =
  //   Builder(config).createClassOption(p)

  case class Builder(context: DataSet.Builder.Context) {
    def build(p: LogicalSection): DataStoreModel =
      p.sections.toList match {
        case Nil => _build_config(p)
        case xs => _build_sections(p, xs)
      }

    private def _build_config(p: LogicalSection): DataStoreModel =
      createClassOption(p).
        map(DataStoreModel.apply).
        getOrElse(empty)

    def createClassOption(p: LogicalSection): Option[CollectionModel] = {
      val name = p.nameForModel
      _build_properties(p, name)
    }

    private def _build_properties(p: LogicalSection, name: String): Option[CollectionModel] = {
      val s = p.text
//      println(s"X: $s")
//      println(s"Y: $p")
      val r = CollectionModel.parse(name, s)
      r.toOption // TODO
    }

    private def _build_sections(p: LogicalSection, sections: List[LogicalSection]): DataStoreModel = {
      val name = p.nameForModel
      case class Z(
        collection: Option[CollectionModel] = None,
        data: Option[Table] = None
      ) {
        def r = {
          val c = collection orElse context.getSchemaClass(name).
            map(x => CollectionModel(name, None, Left(x.schema), data))
          c.map(DataStoreModel.apply).orZero
        }

        def +(rhs: LogicalSection) = rhs.nameForModel match {
          case "properties" => copy(collection = _build_properties(rhs, name))
          case "data" => copy(data = _build_data(rhs))
          case _ => this
        }

        private def _build_data(p: LogicalSection): Option[Table] = {
          val builder = DataSet.Builder(context)
          val ds = builder.createData(name, p)
          ds.getSlot(name).map(_.data.table.toTable)
        }
      }
      sections./:(Z())(_+_).r
    }
  }
}
