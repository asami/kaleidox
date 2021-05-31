package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.smartdox._
import org.goldenport.RAISE
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.collection.VectorMap
import org.goldenport.parser.ParseResult
import org.goldenport.record.v2.Schema
import org.goldenport.record.v3.Table
import org.goldenport.kaleidox._

/*
 * @since   May. 13, 2021
 * @version May. 14, 2021
 * @author  ASAMI, Tomoharu
 */
case class DataStoreModel(
  classes: VectorMap[String, DataStoreModel.CollectionModel] = VectorMap.empty
) {
  import DataStoreModel._

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
  ) {
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

  def create(config: Config, p: Section): DataStoreModel =
    Builder(config).build(p)

  // def createClassOption(config: Config, p: Section): Option[CollectionModel] =
  //   Builder(config).createClassOption(p)

  case class Builder(config: Config) {
    def build(p: Section): DataStoreModel =
      p.sectionsShallow match {
        case Nil => _build_config(p)
        case xs => _build_sections(p, xs)
      }

    private def _build_config(p: Section): DataStoreModel =
      createClassOption(p).
        map(DataStoreModel.apply).
        getOrElse(empty)

    def createClassOption(p: Section): Option[CollectionModel] = {
      val name = p.nameForModel
      val s = p.toPlainText
//      println(s"X: $s")
//      println(s"Y: $p")
      val r = CollectionModel.parse(name, s)
      r.toOption // TODO
    }

    private def _build_sections(p: Section, sections: List[Section]): DataStoreModel =
      RAISE.notImplementedYetDefect
  }
}
