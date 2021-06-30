package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.kaleidox._

/*
 * @since   Feb. 18, 2021
 *  version Feb. 25, 2021
 *  version May. 14, 2021
 * @version Jun. 25, 2021
 * @author  ASAMI, Tomoharu
 */
case class SchemaModel(
  classes: VectorMap[String, SchemaModel.SchemaClass]
) extends ISchemaModel {
  import SchemaModel._

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[SchemaModel] = if (isEmpty) None else Some(this)

  def getSchemaClass(name: String): Option[SchemaClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = getSchemaClass(name).map(_.schema)

  def +(rhs: SchemaModel): SchemaModel = copy(classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.schema.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }
}

object SchemaModel {
  val empty = SchemaModel(VectorMap.empty[String, SchemaClass])

  implicit object SchemaModelMonoid extends Monoid[SchemaModel] {
    def zero = SchemaModel.empty
    def append(lhs: SchemaModel, rhs: => SchemaModel) = lhs + rhs
  }

  def apply(p: SchemaClass): SchemaModel = SchemaModel(VectorMap(p.name -> p))

  // TODO simplemodeler
  import org.smartdox._
  import com.asamioffice.goldenport.text.UString
  import org.goldenport.RAISE
  import org.goldenport.i18n.I18NString
  import org.goldenport.record.v2.{DataType, Multiplicity, XString, MOne}

  case class SchemaClass(
    name: String,
    features: SchemaClass.Features,
    slots: Vector[Slot]
  ) extends ISchemaClass {
    lazy val schema: Schema = {
      val tablename = features.tableName
      val columns = slots.map(_.toColumn)
      tablename match {
        case Some(s) => Schema(columns, sql = SqlSchema(Nil, Some(s)))
        case None => Schema(columns)
      }
    }
  }
  object SchemaClass {
    // def apply(ps: Iterable[Slot]): SchemaClass = new SchemaClass(ps.toVector)

    case class Features(
      tableName: Option[String] = None
    )
    object Features {
      val empty = Features()
    }

    def apply(name: String, features: Option[Features], slots: Vector[Slot]): SchemaClass =
      SchemaClass(name, features getOrElse Features.empty, slots)

    def createOption(p: Section): Option[SchemaClass] =
      new Builder().createOption(p)

    class Builder() {
      val autoCapitalize: Boolean = false

      def createOption(p: Section): Option[SchemaClass] = {
        case class Z(
          propertyTables: Vector[Table] = Vector.empty,
          featureTables: Vector[Table] = Vector.empty,
          anonTables: Vector[Table] = Vector.empty
        ) {
          def r = {
            val features = propertyTables.headOption
            val props = propertyTables.headOption orElse anonTables.headOption
            props.map(_to_schema_class(p.nameForModel, features, _))
          }

          def +(rhs: Dox) = rhs match {
            case m: Table =>
              if (_is_property_table(m))
                copy(propertyTables = propertyTables :+ m)
              else if (_is_property_table(m))
                copy(featureTables = featureTables :+ m)
              else if (_is_anon_table(m))
                copy(anonTables = anonTables :+ m)
              else
                this
            case m: Paragraph =>
              RAISE.notImplementedYetDefect // TODO features by property
          }
        }
        p.elements./:(Z())(_+_).r
      }

      private def _is_property_table(p: Table) = p.getCaptionName.
        map(_ == "特性一覧").getOrElse(false)

      private def _property_table: PartialFunction[Dox, Table] = {
        case m: Table if _is_property_table(m) => m
      }

      private def _is_feature_table(p: Table) = p.getCaptionName.
        map(_ == "性質一覧").getOrElse(false)

      private def _feature_table: PartialFunction[Dox, Table] = {
        case m: Table if _is_feature_table(m) => m
      }

      private def _is_anon_table(p: Table) = p.getCaptionName.isEmpty

      private def _to_schema_class(pname: String, features: Option[Table], props: Table) = {
        val name = if (autoCapitalize) UString.capitalize(pname) else pname
        val rs = SimpleModelerUtils.toRecords(props)
        val xs = rs.flatMap(_slot)
        val fs = features.flatMap(_to_features)
        SchemaClass(name, fs, xs.toVector)
      }

      private def _slot(p: Record): Option[Slot] =
        p.getString("特性").collect {
          case "属性" => _attribute(p)
        }

      private def _attribute(p: Record) = Attribute(
        _name(p),
        _datatype(p),
        _multiplicity(p),
        _label(p)
      )

      private def _name(p: Record): String = p.getString("名前").getOrElse {
        RAISE.syntaxErrorFault("No name in table.")
      }

      private def _label(p: Record): Option[I18NString] = p.getString("ラベル").
        map(I18NString.apply)

      private def _datatype(p: Record): DataType = p.getString("型").
        flatMap(DataType.get).getOrElse(XString)

      private def _multiplicity(p: Record): Multiplicity = p.getString("多重度").
        flatMap(Multiplicity.get).getOrElse(MOne)
    }

    private def _to_features(p: Table): Option[Features] = {
      val rs = SimpleModelerUtils.toRecords(p)
      case class Z(
        tablename: Option[String] = None
      ) {
        def r = {
          tablename match {
            case Some(s) => Some(Features(Some(s)))
            case None => None
          }
        }

        def +(rhs: Record) = {
          rhs.getString("項目").map {
            case "テーブル" => copy(tablename = rhs.getString("値"))
            case _ => this
          }.getOrElse(this)
        }
      }
      rs./:(Z())(_+_).r
    }
  }

  trait Slot {
    def toColumn: Column
  }

  case class Attribute(
    name: String,
    datatype: DataType,
    multiplicity: Multiplicity,
    label: Option[I18NString]
  ) extends Slot {
    def toColumn = Column(
      name,
      datatype,
      multiplicity,
      i18nLabel = label
    )
  }
}
