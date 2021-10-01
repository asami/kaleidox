package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.parser.Dox2Parser
import org.smartdox.Section
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v2.XStateMachine
import org.goldenport.record.v3.Record
import org.goldenport.parser._
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.statemachine.StateMachineClass
import org.goldenport.kaleidox._

/*
 * @since   Feb. 18, 2021
 *  version Feb. 25, 2021
 *  version May. 14, 2021
 *  version Jun. 25, 2021
 *  version Aug. 29, 2021
 * @version Sep. 25, 2021
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

  val featureKeys = Set("feature", "features", "特性")
  val attributeKeys = Set("attribute", "attributes", "属性")
  val statemachineKeys = Set("statemachine", "statemachines", "ステートマシーン", "状態機械", "statechart", "statecharts", "ステートチャート")

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
    lazy val stateMachines: Vector[StateMachineClass] = slots.collect {
      case m: StateMachine => m.statemachine
    }
  }
  object SchemaClass {
    // def apply(ps: Iterable[Slot]): SchemaClass = new SchemaClass(ps.toVector)

    val kindName = Vector("特性")
    val attributeName = Vector("属性", "attribute", "attr")
    val associationName = Vector("関連", "association", "assoc") // TODO composition, aggregation
    val statemachineName = Vector("状態機械", "statemachine", "sm")
    val nameName = Vector("名前", "name")
    val labelName = Vector("ラベル", "label")
    val typeName = Vector("型", "タイプ", "type")
    val multiplicityName = Vector("多重度", "multiplicity")
    val itemName = Vector("項目", "item")
    val valueName = Vector("値", "value")
    val tableName = Vector("テーブル", "表", "table")

    case class Features(
      tableName: Option[String] = None
    ) {
      def isEmpty = tableName.isEmpty
      def toOption: Option[Features] = if (isEmpty) None else Some(this)

      def +(rhs: Features): Features = Features(
        rhs.tableName orElse tableName
      )
    }
    object Features {
      implicit object FeaturesMonoid extends Monoid[Features] {
        def zero = Features.empty
        def append(lhs: Features, rhs: => Features) = lhs + rhs
      }

      val empty = Features()
    }

    def apply(name: String, features: Option[Features], slots: Vector[Slot]): SchemaClass =
      SchemaClass(name, features getOrElse Features.empty, slots)

    def createOption(p: LogicalSection): Option[SchemaClass] =
      new Builder().createOption(p)

    def createOption(p: Section): Option[SchemaClass] =
      new Builder().createOption(p)

    class Builder() {
      val autoCapitalize: Boolean = false

      def createOption(p: LogicalSection): Option[SchemaClass] = {
        case class Z(
          featureTables: Vector[Table] = Vector.empty,
          propertyTables: Vector[Table] = Vector.empty,
          attributeTables: Vector[Table] = Vector.empty,
          associationTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          anonTables: Vector[Table] = Vector.empty
        ) {
          def r = {
            // val features = propertyTables.headOption
            // val props = propertyTables.headOption orElse anonTables.headOption
            // props.map(_to_schema_class(p.nameForModel, features, _))
            val props = if (propertyTables.nonEmpty) propertyTables else anonTables
            _get_schema_class(p.nameForModel, featureTables, props, attributeTables, associationTables, statemachines)
          }

          def +(rhs: LogicalBlock) = rhs match {
            case StartBlock => this
            case EndBlock => this
            case m: LogicalSection => _section(m)
            case m: LogicalParagraph => RAISE.notImplementedYetDefect // TODO features by property
            case m: LogicalVerbatim => RAISE.notImplementedYetDefect // TODO features by property
          }

          private def _table(m: Table) =
            if (_is_property_table(m))
              copy(propertyTables = propertyTables :+ m)
            else if (_is_property_table(m))
              copy(featureTables = featureTables :+ m)
            else if (_is_anon_table(m))
              copy(anonTables = anonTables :+ m)
            else
              this

          private def _section(p: LogicalSection) =
            if (_is_features(p))
              _features(p)
            else if (_is_attributes(p))
              _attributes(p)
            else if (_is_statemachines(p))
              _statemachines(p)
            else
              this

          private def _is_features(p: LogicalSection) =
            featureKeys.contains(p.keyForModel)

          private def _is_attributes(p: LogicalSection) =
            attributeKeys.contains(p.keyForModel)

          private def _is_statemachines(p: LogicalSection) = 
            statemachineKeys.contains(p.keyForModel)

          private def _features(p: LogicalSection) = {
            copy(featureTables = featureTables ++ _table_list(p))
          }

          private def _attributes(p: LogicalSection) = {
            copy(attributeTables = attributeTables ++ _table_list(p))
          }

          private def _statemachines(p: LogicalSection) = {
            val xs = p.sections.flatMap(_statemachine)
            copy(statemachines = statemachines ++ xs)
          }

          private def _statemachine(p: LogicalSection): Option[StateMachineClass] = {
            val f = KaleidoxStateMachineLogic.Factory
            StateMachineClass.parseBodyForResource(f, p.nameForModel, p.text).toOption
          }

          private def _table_list(p: LogicalSection): List[Table] = {
            val doxconfig = Dox2Parser.Config.default // TODO
            val dox = Dox2Parser.parseSection(doxconfig, p)
            dox.tableList
          }
        }

        p.blocks.blocks./:(Z())(_+_).r
      }

      def createOption(p: Section): Option[SchemaClass] = {
        case class Z(
          featureTables: Vector[Table] = Vector.empty,
          propertyTables: Vector[Table] = Vector.empty,
          attributeTables: Vector[Table] = Vector.empty,
          associationTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          anonTables: Vector[Table] = Vector.empty
        ) {
          def r = {
            // val features = propertyTables.headOption
            // val props = propertyTables.headOption orElse anonTables.headOption
            // props.map(_to_schema_class(p.nameForModel, features, _))
            val props = if (propertyTables.nonEmpty) propertyTables else anonTables
            _get_schema_class(p.nameForModel, featureTables, props, attributeTables, associationTables, statemachines)
          }

          def +(rhs: Dox) = rhs match {
            case m: Table => _table(m)
            case m: Section => _section(m)
            case m: Paragraph => RAISE.notImplementedYetDefect // TODO features by property
          }

          private def _table(m: Table) =
            if (_is_property_table(m))
              copy(propertyTables = propertyTables :+ m)
            else if (_is_property_table(m))
              copy(featureTables = featureTables :+ m)
            else if (_is_anon_table(m))
              copy(anonTables = anonTables :+ m)
            else
              this

          private def _section(p: Section) =
            if (_is_attributes(p))
              _attributes(p)
            else if (_is_statemachines(p))
              _statemachines(p)
            else
              this

          private def _is_attributes(p: Section) =
            attributeKeys.contains(p.keyForModel)

          private def _is_statemachines(p: Section) = 
            statemachineKeys.contains(p.keyForModel)

          private def _attributes(p: Section) =
            copy(attributeTables = attributeTables ++ p.tableList)

          private def _statemachines(p: Section) = {
            val xs = p.sections.flatMap(_statemachine)
            copy(statemachines = statemachines ++ xs)
          }

          private def _statemachine(p: Section): Option[StateMachineClass] = {
            val f = KaleidoxStateMachineLogic.Factory
            StateMachineClass.parseBody(f, p.nameForModel, p.toText).toOption
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

      private def _get_schema_class(
        pname: String,
        features: Seq[Table],
        props: Seq[Table],
        attrs: Seq[Table],
        assocs: Seq[Table],
        sms: Seq[StateMachineClass]
      ): Option[SchemaClass] = {
        val name = if (autoCapitalize) UString.capitalize(pname) else pname
        val fs: Option[Features] = _to_features_option(features)
        val xs: Seq[Slot] = _to_props(props) ++ _to_attrs(attrs) ++ _to_assocs(assocs) ++ _to_sms(sms)
        if (fs.isEmpty && xs.isEmpty)
          None
        else
          Some(SchemaClass(name, fs, xs.toVector))
      }

      private def _to_features_option(ps: Seq[Table]): Option[Features] =
        ps.toVector.flatMap(_to_features).concatenate.toOption

      private def _to_props(ps: Seq[Table]): Seq[Slot] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.flatMap(_slot)
      }

      private def _to_attrs(ps: Seq[Table]): Seq[Attribute] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_attribute)
      }

      private def _to_assocs(ps: Seq[Table]): Seq[Association] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_association)
      }

      private def _to_sms(ps: Seq[StateMachineClass]): Seq[StateMachine] = {
        ps.map(_statemachine)
      }

      private def _to_schema_class(pname: String, features: Option[Table], props: Table) = {
        val name = if (autoCapitalize) UString.capitalize(pname) else pname
        val rs = SimpleModelerUtils.toRecords(props)
        val xs = rs.flatMap(_slot)
        val fs = features.flatMap(_to_features)
        SchemaClass(name, fs, xs.toVector)
      }

      // private def _slot(p: Record): Option[Slot] =
      //   p.getString("特性").collect {
      //     case "属性" => _attribute(p)
      //   }

      private def _slot(p: Record): Option[Slot] =
        _get_kind(p).collect {
          case m if _is_attribute(m) => _attribute(p)
          case m if _is_association(m) => _association(p)
        }

      private def _get_kind(p: Record): Option[String] =
        kindName.toStream.flatMap(x => p.getString(x)).headOption

      private def _is_attribute(p: String) = attributeName.exists(_.equalsIgnoreCase(p))

      private def _is_association(p: String) = associationName.exists(_.equalsIgnoreCase(p))

      private def _is_statemachine(p: String) = statemachineName.exists(_.equalsIgnoreCase(p))

      private def _attribute(p: Record) = Attribute(
        _name(p),
        _datatype(p),
        _multiplicity(p),
        _label(p)
      )

      private def _association(p: Record) = Association(
        _name(p),
        _datatype(p),
        _multiplicity(p),
        _label(p)
      )

      private def _statemachine(p: StateMachineClass) = StateMachine(p, None)

      private def _name(p: Record): String = p.getStringCaseInsensitive(nameName).getOrElse {
        RAISE.syntaxErrorFault("No name in table.")
      }

      private def _label(p: Record): Option[I18NString] = p.getStringCaseInsensitive(labelName).
        map(I18NString.apply)

      private def _datatype(p: Record): DataType = p.getStringCaseInsensitive(typeName).
        flatMap(DataType.get).getOrElse(XString)

      private def _multiplicity(p: Record): Multiplicity = p.getStringCaseInsensitive(multiplicityName).
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
          rhs.getStringCaseInsensitive(itemName).map {
            case m if tableName.contains(m.toLowerCase) => copy(tablename = rhs.getStringCaseInsensitive(valueName))
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

  case class Association(
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

  case class StateMachine(
    statemachine: StateMachineClass,
    label: Option[I18NString]
  ) extends Slot {
    def name = statemachine.name
    def datatype = XStateMachine()
    def multiplicity = MOne

    def toColumn = Column(
      name,
      datatype,
      multiplicity,
      i18nLabel = label
    )
  }
}
