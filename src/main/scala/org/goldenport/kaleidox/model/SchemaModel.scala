package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.parser.Dox2Parser
import org.smartdox.Section
import org.smartdox.Description
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v2.XStateMachine
import org.goldenport.record.v2.Constraint
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Record
import org.goldenport.record.v3.Field
import org.goldenport.record.v3.ValueDomain
import org.goldenport.context.Consequence
import org.goldenport.context.Showable
import org.goldenport.values.Designation
import org.goldenport.values.PathName
import org.goldenport.parser._
import org.goldenport.sexpr.SSchema
import org.goldenport.sexpr.eval.entity.EntityId
import org.goldenport.collection.VectorMap
import org.goldenport.event.ObjectId
import org.goldenport.sm.StateMachineClass
import org.goldenport.sm.{StateMachine => StateMachineInstance}
import org.goldenport.kaleidox._

/*
 * @since   Feb. 18, 2021
 *  version Feb. 25, 2021
 *  version May. 14, 2021
 *  version Jun. 25, 2021
 *  version Aug. 29, 2021
 *  version Sep. 25, 2021
 *  version Oct. 31, 2021
 *  version Nov. 20, 2021
 *  version Dec. 31, 2021
 *  version Feb. 24, 2022
 *  version Aug. 21, 2023
 *  version Oct. 15, 2023
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
case class SchemaModel(
  classes: VectorMap[String, SchemaModel.SchemaClass],
  description: Description = Description.name("schema")
) extends Model.ISchemaSubModel {
  import SchemaModel._

  protected def display_String: String = classes.values.map(x => x.name).mkString(",")

  protected def print_String: String = classes.values.map(x => x.name).mkString(",")

  protected def show_String: String = classes.values.map(x => x.name).mkString(",")

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

  val featureKeys = Set("feature", "features", "フィーチャー", "フィーチャ", "特性")
  val attributeKeys = Set("attribute", "attributes", "アトリビュート", "属性")
  val associationKeys = Set("association", "associations", "アソシエーション", "関連")
  val aggregationKeys = Set("aggregation", "aggregations", "アグレゲーション", "集約")
  val compositionKeys = Set("composition", "compositions", "コンポジション", "合成")
  val powertypeKeys = Set("powertype", "powertypes", "パワータイプ", "区分")
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
  ) extends ISchemaClass with Showable.Base {
    lazy val schema: Schema = {
      val columns = slots.map(_.toColumn)
      tableName match {
        case Some(s) => Schema(columns, sql = SqlSchema(Nil, Some(s)))
        case None => Schema(columns)
      }
    }
    lazy val id: Id = slots.collect {
      case m: Id => m
    }.head
    lazy val attributes: Vector[Attribute] = slots.collect {
      case m: Attribute => m
    }
    lazy val attributeMap = attributes.map(x => x.name -> x).toMap
    lazy val stateMachines: Vector[StateMachineClass] = slots.collect {
      case m: StateMachine => m.statemachine
    }
    lazy val stateMachineMap = stateMachines.map(x => x.name -> x).toMap
    val tableName = features.tableName

    def withName(p: String) = copy(name = p)
    def withTableName(p: String) = copy(features = features.withTableName(p))
      def addParentName(p: String) = copy(features = features.addParentName(p))

    def add(p: SchemaClass) = copy(features = features.add(p.features), slots = slots ++ p.slots)

    def attributeRecordForCreate(p: IRecord): Consequence[Record] = {
      case class Z(xs: Consequence[Vector[Field]] = Consequence(Vector.empty)) {
        def r = xs.map(Record(_))

        def +(rhs: Attribute) =
          p.get(rhs.name) match {
            case Some(s) => _add(rhs.verifyField(rhs.name, s))
            case None =>
              if (rhs.isRequired)
                _add(Consequence.missingPropertyFault[Field](rhs.name))
              else
                this
          }

        private def _add(p: Consequence[Field]) = copy(xs = (xs |@| p)(_ :+ _))
      }
      attributes./:(Z())(_+_).r
      // case class Z(xs: Vector[Consequence[Field]] = Vector.empty) {
      //   def r = 

      //   def +(rhs: Field) = attributeMap.get(rhs.name) match {
      //     case Some(s) => 
      //     case None => this // XXX more gidid
      //   }
      // }
      // p.fields./:(Z())(_+_).r
    }

    def idForReconstitute(p: IRecord): Consequence[EntityId] =
      Consequence.successOrMissingPropertyFault("id", p.get(id.name).map(id.reconstitute(name, _)))

    def attributeRecordForReconstitute(p: IRecord): Consequence[Record] =
      attributeRecordForCreate(p)

    def stateMachineRecordForReconstitute(p: IRecord, id: EntityId): Consequence[VectorMap[Symbol, StateMachineInstance]] = {
      val a = stateMachineMap.toVector.map {
        case (k, sm) => for {
          v <- Consequence.successOrMissingPropertyFault(k, p.get(k))
          x <- sm.reconstitute(v, ObjectId(id.objectId.string))
        } yield Symbol(k) -> x
      }
      a.sequence.map(_.foldLeft(VectorMap.empty[Symbol, StateMachineInstance]) { (z, x) =>
        z + (x)
      })
    }

    def unmarshallProperties(p: IRecord): Consequence[IRecord] = {
      case class Z(xs: Consequence[Vector[Field]] = Consequence.success(Vector.empty)) {
        def r = xs.map(x => Record(x))

        def +(rhs: Field) =
          slots.find(_.name == rhs.name) match {
            case Some(s) => rhs.getValue.
                map(x => _add(s.unmarshall(x).map(Field.create(rhs.key, _)))).
                getOrElse(_add(rhs))
            case None => _add(rhs)
          }

        private def _add(p: Consequence[Field]) = copy(xs = (xs |@| p)(_ :+ _))

        private def _add(p: Field) = copy(xs = xs.map(_ :+ p))
      }
      p.fields./:(Z())(_+_).r
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
    val objectRefName = Vector("オブジェクト参照", "エンティティ", "objectref", "entity")
    val powertypeRefName = Vector("パワータイプ", "区分", "powertyperef", "powertype")

    trait SchemaClassContainer extends Showable.Base {
      def schemaClass: SchemaClass

      protected def display_String: String = "SchemaClassContainer"

      protected def print_String: String = "SchemaClassContainer"

      protected def show_String: String = "SchemaClassContainer"
    }

    case class Features(
      tableName: Option[String] = None,
      parentsName: List[String] = Nil
    ) {
      def isEmpty = tableName.isEmpty
      def toOption: Option[Features] = if (isEmpty) None else Some(this)

      def withTableName(p: String) = copy(tableName = Some(p))
      def addParentName(p: String) = copy(parentsName = parentsName :+ p)

      def add(p: Features) = copy(
        tableName = p.tableName orElse tableName
      )

      def +(rhs: Features): Features = Features(
        rhs.tableName orElse tableName,
        parentsName ::: rhs.parentsName
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
          aggregationTables: Vector[Table] = Vector.empty,
          compositionTables: Vector[Table] = Vector.empty,
          powertypeTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          anonTables: Vector[Table] = Vector.empty
        ) {
          def r = {
            // val features = propertyTables.headOption
            // val props = propertyTables.headOption orElse anonTables.headOption
            // props.map(_to_schema_class(p.nameForModel, features, _))
            val props = if (propertyTables.nonEmpty) propertyTables else anonTables
            _get_schema_class(
              p.nameForModel,
              featureTables,
              props,
              attributeTables,
              associationTables,
              aggregationTables,
              compositionTables,
              powertypeTables,
              statemachines
            )
          }

          def +(rhs: LogicalBlock) = rhs match {
            case StartBlock => this
            case EndBlock => this
            case m: LogicalSection => _section(m)
            case m: LogicalParagraph => this // TODO features by property
            case m: LogicalVerbatim => this // TODO features by property
          }

          private def _table(m: Table) =
            if (_is_property_table(m))
              copy(propertyTables = propertyTables :+ m)
            else if (_is_feature_table(m))
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
            else if (_is_associations(p))
              _associations(p)
            else if (_is_aggregations(p))
              _aggregations(p)
            else if (_is_compositions(p))
              _compositions(p)
            else if (_is_powertypes(p))
              _powertypes(p)
            else if (_is_statemachines(p))
              _statemachines(p)
            else
              this

          private def _is_features(p: LogicalSection) =
            featureKeys.contains(p.keyForModel)

          private def _is_attributes(p: LogicalSection) =
            attributeKeys.contains(p.keyForModel)

          private def _is_associations(p: LogicalSection) =
            associationKeys.contains(p.keyForModel)

          private def _is_aggregations(p: LogicalSection) =
            aggregationKeys.contains(p.keyForModel)

          private def _is_compositions(p: LogicalSection) =
            compositionKeys.contains(p.keyForModel)

          private def _is_powertypes(p: LogicalSection) =
            powertypeKeys.contains(p.keyForModel)

          private def _is_statemachines(p: LogicalSection) = 
            statemachineKeys.contains(p.keyForModel)

          private def _features(p: LogicalSection) = {
            copy(featureTables = featureTables ++ _table_list(p))
          }

          private def _attributes(p: LogicalSection) = {
            copy(attributeTables = attributeTables ++ _table_list(p))
          }

          private def _associations(p: LogicalSection) = {
            copy(associationTables = associationTables ++ _table_list(p))
          }

          private def _aggregations(p: LogicalSection) = {
            copy(aggregationTables = aggregationTables ++ _table_list(p))
          }

          private def _compositions(p: LogicalSection) = {
            copy(compositionTables = compositionTables ++ _table_list(p))
          }

          private def _powertypes(p: LogicalSection) = {
            copy(powertypeTables = powertypeTables ++ _table_list(p))
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
          aggregationTables: Vector[Table] = Vector.empty,
          compositionTables: Vector[Table] = Vector.empty,
          powertypeTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          anonTables: Vector[Table] = Vector.empty
        ) {
          def r = {
            // val features = propertyTables.headOption
            // val props = propertyTables.headOption orElse anonTables.headOption
            // props.map(_to_schema_class(p.nameForModel, features, _))
            val props = if (propertyTables.nonEmpty) propertyTables else anonTables
            _get_schema_class(
              p.nameForModel,
              featureTables,
              props,
              attributeTables,
              associationTables,
              aggregationTables,
              compositionTables,
              powertypeTables,
              statemachines
            )
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
        aggres: Seq[Table],
        compos: Seq[Table],
        powers: Seq[Table],
        sms: Seq[StateMachineClass]
      ): Option[SchemaClass] = {
        val name = if (autoCapitalize) UString.capitalize(pname) else pname
        val fs: Option[Features] = _to_features_option(features)
        val xs: Seq[Slot] =
          _to_props(props) ++ _to_attrs(attrs) ++
        _to_assocs(assocs) ++ _to_aggres(aggres) ++ _to_comps(compos) ++
        _to_powers(powers) ++ _to_sms(sms)
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

      private def _to_attrs(ps: Seq[Table]): Seq[Slot] = { // Attribute | Id
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_attribute_or_id)
      }

      private def _to_assocs(ps: Seq[Table]): Seq[Association] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_association)
      }

      private def _to_aggres(ps: Seq[Table]): Seq[Aggregation] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_aggregation)
      }

      private def _to_comps(ps: Seq[Table]): Seq[Composition] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_composition)
      }

      private def _to_powers(ps: Seq[Table]): Seq[PowertypeSlot] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.map(_powertype)
      }

      private def _to_sms(ps: Seq[StateMachineClass]): Seq[StateMachineSlot] = {
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
          case m if _is_attribute(m) =>
            if (_is_id(p))
              _id(p)
            else
              _attribute(p)
          case m if _is_association(m) => _association(p)
        }

      private def _get_kind(p: Record): Option[String] =
        kindName.toStream.flatMap(x => p.getString(x)).headOption

      private def _is_id(p: Record) = _name(p).equalsIgnoreCase("id") // TODO

      private def _is_attribute(p: String) = attributeName.exists(_.equalsIgnoreCase(p))

      private def _is_association(p: String) = associationName.exists(_.equalsIgnoreCase(p))

      private def _is_statemachine(p: String) = statemachineName.exists(_.equalsIgnoreCase(p))

      private def _attribute_or_id(p: Record) =
        if (_name(p).equalsIgnoreCase("id")) // TODO
          _id(p)
        else
          _attribute(p)

      private def _id(p: Record) = Id(
        _name(p),
        _label(p),
        ValueDomain(
          _datatype(p),
          _multiplicity(p),
          _constraints(p)
        )
      )

      private def _attribute(p: Record) = Attribute(
        _name(p),
        _label(p),
        ValueDomain(
          _datatype(p),
          _multiplicity(p),
          _constraints(p)
        )
      )

      private def _association(p: Record) = Association(
        _desc(p),
        _objectref(p),
        _multiplicity(p)
      )

      private def _aggregation(p: Record) = Aggregation(
        _desc(p),
        _objectref(p),
        _multiplicity(p)
      )

      private def _composition(p: Record) = Composition(
        _desc(p),
        _objectref(p),
        _multiplicity(p)
      )

      private def _powertype(p: Record) = PowertypeRelationship(
        _desc(p),
        _powertyperef(p),
        _multiplicity(p)
      )

      private def _statemachine(p: StateMachineClass) =
        StateMachine(p, None)
        // if (true) {
        //   StateMachineRelationship(
        //     _desc(p),
        //     _statemachineref(p)
        //   )
        // } else {
        //   StateMachine(p, None)
        // }

      private def _name(p: Record): String = p.getStringCaseInsensitive(nameName).getOrElse {
        RAISE.syntaxErrorFault("No name in table.")
      }

      private def _label(p: Record): Option[I18NString] = p.getStringCaseInsensitive(labelName).
        map(I18NString.apply)

      private def _desc(p: Record): Description =
        p.getStringCaseInsensitive(nameName).map { name =>
          val d = Designation(name)
          Description(d)
        }.getOrElse (
          RAISE.syntaxErrorFault("No name in table.")
        )

      private def _datatype(p: Record): DataType = p.getStringCaseInsensitive(typeName).
        flatMap(DataType.get).getOrElse(XString)

      private def _objectref(p: Record): ObjectRef =
        p.getStringCaseInsensitive(objectRefName).map(ObjectRef).getOrElse(
          RAISE.syntaxErrorFault("No objectref in table")
        )

      private def _powertyperef(p: Record): PowertypeRef =
        p.getStringCaseInsensitive(powertypeRefName).map(PowertypeRef).getOrElse(
          RAISE.syntaxErrorFault("No powertyperef in table")
        )

      private def _multiplicity(p: Record): Multiplicity = p.getStringCaseInsensitive(multiplicityName).
        flatMap(Multiplicity.get).getOrElse(MOne)

      private def _constraints(p: Record): List[Constraint] = Nil // TODO
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

    def print(p: Schema): String = p.columns.map(_.name).mkString(",")
    def display(p: Schema): String = p.columns.map(_.name).mkString(",")
    def show(p: Schema): String = p.columns.map(_.name).mkString(",")
  }

  case class ObjectRef(v: String) {
    lazy val pathname = PathName(v, ".")
    def qualifiedName = v
  }

  case class PowertypeRef(v: String) {
  }

  case class StateMachineRef(v: String) {
  }

  trait Slot {
    def name: String
    def toColumn: Column
    def unmarshall(p: Any): Consequence[Any]
  }

  case class Id(
    name: String,
    label: Option[I18NString],
    domain: ValueDomain
  ) extends Slot {
    def toColumn = Column(
      name,
      domain.datatype,
      domain.multiplicity,
      i18nLabel = label
    )

    def reconstitute(classname: String, p: Any): EntityId = p match {
      case m: String => EntityId(classname, m)
      case m: Long => EntityId(classname, m)
      case m => EntityId(classname, m.toString)
    }

    def unmarshall(p: Any): Consequence[Any] = Consequence(p.toString) // TODO domain
  }

  case class Attribute(
    name: String,
    label: Option[I18NString],
    domain: ValueDomain
  ) extends Slot {
    def isRequired = domain.isRequired

    def verify(p: Any): Consequence[Any] = domain.verify(p)
    def verifyField(p: Field): Consequence[Field] = domain.verifyField(p)
    def verifyField(key: String, p: Any): Consequence[Field] = domain.verifyField(key, p)

    def toColumn = Column(
      name,
      domain.datatype,
      domain.multiplicity,
      i18nLabel = label
    )

    def unmarshall(p: Any): Consequence[Any] = verify(p)
  }

  class Association(
    constituent: Association.Constituent
  ) extends Slot {
    def name: String = constituent.name
    def objectRef: ObjectRef = constituent.objectRef
    def multiplicity: Multiplicity = constituent.multiplicity
    def description: Description = constituent.description

    def toColumn = RAISE.noReachDefect

    def unmarshall(p: Any): Consequence[Any] = Consequence.success(p) // TODO
  }
  object Association {
    case class Constituent(
      description: Description,
      objectRef: ObjectRef,
      multiplicity: Multiplicity
    ) {
      def name = description.name
    }

    def apply(desc: Description, oref: ObjectRef, multiplicity: Multiplicity) =
      new Association(Constituent(desc, oref, multiplicity))
  }

  class Aggregation(constituent: Association.Constituent) extends Association(constituent)
  object Aggregation {
    def apply(desc: Description, oref: ObjectRef, multiplicity: Multiplicity) =
      new Aggregation(Association.Constituent(desc, oref, multiplicity))
  }

  class Composition(constituent: Association.Constituent) extends Aggregation(constituent)
  object Composition {
    def apply(desc: Description, oref: ObjectRef, multiplicity: Multiplicity) =
      new Composition(Association.Constituent(desc, oref, multiplicity))
  }

  sealed trait PowertypeSlot extends Slot {
  }

  case class PowertypeRelationship(
    description: Description,
    powertypeRef: PowertypeRef,
    multiplicity: Multiplicity
  ) extends PowertypeSlot {
    def name: String = description.name

    def toColumn = Column(
      name
    )

    def unmarshall(p: Any): Consequence[Any] = Consequence.success(p) // TODO
  }

  sealed trait StateMachineSlot extends Slot {
  }

  case class StateMachineRelationship(
    description: Description,
    stateMachineRef: StateMachineRef
  ) extends StateMachineSlot with Description.Holder {
    def toColumn = Column(
      name
    )

    def unmarshall(p: Any): Consequence[Any] = Consequence.success(p) // TODO
  }

  case class StateMachine(
    statemachine: StateMachineClass,
    label: Option[I18NString]
  ) extends StateMachineSlot {
    def name = statemachine.name
    def datatype = XStateMachine(Some(statemachine))
    def multiplicity = MOne

    def toColumn = Column(
      name,
      datatype,
      multiplicity,
      i18nLabel = label
    )

    def unmarshall(p: Any): Consequence[StateMachineInstance] = statemachine.reconstitute(p)
  }
}
