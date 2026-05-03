package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.smartdox.parser.Dox2Parser
import org.smartdox.Section
import org.smartdox.Description
import org.smartdox.InlineMacro
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
import org.goldenport.sm._
import org.goldenport.kaleidox._
import scala.util.Try

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
 *  version Sep.  6, 2024
 *  version May.  2, 2025
 *  version Mar. 31, 2026
 * @version May.  3, 2026
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
  type AggregateDef = AggregateDefinition
  type ViewDef = ViewDefinition
  val empty = SchemaModel(VectorMap.empty[String, SchemaClass])

  implicit object SchemaModelMonoid extends Monoid[SchemaModel] {
    def zero = SchemaModel.empty
    def append(lhs: SchemaModel, rhs: => SchemaModel) = lhs + rhs
  }

  val featureKeys = Set("feature", "features", "フィーチャー", "フィーチャ", "特性")
  val delegateKeys = Set("delegate", "delegates", "委譲")
  val attributeKeys = Set("attribute", "attributes", "アトリビュート", "属性")
  val associationKeys = Set("association", "associations", "アソシエーション", "関連")
  val aggregationKeys = Set("aggregation", "aggregations", "アグレゲーション", "集約")
  val compositionKeys = Set("composition", "compositions", "コンポジション", "合成")
  val powertypeKeys = Set("powertype", "powertypes", "パワータイプ", "区分")
  val statemachineKeys = Set("statemachine", "statemachines", "ステートマシーン", "状態機械", "statechart", "statecharts", "ステートチャート")
  val eventKeys = Set("event", "events", "イベント")
  val aggregateKeys = Set("aggregate", "aggregates", "集約")
  val viewKeys = Set("view", "views", "ビュー")

  def apply(p: SchemaClass): SchemaModel = SchemaModel(VectorMap(p.name -> p))

  // TODO simplemodeler
  import org.smartdox._
  import com.asamioffice.goldenport.text.UString
  import org.goldenport.RAISE
  import org.goldenport.i18n.I18NString
  import org.goldenport.record.v2.{DataType, Multiplicity, XString, MOne, SqlColumn, NullSqlColumn}
  import org.goldenport.record.sql

  case class SchemaClass(
    name: String,
    features: SchemaClass.Features,
    slots: Vector[Slot],
    events: Vector[EventDefinition] = Vector.empty,
    aggregate: Option[AggregateDefinition] = None,
    view: Option[ViewDefinition] = None
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

    def add(p: SchemaClass) = copy(
      features = features.add(p.features),
      slots = slots ++ p.slots,
      events = events ++ p.events,
      aggregate = p.aggregate.orElse(aggregate),
      view = p.view.orElse(view)
    )

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
      attributes.foldLeft(Z())(_+_).r
      // case class Z(xs: Vector[Consequence[Field]] = Vector.empty) {
      //   def r = 

      //   def +(rhs: Field) = attributeMap.get(rhs.name) match {
      //     case Some(s) => 
      //     case None => this // XXX more gidid
      //   }
      // }
      // p.fields.foldLeft(Z())(_+_).r
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
      p.fields.foldLeft(Z())(_+_).r
    }
  }

  case class EventDefinition(
    name: String,
    category: String = "NonActionEvent",
    kind: Option[String] = None,
    selectors: Map[String, String] = Map.empty,
    actionName: Option[String] = None,
    priority: Int = 0
  )

  case class AggregateDefinition(
    members: Vector[AggregateMemberDefinition] = Vector.empty,
    creates: Vector[AggregateCreateDefinition] = Vector.empty,
    commands: Vector[AggregateCommandDefinition] = Vector.empty,
    state: Vector[AggregateStateDefinition] = Vector.empty,
    invariants: Vector[AggregateInvariantDefinition] = Vector.empty
  )

  case class AggregateMemberDefinition(
    name: String,
    entity: String,
    kind: String = "composition",
    boundary: String = "internal",
    join: Option[String] = None,
    multiplicity: Option[String] = None,
    joinField: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class AggregateCommandDefinition(
    name: String,
    input: Map[String, String] = Map.empty,
    validations: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty,
    newState: Option[String] = None,
    implementation: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class AggregateCreateDefinition(
    name: String,
    input: Map[String, String] = Map.empty,
    validations: Vector[String] = Vector.empty,
    events: Vector[String] = Vector.empty,
    initialState: Option[String] = None,
    implementation: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class AggregateStateDefinition(
    name: String,
    datatype: Option[String] = None,
    multiplicity: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class AggregateInvariantDefinition(
    name: String,
    expression: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class ViewDefinition(
    attributes: Vector[ViewAttributeDefinition] = Vector.empty,
    queries: Vector[ViewQueryDefinition] = Vector.empty,
    sourceEvents: Vector[String] = Vector.empty,
    rebuildable: Option[Boolean] = None,
    viewNames: Vector[String] = Vector.empty
  )

  case class ViewAttributeDefinition(
    name: String,
    datatype: Option[String] = None,
    multiplicity: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class ViewQueryDefinition(
    name: String,
    expression: Option[String] = None,
    properties: Map[String, String] = Map.empty
  )

  case class RoutingDefinition(
    name: String,
    when: Option[String] = None,
    topic: Option[String] = None,
    service: Option[String] = None,
    partition: Option[String] = None
  )

  case class SubscriptionDefinition(
    name: String,
    eventName: Option[String] = None,
    route: Option[String] = None,
    entityName: Option[String] = None,
    target: Option[String] = None,
    targets: Vector[String] = Vector.empty,
    selector: Option[String] = None,
    actionName: Option[String] = None,
    declaredTargetUpperBound: Option[Int] = None,
    activation: Option[String] = None
  )
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
    val dbColumnNameName = Vector("DBカラム名", "dbカラム名", "dbcolumnname", "db_column_name", "db column name", "column_name")
    val dbColumnTypeName = Vector("DBカラム型", "dbカラム型", "dbcolumntype", "db_column_type", "db column type", "column_type")
    val externalNameName = Vector("外部連携属性名", "external_name", "external name", "externalName")
    val derivedName = Vector("派生", "derived", "derivedFrom", "derived from")
    val minLengthName = Vector("min_length", "minlength", "min length", "最小長", "最小文字数")
    val maxLengthName = Vector("max_length", "maxlength", "max length", "最大長", "最大文字数")
    val patternName = Vector("pattern", "regex", "正規表現", "パターン")
    val formatName = Vector("format", "フォーマット", "書式")
    val objectRefName = Vector("オブジェクト参照", "エンティティ", "objectref", "entity")
    val powertypeRefName = Vector("パワータイプ", "区分", "powertyperef", "powertype")

    trait SchemaClassContainer extends Showable.Base {
      def schemaClass: SchemaClass

      def print = s"[${label_string}]${print_String}"

      protected def display_String: String = "SchemaClassContainer"

      protected def print_String: String = "SchemaClassContainer"

      protected def show_String: String = "SchemaClassContainer"
    }

    case class DelegateDefinition(
      name: String,
      multiplicity: String = "1"
    ) {
      def normalize: DelegateDefinition =
        copy(
          name = Option(name).map(_.trim).getOrElse(""),
          multiplicity = Option(multiplicity).map(_.trim).filterNot(_.isEmpty).getOrElse("1")
        )
    }

    case class Features(
      tableName: Option[String] = None,
      parentsName: List[String] = Nil,
      delegatesName: List[String] = Nil,
      delegates: List[DelegateDefinition] = Nil
    ) {
      def isEmpty = tableName.isEmpty && parentsName.isEmpty && delegatesName.isEmpty && delegates.isEmpty
      def toOption: Option[Features] = if (isEmpty) None else Some(this)

      def withTableName(p: String) = copy(tableName = Some(p))
      def addParentName(p: String) = copy(parentsName = parentsName :+ p)
      def addDelegateName(p: String) =
        addDelegate(DelegateDefinition(p))
      def addDelegate(p: DelegateDefinition) = copy(
        delegatesName = delegatesName :+ p.name,
        delegates = delegates :+ p
      )

      def add(p: Features) = copy(
        tableName = p.tableName orElse tableName
      )

      def +(rhs: Features): Features = Features(
        rhs.tableName orElse tableName,
        parentsName ::: rhs.parentsName,
        delegatesName ::: rhs.delegatesName,
        delegates ::: rhs.delegates
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
          delegates: Vector[DelegateDefinition] = Vector.empty,
          propertyTables: Vector[Table] = Vector.empty,
          attributeTables: Vector[Table] = Vector.empty,
          attributeSlots: Vector[Record] = Vector.empty,
          associationTables: Vector[Table] = Vector.empty,
          aggregationTables: Vector[Table] = Vector.empty,
          compositionTables: Vector[Table] = Vector.empty,
          powertypeTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          events: Vector[EventDefinition] = Vector.empty,
          aggregate: Option[AggregateDefinition] = None,
          view: Option[ViewDefinition] = None,
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
              delegates,
              props,
              _merge_attribute_records(attributeTables.toVector.flatMap(SimpleModelerUtils.toRecords).map(_normalize_attribute_record) ++ attributeSlots),
              associationTables,
              aggregationTables,
              compositionTables,
              powertypeTables,
              statemachines,
              events,
              aggregate,
              view
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

          private def _normalize_attribute_record(p: Record): Record = {
            val inferred = _attribute_name_from_leading_column(p)
            val normalized = Vector(
              _get_string(p, nameName).orElse(inferred).map("name" -> _),
              _get_string(p, typeName).map("type" -> _),
              _get_string(p, multiplicityName).map("multiplicity" -> _),
              _get_string(p, labelName).map("label" -> _),
              _get_string(p, Vector("description")).map("description" -> _),
              _get_string(p, derivedName).map("derived" -> _)
            ).flatten
            val normalizedKeys = normalized.map(_._1.toLowerCase(java.util.Locale.ROOT)).toSet
            val original = p.fields.toVector.flatMap { field =>
              val key = field.name.trim
              if (key.nonEmpty && !normalizedKeys.contains(key.toLowerCase(java.util.Locale.ROOT)))
                Some(key -> field.value.asString)
              else
                None
            }
            val fields = normalized ++ original
            if (fields.isEmpty) p else Record.create(fields)
          }

          private def _has_web_field(p: Record): Boolean =
            p.fields.exists(field => _normalize_record_key(field.name).startsWith("web"))

          private def _normalize_record_key(p: String): String =
            p.toLowerCase(java.util.Locale.ROOT).replaceAll("[\\s_\\-　]+", "")

          private def _attribute_name_from_leading_column(p: Record): Option[String] = {
            val reserved = (nameName ++ typeName ++ multiplicityName ++ labelName ++ Vector("description") ++ derivedName).
              map(_normalize_record_key).toSet
            p.fields.toVector.collectFirst {
              case field if !reserved.contains(_normalize_record_key(field.name)) =>
                val v = Option(field.value.asString).map(_.trim).getOrElse("")
                if (v.nonEmpty) v else field.name.trim
            }.filterNot(_.isEmpty)
          }

          private def _get_string(p: Record, names: Seq[String]): Option[String] =
            p.getStringCaseInsensitive(names.toVector).orElse {
              val keys = names.map(_.trim.toLowerCase(java.util.Locale.ROOT)).toSet
              p.fields.toStream.flatMap { field =>
              val key = field.name.trim.toLowerCase(java.util.Locale.ROOT)
                if (keys.contains(key)) Some(field.value.asString) else None
              }.headOption
            }

          private def _section(p: LogicalSection) =
            if (_is_features(p))
              _features(p)
            else if (_is_attributes(p))
              _attributes(p)
            else if (_is_delegates(p))
              _delegates(p)
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
            else if (_is_events(p))
              _events(p)
            else if (_is_aggregate(p))
              _aggregate(p)
            else if (_is_view(p))
              _view(p)
            else
              this

          private def _is_features(p: LogicalSection) =
            featureKeys.contains(p.keyForModel)

          private def _is_attributes(p: LogicalSection) =
            attributeKeys.contains(p.keyForModel)

          private def _is_delegates(p: LogicalSection) =
            delegateKeys.contains(p.keyForModel)

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

          private def _is_events(p: LogicalSection) =
            eventKeys.contains(p.keyForModel)

          private def _is_aggregate(p: LogicalSection) =
            aggregateKeys.contains(p.keyForModel)

          private def _is_view(p: LogicalSection) =
            viewKeys.contains(p.keyForModel)

          private def _features(p: LogicalSection) = {
            copy(featureTables = featureTables ++ _table_list(p))
          }

          private def _attributes(p: LogicalSection) = {
            copy(
              attributeTables = attributeTables ++ _table_list(p),
              attributeSlots = attributeSlots ++ _attribute_records_from_sections(p)
            )
          }

          private def _delegates(p: LogicalSection) = {
            copy(delegates = delegates ++ _delegate_definitions(p))
          }

          private def _delegate_definitions(p: LogicalSection): Vector[DelegateDefinition] = {
            val fromTables = _table_list(p).toVector.flatMap(SimpleModelerUtils.toRecords).flatMap(_delegate_from_record)
            val fromText = _delegate_records_from_text(_logical_body_text(p))
            val fromSections = p.sections.toVector.flatMap(s => _delegate_from_section(s))
            _merge_delegate_definitions(fromTables ++ fromText ++ fromSections)
          }

          private def _logical_body_text(p: LogicalSection): String = {
            val lines = Option(p.text).getOrElse("").split("\\r?\\n").toVector
            lines.takeWhile { x =>
              val s = x.trim
              !(s.startsWith("####") || s.startsWith("#####") || s.startsWith("######"))
            }.mkString("\n")
          }

          private def _delegate_from_section(p: LogicalSection): Option[DelegateDefinition] = {
            val kv = _key_values(p.text).toMap
            val name = kv.get("name").orElse(kv.get("delegate")).orElse {
              Option(p.nameForModel).map(_.trim).filterNot(_.isEmpty)
            }
            name.map(n => DelegateDefinition(n, kv.getOrElse("multiplicity", "1")).normalize)
          }

          private def _delegate_records_from_text(p: String): Vector[DelegateDefinition] = {
            _delegate_records_from_meta_text(p).flatMap(_delegate_from_record)
          }

          private def _delegate_from_value_line(p: String): Option[DelegateDefinition] = {
            _normalize_delegate_name_line(p).map(s => DelegateDefinition(s, "1"))
          }

          private def _delegate_from_record(p: Record): Option[DelegateDefinition] = {
            val name = p.getStringCaseInsensitive(nameName).orElse(p.getStringCaseInsensitive(Vector("delegate"))).map(_.trim).filterNot(_.isEmpty)
            val multi = p.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty).getOrElse("1")
            name.map(n => DelegateDefinition(n, multi).normalize)
          }

          private def _merge_delegate_definitions(ps: Vector[DelegateDefinition]): Vector[DelegateDefinition] = {
            case class Z(xs: Vector[(String, DelegateDefinition)]) {
              def +(rhs: DelegateDefinition): Z = {
                val n = rhs.normalize
                if (n.name.isEmpty)
                  this
                else {
                  val key = n.name.toLowerCase
                  xs.indexWhere(_._1 == key) match {
                    case -1 => copy(xs = xs :+ (key -> n))
                    case i => copy(xs = xs.updated(i, key -> n))
                  }
                }
              }
              def result: Vector[DelegateDefinition] = xs.map(_._2)
            }
            ps.foldLeft(Z(Vector.empty))(_ + _).result
          }

          private def _attribute_records_from_sections(p: LogicalSection): Vector[Record] =
            p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kv = _key_values(s.text).toMap
              val description = s.sections.find(_.keyForModel.equalsIgnoreCase("description")).map(_.text.trim).filterNot(_.isEmpty).orElse(Option(s.text).map(_.trim).filterNot(_.isEmpty))
              val summary = s.sections.find(_.keyForModel.equalsIgnoreCase("summary")).map(_.text.trim).filterNot(_.isEmpty)
              val label = s.sections.find(_.keyForModel.equalsIgnoreCase("label")).map(_.text.trim).filterNot(_.isEmpty)
              val fields = Vector(
                Some("name" -> s.nameForModel),
                kv.get("type").map("type" -> _),
                kv.get("datatype").map("type" -> _),
                kv.get("multiplicity").map("multiplicity" -> _),
                kv.get("label").orElse(label).map("label" -> _),
                kv.get("summary").orElse(summary).map("summary" -> _),
                kv.get("description").orElse(description).map("description" -> _)
              ).flatten
              Record.create(fields)
            }

          private def _merge_attribute_records(records: Vector[Record]): Vector[Record] = {
            case class Z(xs: Vector[(String, Record)], anon: Vector[Record]) {
              def +(rhs: Record): Z =
                rhs.getStringCaseInsensitive(nameName).map(_.trim).filterNot(_.isEmpty) match {
                  case Some(name) =>
                    val key = name.toLowerCase
                    xs.indexWhere(_._1 == key) match {
                      case -1 => copy(xs = xs :+ (key -> rhs))
                      case i => copy(xs = xs.updated(i, key -> _merge_attribute_record(xs(i)._2, rhs)))
                    }
                  case None => copy(anon = anon :+ rhs)
                }
              def result: Vector[Record] = xs.map(_._2) ++ anon
            }
            records.foldLeft(Z(Vector.empty, Vector.empty))(_ + _).result
          }

          private def _merge_attribute_record(lhs: Record, rhs: Record): Record = {
            val names = (lhs.fields.map(_.name) ++ rhs.fields.map(_.name)).distinct
            val fields = names.flatMap { key =>
              rhs.fields.find(_.name == key).orElse(lhs.fields.find(_.name == key)).map(x => Field.create(key, x.asString))
            }
            Record(fields)
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

          private def _events(p: LogicalSection) = {
            val xs = p.sections.map(_event_definition).filter(_.name.nonEmpty)
            copy(events = events ++ xs)
          }

          private def _aggregate(p: LogicalSection) =
            copy(aggregate = Some(_aggregate_definition(p)))

          private def _view(p: LogicalSection) =
            copy(view = Some(_view_definition(p)))

          private def _event_definition(p: LogicalSection): EventDefinition = {
            val kv = _key_values(p.text)
            val rec = Record.create(kv)
            if (kv.exists { case (k, _) => k == "view" || k == "viewname" || k == "view_name" })
              RAISE.syntaxErrorFault(s"Event '${p.nameForModel}' cannot depend on View.")
            val category = rec.getStringCaseInsensitive(Vector("category")).map(_normalize_event_category(_, p.nameForModel)).getOrElse("NonActionEvent")
            val kind = rec.getStringCaseInsensitive(Vector("kind")).map(_.trim).filterNot(_.isEmpty)
            val actionname = rec.getStringCaseInsensitive(Vector("actionname", "action_name")).map(_.trim).filterNot(_.isEmpty)
            val priority = rec.getStringCaseInsensitive(Vector("priority")).map(_to_int_or_raise(_, p.nameForModel)).getOrElse(0)
            val selectors = kv.collect {
              case (k, v) if k == "selector" =>
                _selector_pair(v, p.nameForModel)
            }.toMap
            EventDefinition(
              name = p.nameForModel.trim,
              category = category,
              kind = kind,
              selectors = selectors,
              actionName = actionname,
              priority = priority
            )
          }

          private def _aggregate_definition(p: LogicalSection): AggregateDefinition = {
            val membersections = p.sections.filter(x => x.keyForModel == "member" || x.keyForModel == "members")
            val createsections = p.sections.filter(x => x.keyForModel == "create" || x.keyForModel == "creates")
            val commandsections = p.sections.filter(x => x.keyForModel == "command" || x.keyForModel == "commands")
            val statesections = p.sections.filter(x => x.keyForModel == "state" || x.keyForModel == "states")
            val invariantsections = p.sections.filter(x => x.keyForModel == "invariant" || x.keyForModel == "invariants")
            val members = membersections.toVector.flatMap(_aggregate_member_definitions)
            val creates = createsections.toVector.flatMap(_.sections).map(_aggregate_create_definition)
            val commands = commandsections.toVector.flatMap(_.sections).map(_aggregate_command_definition)
            val state = statesections.toVector.flatMap(_aggregate_state_definitions)
            val invariants = invariantsections.toVector.flatMap(_.sections).map(_aggregate_invariant_definition)
            AggregateDefinition(members = members, creates = creates, commands = commands, state = state, invariants = invariants)
          }

          private def _aggregate_member_definitions(
            p: LogicalSection
          ): Vector[AggregateMemberDefinition] = {
            val fromTables = _table_list(p).toVector.flatMap(_aggregate_member_rows).map {
              case (name, entity, kind, join, multi, joinField, props) =>
                AggregateMemberDefinition(
                  name = name,
                  entity = entity,
                  kind = kind,
                  boundary = props.getOrElse("boundary", props.getOrElse("scope", "internal")),
                  join = join.orElse(props.get("join")).orElse(props.get("join_strategy")).orElse(props.get("joinstrategy")).orElse(props.get("join_kind")).orElse(props.get("joinkind")),
                  multiplicity = multi,
                  joinField = joinField,
                  properties = props
                )
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kvv = _key_values(s.text)
              val kv = kvv.toMap
              val rec = Record.create(kvv)
              AggregateMemberDefinition(
                name = s.nameForModel,
                entity = rec.getStringCaseInsensitive(Vector("entity", "objectref")).getOrElse(""),
                kind = rec.getStringCaseInsensitive(Vector("kind")).getOrElse("composition"),
                boundary = rec.getStringCaseInsensitive(Vector("boundary", "scope")).getOrElse("internal"),
                join = rec.getStringCaseInsensitive(Vector("join", "join_strategy", "joinstrategy", "join_kind", "joinkind")),
                multiplicity = rec.getStringCaseInsensitive(multiplicityName),
                joinField = rec.getStringCaseInsensitive(Vector("join_field", "joinfield")),
                properties = kv
              )
            }.filter(_.entity.nonEmpty)
            fromTables ++ fromSections
          }

          private def _aggregate_command_definition(
            p: LogicalSection
          ): AggregateCommandDefinition = {
            val kv = _key_values(p.text)
            val rec = Record.create(kv)
            val props = kv.toMap
            val input = kv.collect {
              case (k, v) if k == "input" || k.startsWith("input.") => k -> v
            }.toMap
            val validations = kv.collect {
              case (k, v) if k == "validate" || k == "validation" || k == "guard" => v
            }
            val events = kv.collect {
              case (k, v) if k == "event" || k == "emit" || k == "events" =>
                _split_list(v)
            }.flatten
            val newstate = rec.getStringCaseInsensitive(Vector("newstate", "new_state", "state"))
            val implementation = rec.getStringCaseInsensitive(Vector("implementation", "impl"))
            AggregateCommandDefinition(
              name = p.nameForModel,
              input = input,
              validations = validations,
              events = events,
              newState = newstate,
              implementation = implementation,
              properties = props
            )
          }

          private def _aggregate_create_definition(
            p: LogicalSection
          ): AggregateCreateDefinition = {
            val kv = _key_values(p.text)
            val rec = Record.create(kv)
            val props = kv.toMap
            val input = kv.collect {
              case (k, v) if k == "input" || k.startsWith("input.") => k -> v
            }.toMap
            val validations = kv.collect {
              case (k, v) if k == "validate" || k == "validation" || k == "guard" => v
            }
            val events = kv.collect {
              case (k, v) if k == "event" || k == "emit" || k == "events" =>
                _split_list(v)
            }.flatten
            val initialstate = rec.getStringCaseInsensitive(Vector("initialstate", "initial_state", "state"))
            val implementation = rec.getStringCaseInsensitive(Vector("implementation", "impl"))
            AggregateCreateDefinition(
              name = p.nameForModel,
              input = input,
              validations = validations,
              events = events,
              initialState = initialstate,
              implementation = implementation.orElse(Some("pattern:create")),
              properties = props
            )
          }

          private def _aggregate_state_definitions(
            p: LogicalSection
          ): Vector[AggregateStateDefinition] = {
            val fromTables = _table_list(p).toVector.flatMap(_attribute_rows_for_view_or_aggregate_state).map {
              case (name, tpe, multi, props) =>
                AggregateStateDefinition(name, tpe, multi, props)
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kvv = _key_values(s.text)
              val kv = kvv.toMap
              val rec = Record.create(kvv)
              AggregateStateDefinition(
                name = s.nameForModel,
                datatype = rec.getStringCaseInsensitive(typeName),
                multiplicity = rec.getStringCaseInsensitive(multiplicityName),
                properties = kv
              )
            }
            _merge_named_logical(fromTables ++ fromSections)(_.name) { (lhs, rhs) =>
              AggregateStateDefinition(
                name = lhs.name,
                datatype = rhs.datatype.orElse(lhs.datatype),
                multiplicity = rhs.multiplicity.orElse(lhs.multiplicity),
                properties = lhs.properties ++ rhs.properties
              )
            }
          }

          private def _aggregate_invariant_definition(
            p: LogicalSection
          ): AggregateInvariantDefinition = {
            val kvv = _key_values(p.text)
            val kv = kvv.toMap
            val rec = Record.create(kvv)
            AggregateInvariantDefinition(
              name = p.nameForModel,
              expression = rec.getStringCaseInsensitive(Vector("expression", "expr", "guard")),
              properties = kv
            )
          }

          private def _view_definition(
            p: LogicalSection
          ): ViewDefinition = {
            if (_contains_view_mutation_marker(p))
              RAISE.syntaxErrorFault(s"View '${p.nameForModel}' cannot mutate command-side state.")
            val attributesections = p.sections.filter(x => x.keyForModel == "attribute" || x.keyForModel == "attributes")
            val querysections = p.sections.filter(x => x.keyForModel == "query" || x.keyForModel == "queries")
            val attrs = attributesections.toVector.flatMap(_view_attribute_definitions)
            val queries = querysections.toVector.flatMap(_.sections).map(_view_query_definition)
            val rootkvv = _key_values(p.text)
            val rootkv = rootkvv.toMap
            val rootrec = Record.create(rootkvv)
            val sourceevents = rootkv.get("events").toVector.flatMap(_split_list) ++ rootkv.get("event").toVector.flatMap(_split_list)
            val rebuildable = rootrec.getStringCaseInsensitive(Vector("rebuildable")).map(_.equalsIgnoreCase("true"))
            val viewnames = rootkv.get("views").toVector.flatMap(_split_list) ++ rootkv.get("view").toVector.flatMap(_split_list) ++ rootkv.get("viewname").toVector.flatMap(_split_list) ++ rootkv.get("view_name").toVector.flatMap(_split_list)
            ViewDefinition(
              attributes = attrs,
              queries = queries,
              sourceEvents = sourceevents.distinct,
              rebuildable = rebuildable,
              viewNames = viewnames.map(_.trim).filterNot(_.isEmpty).distinct
            )
          }

          private def _contains_view_mutation_marker(
            p: LogicalSection
          ): Boolean = {
            def _all(s: LogicalSection): Vector[(String, String)] =
              _key_values(s.text) ++ s.sections.toVector.flatMap(_all)
            val keys = _all(p).map(_._1).toSet
            keys.exists(k => k == "mutates" || k == "mutation" || k == "write" || k == "action")
          }

          private def _view_attribute_definitions(
            p: LogicalSection
          ): Vector[ViewAttributeDefinition] = {
            val fromTables = _table_list(p).toVector.flatMap(_attribute_rows_for_view_or_aggregate_state).map {
              case (name, tpe, multi, props) =>
                ViewAttributeDefinition(name, tpe, multi, props)
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kvv = _key_values(s.text)
              val kv = kvv.toMap
              val rec = Record.create(kvv)
              ViewAttributeDefinition(
                name = s.nameForModel,
                datatype = rec.getStringCaseInsensitive(typeName),
                multiplicity = rec.getStringCaseInsensitive(multiplicityName),
                properties = kv
              )
            }
            _merge_named_logical(fromTables ++ fromSections)(_.name) { (lhs, rhs) =>
              ViewAttributeDefinition(
                name = lhs.name,
                datatype = rhs.datatype.orElse(lhs.datatype),
                multiplicity = rhs.multiplicity.orElse(lhs.multiplicity),
                properties = lhs.properties ++ rhs.properties
              )
            }
          }

          private def _view_query_definition(
            p: LogicalSection
          ): ViewQueryDefinition = {
            val kvv = _key_values(p.text)
            val kv = kvv.toMap
            val rec = Record.create(kvv)
            if (kv.contains("mutates") || kv.contains("mutation") || kv.contains("write") || kv.contains("action"))
              RAISE.syntaxErrorFault(s"View Query '${p.nameForModel}' cannot mutate command-side state.")
            ViewQueryDefinition(
              name = p.nameForModel,
              expression = rec.getStringCaseInsensitive(Vector("expression", "expr")),
              properties = kv
            )
          }

          private def _attribute_rows_for_view_or_aggregate_state(
            table: Table
          ): Vector[(String, Option[String], Option[String], Map[String, String])] = {
            val records = SimpleModelerUtils.toRecords(table).toVector
            records.flatMap { r =>
              val name = r.getStringCaseInsensitive(nameName)
              name.filterNot(_.trim.isEmpty).map { n =>
                val dtype = r.getStringCaseInsensitive(typeName).map(_.trim).filterNot(_.isEmpty)
                val mult = r.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty)
                val props = r.fields.map(f => f.name.toLowerCase -> f.asString).toMap
                (n, dtype, mult, props)
              }
            }
          }

          private def _aggregate_member_rows(
            table: Table
          ): Vector[(String, String, String, Option[String], Option[String], Option[String], Map[String, String])] = {
            val records = SimpleModelerUtils.toRecords(table).toVector
            records.flatMap { r =>
              val name = r.getStringCaseInsensitive(nameName).map(_.trim).filterNot(_.isEmpty)
              val entity = r.getStringCaseInsensitive(objectRefName).map(_.trim).filterNot(_.isEmpty)
              (name, entity) match {
                case (Some(n), Some(e)) =>
                  val kind = r.getStringCaseInsensitive(Vector("kind")).map(_.trim).filterNot(_.isEmpty).getOrElse("composition")
                  val join = r.getStringCaseInsensitive(Vector("join", "join_strategy", "joinstrategy", "join_kind", "joinkind")).map(_.trim).filterNot(_.isEmpty)
                  val mult = r.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty)
                  val joinfield = r.getStringCaseInsensitive(Vector("join_field", "joinfield")).map(_.trim).filterNot(_.isEmpty)
                  val props = r.fields.map(f => f.name.toLowerCase -> f.asString).toMap
                  Some((n, e, kind, join, mult, joinfield, props))
                case _ =>
                  None
              }
            }
          }

          private def _split_list(p: String): Vector[String] =
            p.split(",").toVector.map(_.trim).filterNot(_.isEmpty)

          private def _merge_named_logical[A](
            ps: Vector[A]
          )(name: A => String)(merge: (A, A) => A): Vector[A] = {
            case class Z(xs: Vector[(String, A)]) {
              def +(rhs: A): Z = {
                val key = name(rhs).trim.toLowerCase
                xs.indexWhere(_._1 == key) match {
                  case -1 => copy(xs = xs :+ (key -> rhs))
                  case i => copy(xs = xs.updated(i, key -> merge(xs(i)._2, rhs)))
                }
              }
              def result: Vector[A] = xs.map(_._2)
            }
            ps.foldLeft(Z(Vector.empty))(_ + _).result
          }

          private def _normalize_event_category(p: String, eventname: String): String =
            p.trim.toLowerCase match {
              case "actionevent" | "action" => "ActionEvent"
              case "nonactionevent" | "non-action" | "nonaction" => "NonActionEvent"
              case s =>
                RAISE.syntaxErrorFault(s"Event '$eventname' has invalid category: '$s'. Use ActionEvent or NonActionEvent.")
            }

          private def _selector_pair(p: String, eventname: String): (String, String) = {
            val i = p.indexOf("=")
            if (i <= 0)
              RAISE.syntaxErrorFault(s"Event '$eventname' selector requires key=value format: '$p'.")
            else {
              val k = p.substring(0, i).trim
              val v = p.substring(i + 1).trim
              if (k.isEmpty || v.isEmpty)
                RAISE.syntaxErrorFault(s"Event '$eventname' selector requires key=value format: '$p'.")
              else
                k -> v
            }
          }

          private def _to_int_or_raise(p: String, eventname: String): Int =
            scala.util.Try(p.trim.toInt).getOrElse(
              RAISE.syntaxErrorFault(s"Event '$eventname' has invalid priority: '$p'.")
            )

          private def _statemachine(p: LogicalSection): Option[StateMachineClass] = {
            val f = KaleidoxStateMachineLogic.Factory
            _parse_statemachine_cml_for_resource(f, p).orElse(
              StateMachineClass.parseBodyForResource(f, p.nameForModel, p.text).toOption
            )
          }

          private def _parse_statemachine_cml_for_resource(
            factory: StateMachineLogic.Factory,
            p: LogicalSection
          ): Option[StateMachineClass] = {
            val statesection = p.sections.find(_.keyForModel.equalsIgnoreCase("state"))
            statesection.flatMap { ss =>
              val machinename = p.nameForModel
              val events = _event_names(p)
              val states = ss.sections.toList.zipWithIndex.map {
                case (s, i) => _state_from_section(machinename, s, i)
              }
              if (states.isEmpty)
                None
              else {
                _validate_state_transitions(machinename, states, events)
                val rule = StateMachineRule(
                  name = Some(p.nameForModel),
                  kind = StateMachineKind.Resource,
                  states = states
                )
                Some(StateMachineClass(p.nameForModel, rule, factory.create(rule)))
              }
            }
          }

          private def _event_names(p: LogicalSection): Set[String] =
            p.sections.find(_.keyForModel.equalsIgnoreCase("event")).toVector.flatMap(_.sections.map(_.nameForModel.trim)).filterNot(_.isEmpty).toSet

          private def _state_from_section(
            machinename: String,
            p: LogicalSection,
            index: Int
          ): StateClass = {
            val entry = _activity_from_actions(_collect_action_lines(p, "entry"))
            val exit = _activity_from_actions(_collect_action_lines(p, "exit"))
            val ts = p.sections.filter(_.keyForModel.equalsIgnoreCase("transition")).map(_transition_from_section(machinename, p.nameForModel, _))
            StateClass(
              name = p.nameForModel,
              value = _state_value(p, index),
              stateMachinePath = None,
              transitions = Transitions.call(ts)
            ).withEntryActivity(entry).withExitActivity(exit)
          }

          private def _state_value(p: LogicalSection, index: Int): Int =
            _key_values(p.text).collectFirst {
              case (k, v) if k == "value" => v.trim.stripPrefix("\"").stripSuffix("\"").toInt
            }.getOrElse(index + 1)

          private def _collect_action_lines(p: LogicalSection, key: String): Vector[String] = {
            p.sections.filter(_.keyForModel.equalsIgnoreCase(key)).flatMap { x =>
              _key_values(x.text).collect {
                case (k, v) if k == "action" => v
              }
            }.toVector
          }

          private def _transition_from_section(
            machinename: String,
            statename: String,
            p: LogicalSection
          ): Transition = {
            val kv = _key_values(p.text)
            val to = _required_transition_key(kv, "to", machinename, statename)
            val on = _required_transition_key(kv, "on", machinename, statename)
            val guard = kv.collectFirst {
              case (k, v) if k == "guard" => v
            }
            val actions = kv.collect {
              case (k, v) if k == "action" => v
            }
            Transition(
              guard = _transition_guard(on, guard),
              to = _transition_to(to),
              effect = _activity_from_actions(actions)
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
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition requires '$key'.")
            }

          private def _validate_state_transitions(
            machinename: String,
            states: Seq[StateClass],
            events: Set[String]
          ): Unit = {
            val statenames = states.map(_.name).toSet
            states.foreach { s =>
              s.transitions.call.foreach { t =>
                _validate_transition_target(machinename, s.name, t, statenames)
                _validate_transition_event(machinename, s.name, t, events)
              }
            }
          }

          private def _validate_transition_target(
            machinename: String,
            statename: String,
            t: Transition,
            statenames: Set[String]
          ): Unit =
            t.to match {
              case NameTransitionTo(name) =>
                if (!statenames.contains(name))
                  RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition target '$name' is not defined.")
              case _ =>
            }

          // Policy: when Event section is present, transitions must reference declared events.
          // When Event section is omitted, events are accepted as implicit declarations.
          private def _validate_transition_event(
            machinename: String,
            statename: String,
            t: Transition,
            events: Set[String]
          ): Unit = {
            val eventname = _event_name(t.guard).getOrElse {
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition requires 'on'.")
            }
            if (events.nonEmpty && !events.contains(eventname))
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' references undeclared event '$eventname'.")
          }

          private def _event_name(guard: SmGuard): Option[String] =
            guard match {
              case EventNameGuard(name) => Some(name)
              case AndGuard(exprs) => exprs.toStream.flatMap(_event_name).headOption
              case OrGuard(exprs) => exprs.toStream.flatMap(_event_name).headOption
              case _ => None
            }

          private def _transition_guard(on: String, guard: Option[String]): SmGuard =
            guard.filterNot(_.isEmpty) match {
              case Some(g) => AndGuard(Vector(EventNameGuard(on), CmlExpressionGuard(g)))
              case None => EventNameGuard(on)
          }

          private def _transition_to(p: String): TransitionTo =
            if (p.equalsIgnoreCase(PROP_STATE_FINAL))
              FinalTransitionTo
            else if (p.equalsIgnoreCase(PROP_STATE_HISTORY))
              HistoryTransitionTo()
            else
              NameTransitionTo(p)

          private def _activity_from_actions(actions: Seq[String]): Activity =
            actions.toList match {
              case Nil => Activity.Empty
              case x :: Nil => Activity.Opaque(x)
              case xs => Activity.Opaque(xs.mkString("\n"))
            }

          private def _key_values(p: String): Vector[(String, String)] = {
            CmlSectionFormat.keyValues(p)
          }

          private def _table_list(p: LogicalSection): List[Table] = {
            val doxconfig = Dox2Parser.Config.default // TODO
            val dox = Dox2Parser.parseSection(doxconfig, p)
            dox.tableList
          }
        }

        p.blocks.blocks.foldLeft(Z())(_+_).r
      }

      def createOption(p: Section): Option[SchemaClass] = {
        case class Z(
          featureTables: Vector[Table] = Vector.empty,
          delegates: Vector[DelegateDefinition] = Vector.empty,
          propertyTables: Vector[Table] = Vector.empty,
          attributeSlots: Vector[Record] = Vector.empty,
          associationTables: Vector[Table] = Vector.empty,
          aggregationTables: Vector[Table] = Vector.empty,
          compositionTables: Vector[Table] = Vector.empty,
          powertypeTables: Vector[Table] = Vector.empty,
          statemachines: Vector[StateMachineClass] = Vector.empty,
          events: Vector[EventDefinition] = Vector.empty,
          aggregate: Option[AggregateDefinition] = None,
          view: Option[ViewDefinition] = None,
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
              delegates,
              props,
              attributeSlots,
              associationTables,
              aggregationTables,
              compositionTables,
              powertypeTables,
              statemachines,
              events,
              aggregate,
              view
            )
          }

          def +(rhs: Dox) = rhs match {
            case m: Table => _table(m)
            case m: Section => _section(m)
            case m: Paragraph => this
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
            else if (_is_delegates(p))
              _delegates(p)
            else if (_is_statemachines(p))
              _statemachines(p)
            else if (_is_events(p))
              _events(p)
            else if (_is_aggregate(p))
              _aggregate(p)
            else if (_is_view(p))
              _view(p)
            else
              this

          private def _is_attributes(p: Section) =
            attributeKeys.contains(p.keyForModel)

          private def _is_delegates(p: Section) =
            delegateKeys.contains(p.keyForModel)

          private def _is_statemachines(p: Section) = 
            statemachineKeys.contains(p.keyForModel)

          private def _is_events(p: Section) =
            eventKeys.contains(p.keyForModel)

          private def _is_aggregate(p: Section) =
            aggregateKeys.contains(p.keyForModel)

          private def _is_view(p: Section) =
            viewKeys.contains(p.keyForModel)

          private def _attributes(p: Section) =
            copy(attributeSlots = this.attributeSlots ++ _attribute_records(p))

          private def _delegates(p: Section) =
            copy(delegates = delegates ++ _delegate_definitions(p))

          private def _delegate_definitions(p: Section): Vector[DelegateDefinition] = {
            val fromTables = p.tableList.toVector.flatMap(SimpleModelerUtils.toRecords).flatMap(_delegate_from_record)
            val fromItems = _delegate_records_from_items(p)
            val fromText = _delegate_records_from_text(_section_body_text(p))
            val fromSections = p.sections.toVector.flatMap(_delegate_from_section)
            _merge_delegate_definitions(fromTables ++ fromItems ++ fromText ++ fromSections)
          }

          private def _delegate_records_from_items(p: Section): Vector[DelegateDefinition] = {
            val fromUl = p.uls.toVector.flatMap { ul =>
              ul.contents.toVector.flatMap { li =>
                _delegate_from_value_line(li.toText)
              }
            }
            val fromDl = p.dls.toVector.flatMap { dl =>
              dl.contents.toVector.flatMap {
                case (dt, dd) =>
                  _delegate_from_record(Record.create(Vector("name" -> dt.toText, "multiplicity" -> dd.toText)))
              }
            }
            fromUl ++ fromDl
          }

          private def _delegate_from_section(p: Section): Option[DelegateDefinition] = {
            val kv = _merged_key_values(p).toMap
            val name = kv.get("name").orElse(kv.get("delegate")).orElse(Option(p.nameForModel).map(_.trim).filterNot(_.isEmpty))
            name.map(n => DelegateDefinition(n, kv.getOrElse("multiplicity", "1")).normalize)
          }

          private def _delegate_records_from_text(p: String): Vector[DelegateDefinition] = {
            _delegate_records_from_meta_text(p).flatMap(_delegate_from_record)
          }

          private def _delegate_from_value_line(p: String): Option[DelegateDefinition] = {
            _normalize_delegate_name_line(p).map(s => DelegateDefinition(s, "1"))
          }

          private def _delegate_from_record(p: Record): Option[DelegateDefinition] = {
            val name = p.getStringCaseInsensitive(nameName).orElse(p.getStringCaseInsensitive(Vector("delegate"))).map(_.trim).filterNot(_.isEmpty)
            val multi = p.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty).getOrElse("1")
            name.map(n => DelegateDefinition(n, multi).normalize)
          }

          private def _merge_delegate_definitions(ps: Vector[DelegateDefinition]): Vector[DelegateDefinition] = {
            case class Z(xs: Vector[(String, DelegateDefinition)]) {
              def +(rhs: DelegateDefinition): Z = {
                val n = rhs.normalize
                if (n.name.isEmpty)
                  this
                else {
                  val key = n.name.toLowerCase
                  xs.indexWhere(_._1 == key) match {
                    case -1 => copy(xs = xs :+ (key -> n))
                    case i => copy(xs = xs.updated(i, key -> n))
                  }
                }
              }
              def result: Vector[DelegateDefinition] = xs.map(_._2)
            }
            ps.foldLeft(Z(Vector.empty))(_ + _).result
          }

          private def _attribute_records(p: Section): Vector[Record] = {
            val fromTables = p.tableList.toVector.flatMap(SimpleModelerUtils.toRecords).map(_normalize_attribute_record)
            val fromItems = _attribute_records_from_items(p)
            val fromText = _attribute_records_from_text(_section_body_text(p))
            val fromSections = _attribute_records_from_sections(p)
            _merge_attribute_records(fromTables ++ fromItems ++ fromText ++ fromSections)
          }

          private def _normalize_attribute_record(p: Record): Record = {
            val inferred = _attribute_name_from_leading_column(p)
            val normalized = Vector(
              _get_string(p, nameName).orElse(inferred).map("name" -> _),
              _get_string(p, typeName).map("type" -> _),
              _get_string(p, multiplicityName).map("multiplicity" -> _),
              _get_string(p, labelName).map("label" -> _),
              _get_string(p, Vector("description")).map("description" -> _),
              _get_string(p, derivedName).map("derived" -> _)
            ).flatten
            val normalizedKeys = normalized.map(_._1.toLowerCase(java.util.Locale.ROOT)).toSet
            val original = p.fields.toVector.flatMap { field =>
              val key = field.name.trim
              if (key.nonEmpty && !normalizedKeys.contains(key.toLowerCase(java.util.Locale.ROOT)))
                Some(key -> field.value.asString)
              else
                None
            }
            val fields = normalized ++ original
            if (fields.isEmpty) p else Record.create(fields)
          }

          private def _has_web_field(p: Record): Boolean =
            p.fields.exists(field => _normalize_record_key(field.name).startsWith("web"))

          private def _normalize_record_key(p: String): String =
            p.toLowerCase(java.util.Locale.ROOT).replaceAll("[\\s_\\-　]+", "")

          private def _attribute_name_from_leading_column(p: Record): Option[String] = {
            val reserved = (nameName ++ typeName ++ multiplicityName ++ labelName ++ Vector("description") ++ derivedName).
              map(_normalize_record_key).toSet
            p.fields.toVector.collectFirst {
              case field if !reserved.contains(_normalize_record_key(field.name)) =>
                val v = Option(field.value.asString).map(_.trim).getOrElse("")
                if (v.nonEmpty) v else field.name.trim
            }.filterNot(_.isEmpty)
          }

          private def _get_string(p: Record, names: Seq[String]): Option[String] =
            p.getStringCaseInsensitive(names.toVector).orElse {
              val keys = names.map(_.trim.toLowerCase(java.util.Locale.ROOT)).toSet
              p.fields.toStream.flatMap { field =>
                val key = field.name.trim.toLowerCase(java.util.Locale.ROOT)
                if (keys.contains(key)) Some(field.value.asString) else None
              }.headOption
            }

          private def _attribute_records_from_items(p: Section): Vector[Record] = {
            val fromUl = p.uls.toVector.flatMap { ul =>
              ul.contents.toVector.flatMap { li =>
                _attribute_record(li.toText)
              }
            }
            val fromDl = p.dls.toVector.flatMap { dl =>
              dl.contents.toVector.flatMap {
                case (dt, dd) =>
                  _attribute_record(s"${dt.toText}\n${dd.toText}")
              }
            }
            fromUl ++ fromDl
          }

          private def _attribute_records_from_sections(p: Section): Vector[Record] =
            p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kv = _merged_key_values(s).toMap
              val description = s.sections.find(_.keyForModel.equalsIgnoreCase("description")).map(_.toText.trim).filterNot(_.isEmpty).orElse(Option(s.toText).map(_.trim).filterNot(_.isEmpty))
              val summary = s.sections.find(_.keyForModel.equalsIgnoreCase("summary")).map(_.toText.trim).filterNot(_.isEmpty)
              val label = s.sections.find(_.keyForModel.equalsIgnoreCase("label")).map(_.toText.trim).filterNot(_.isEmpty)
              val fields = Vector(
                Some("name" -> s.nameForModel),
                kv.get("type").map("type" -> _),
                kv.get("datatype").map("type" -> _),
                kv.get("multiplicity").map("multiplicity" -> _),
                kv.get("label").orElse(label).map("label" -> _),
                kv.get("summary").orElse(summary).map("summary" -> _),
                kv.get("description").orElse(description).map("description" -> _)
              ).flatten
              Record.create(fields)
            }

          private def _merge_attribute_records(records: Vector[Record]): Vector[Record] = {
            case class Z(xs: Vector[(String, Record)], anon: Vector[Record]) {
              def +(rhs: Record): Z =
                rhs.getStringCaseInsensitive(nameName).map(_.trim).filterNot(_.isEmpty) match {
                  case Some(name) =>
                    val key = name.toLowerCase
                    xs.indexWhere(_._1 == key) match {
                      case -1 => copy(xs = xs :+ (key -> rhs))
                      case i =>
                        val merged = _merge_attribute_record(xs(i)._2, rhs)
                        copy(xs = xs.updated(i, key -> merged))
                    }
                  case None =>
                    copy(anon = anon :+ rhs)
                }
              def result: Vector[Record] = xs.map(_._2) ++ anon
            }
            records.foldLeft(Z(Vector.empty, Vector.empty))(_ + _).result
          }

          private def _merge_attribute_record(lhs: Record, rhs: Record): Record = {
            val names = (lhs.fields.map(_.name) ++ rhs.fields.map(_.name)).distinct
            val fields = names.flatMap { key =>
              rhs.fields.find(_.name == key).orElse(lhs.fields.find(_.name == key)).map(x => Field.create(key, x.asString))
            }
            Record(fields)
          }

          private def _attribute_records_from_text(p: String): Vector[Record] = {
            val structured = _records_from_meta_text(p)
            if (structured.nonEmpty)
              structured
            else {
              val pairs = _attribute_pairs(p)
              if (pairs.isEmpty)
                Vector.empty
              else {
                val records = _attribute_pair_records(pairs)
                records.map(_to_record)
              }
            }
          }

          private def _attribute_record(p: String): Option[Record] = {
            val xs = _attribute_pairs(p)
            if (xs.isEmpty)
              None
            else
              Some(_to_record(xs))
          }

          private def _attribute_pair_records(pairs: Vector[(String, String)]): Vector[Vector[(String, String)]] = {
            case class Z(
              xs: Vector[Vector[(String, String)]],
              current: Vector[(String, String)]
            ) {
              def +(rhs: (String, String)): Z = {
                val key = rhs._1.trim.toLowerCase
                if (key == "name" && current.nonEmpty)
                  copy(xs = xs :+ current, current = Vector(rhs))
                else
                  copy(current = current :+ rhs)
              }
              def result: Vector[Vector[(String, String)]] =
                if (current.isEmpty) xs else xs :+ current
            }
            pairs.foldLeft(Z(Vector.empty, Vector.empty))(_ + _).result.filter(_.nonEmpty)
          }

          private val _attribute_key_value_pattern =
            """(?i)(?:^|[\s\-\*])([A-Za-z_][A-Za-z0-9_.-]*)\s*(?:::|:|=)\s*(".*?"|'.*?'|[^\s]+)""".r

          private def _attribute_pairs(p: String): Vector[(String, String)] = {
            val xs = _attribute_key_value_pattern.findAllMatchIn(Option(p).getOrElse("")).toVector.map { m =>
              val k = m.group(1).trim.toLowerCase
              val v = m.group(2).trim
              k -> v
            }
            if (xs.nonEmpty)
              xs
            else
              CmlSectionFormat.keyValues(p)
          }

          private def _to_record(p: Vector[(String, String)]): Record =
            Record(p.map { case (k, v) => Field.create(k, v) })

          private def _statemachines(p: Section) = {
            val xs = p.sections.flatMap(_statemachine)
            copy(statemachines = statemachines ++ xs)
          }

          private def _events(p: Section) = {
            val xs = p.sections.map(_event_definition).filter(_.name.nonEmpty)
            copy(events = events ++ xs)
          }

          private def _aggregate(p: Section) =
            copy(aggregate = Some(_aggregate_definition(p)))

          private def _view(p: Section) =
            copy(view = Some(_view_definition(p)))

          private def _event_definition(p: Section): EventDefinition = {
            val kv = _key_values(p.toText)
            val rec = Record.create(kv)
            if (kv.exists { case (k, _) => k == "view" || k == "viewname" || k == "view_name" })
              RAISE.syntaxErrorFault(s"Event '${p.nameForModel}' cannot depend on View.")
            val category = rec.getStringCaseInsensitive(Vector("category")).map(_normalize_event_category(_, p.nameForModel)).getOrElse("NonActionEvent")
            val kind = rec.getStringCaseInsensitive(Vector("kind")).map(_.trim).filterNot(_.isEmpty)
            val actionname = rec.getStringCaseInsensitive(Vector("actionname", "action_name")).map(_.trim).filterNot(_.isEmpty)
            val priority = rec.getStringCaseInsensitive(Vector("priority")).map(_to_int_or_raise(_, p.nameForModel)).getOrElse(0)
            val selectors = kv.collect {
              case (k, v) if k == "selector" =>
                _selector_pair(v, p.nameForModel)
            }.toMap
            EventDefinition(
              name = p.nameForModel.trim,
              category = category,
              kind = kind,
              selectors = selectors,
              actionName = actionname,
              priority = priority
            )
          }

          private def _aggregate_definition(p: Section): AggregateDefinition = {
            val membersections = p.sections.filter(x => x.keyForModel == "member" || x.keyForModel == "members")
            val createsections = p.sections.filter(x => x.keyForModel == "create" || x.keyForModel == "creates")
            val commandsections = p.sections.filter(x => x.keyForModel == "command" || x.keyForModel == "commands")
            val statesections = p.sections.filter(x => x.keyForModel == "state" || x.keyForModel == "states")
            val invariantsections = p.sections.filter(x => x.keyForModel == "invariant" || x.keyForModel == "invariants")
            val members = membersections.toVector.flatMap(_aggregate_member_definitions)
            val creates = createsections.toVector.flatMap(_.sections).map(_aggregate_create_definition)
            val commands = commandsections.toVector.flatMap(_.sections).map(_aggregate_command_definition)
            val state = statesections.toVector.flatMap(_aggregate_state_definitions)
            val invariants = invariantsections.toVector.flatMap(_.sections).map(_aggregate_invariant_definition)
            AggregateDefinition(members = members, creates = creates, commands = commands, state = state, invariants = invariants)
          }

          private def _aggregate_member_definitions(
            p: Section
          ): Vector[AggregateMemberDefinition] = {
            val fromTables = p.tableList.toVector.flatMap(_aggregate_member_rows).map {
              case (name, entity, kind, join, multi, joinField, props) =>
                AggregateMemberDefinition(
                  name = name,
                  entity = entity,
                  kind = kind,
                  boundary = props.getOrElse("boundary", props.getOrElse("scope", "internal")),
                  join = join.orElse(props.get("join")).orElse(props.get("join_strategy")).orElse(props.get("joinstrategy")).orElse(props.get("join_kind")).orElse(props.get("joinkind")),
                  multiplicity = multi,
                  joinField = joinField,
                  properties = props
                )
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kv = _merged_key_values(s).toMap
              AggregateMemberDefinition(
                name = s.nameForModel,
                entity = kv.getOrElse("entity", kv.getOrElse("objectref", "")),
                kind = kv.getOrElse("kind", "composition"),
                boundary = kv.getOrElse("boundary", kv.getOrElse("scope", "internal")),
                join = kv.get("join").orElse(kv.get("join_strategy")).orElse(kv.get("joinstrategy")).orElse(kv.get("join_kind")).orElse(kv.get("joinkind")),
                multiplicity = kv.get("multiplicity"),
                joinField = kv.get("join_field").orElse(kv.get("joinfield")),
                properties = kv
              )
            }.filter(_.entity.nonEmpty)
            _merge_named(fromTables ++ fromSections)(_.name) { (lhs, rhs) =>
              AggregateMemberDefinition(
                name = lhs.name,
                entity = if (rhs.entity.nonEmpty) rhs.entity else lhs.entity,
                kind = if (rhs.kind.nonEmpty) rhs.kind else lhs.kind,
                boundary = if (rhs.boundary.nonEmpty) rhs.boundary else lhs.boundary,
                join = rhs.join.orElse(lhs.join),
                multiplicity = rhs.multiplicity.orElse(lhs.multiplicity),
                joinField = rhs.joinField.orElse(lhs.joinField),
                properties = lhs.properties ++ rhs.properties
              )
            }
          }

          private def _aggregate_command_definition(
            p: Section
          ): AggregateCommandDefinition = {
            val kv = _merged_key_values(p)
            val props = kv.toMap
            val input = kv.collect {
              case (k, v) if k == "input" || k.startsWith("input.") => k -> v
            }.toMap
            val validations = kv.collect {
              case (k, v) if k == "validate" || k == "validation" || k == "guard" => v
            }
            val events = kv.collect {
              case (k, v) if k == "event" || k == "emit" || k == "events" =>
                _split_list(v)
            }.flatten
            val newstate = kv.collectFirst {
              case (k, v) if k == "newstate" || k == "new_state" || k == "state" => v
            }
            val implementation = kv.collectFirst {
              case (k, v) if k == "implementation" || k == "impl" => v
            }
            AggregateCommandDefinition(
              name = p.nameForModel,
              input = input,
              validations = validations,
              events = events,
              newState = newstate,
              implementation = implementation,
              properties = props
            )
          }

          private def _aggregate_create_definition(
            p: Section
          ): AggregateCreateDefinition = {
            val kv = _merged_key_values(p)
            val props = kv.toMap
            val input = kv.collect {
              case (k, v) if k == "input" || k.startsWith("input.") => k -> v
            }.toMap
            val validations = kv.collect {
              case (k, v) if k == "validate" || k == "validation" || k == "guard" => v
            }
            val events = kv.collect {
              case (k, v) if k == "event" || k == "emit" || k == "events" =>
                _split_list(v)
            }.flatten
            val initialstate = kv.collectFirst {
              case (k, v) if k == "initialstate" || k == "initial_state" || k == "state" => v
            }
            val implementation = kv.collectFirst {
              case (k, v) if k == "implementation" || k == "impl" => v
            }
            AggregateCreateDefinition(
              name = p.nameForModel,
              input = input,
              validations = validations,
              events = events,
              initialState = initialstate,
              implementation = implementation.orElse(Some("pattern:create")),
              properties = props
            )
          }

          private def _aggregate_state_definitions(
            p: Section
          ): Vector[AggregateStateDefinition] = {
            val fromTables = p.tableList.toVector.flatMap(_attribute_rows_for_view_or_aggregate_state).map {
              case (name, tpe, multi, props) =>
                AggregateStateDefinition(name, tpe, multi, props)
            }
            val fromText = CmlSectionFormat.fieldDefinitions(_section_body_text(p)).map {
              case (name, tpe, multi) => AggregateStateDefinition(name, Some(tpe), Some(multi), Map.empty)
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kv = _merged_key_values(s).toMap
              AggregateStateDefinition(
                name = s.nameForModel,
                datatype = kv.get("type"),
                multiplicity = kv.get("multiplicity"),
                properties = kv
              )
            }
            _merge_named(fromTables ++ fromText ++ fromSections)(_.name) { (lhs, rhs) =>
              AggregateStateDefinition(
                name = lhs.name,
                datatype = rhs.datatype.orElse(lhs.datatype),
                multiplicity = rhs.multiplicity.orElse(lhs.multiplicity),
                properties = lhs.properties ++ rhs.properties
              )
            }
          }

          private def _aggregate_invariant_definition(
            p: Section
          ): AggregateInvariantDefinition = {
            val kv = _merged_key_values(p).toMap
            AggregateInvariantDefinition(
              name = p.nameForModel,
              expression = kv.get("expression").orElse(kv.get("expr")).orElse(kv.get("guard")),
              properties = kv
            )
          }

          private def _view_definition(
            p: Section
          ): ViewDefinition = {
            if (_contains_view_mutation_marker(p))
              RAISE.syntaxErrorFault(s"View '${p.nameForModel}' cannot mutate command-side state.")
            val attributesections = p.sections.filter(x => x.keyForModel == "attribute" || x.keyForModel == "attributes")
            val querysections = p.sections.filter(x => x.keyForModel == "query" || x.keyForModel == "queries")
            val attrs = attributesections.toVector.flatMap(_view_attribute_definitions)
            val queries = querysections.toVector.flatMap(_.sections).map(_view_query_definition)
            val rootkv = _merged_key_values(p).toMap
            val sourceevents = rootkv.get("events").toVector.flatMap(_split_list) ++ rootkv.get("event").toVector.flatMap(_split_list)
            val rebuildable = rootkv.get("rebuildable").map(_.equalsIgnoreCase("true"))
            val viewnames = rootkv.get("views").toVector.flatMap(_split_list) ++ rootkv.get("view").toVector.flatMap(_split_list) ++ rootkv.get("viewname").toVector.flatMap(_split_list) ++ rootkv.get("view_name").toVector.flatMap(_split_list)
            ViewDefinition(
              attributes = attrs,
              queries = queries,
              sourceEvents = sourceevents.distinct,
              rebuildable = rebuildable,
              viewNames = viewnames.map(_.trim).filterNot(_.isEmpty).distinct
            )
          }

          private def _contains_view_mutation_marker(
            p: Section
          ): Boolean = {
            def _all(s: Section): Vector[(String, String)] =
              _merged_key_values(s) ++ s.sections.toVector.flatMap(_all)
            val keys = _all(p).map(_._1).toSet
            keys.exists(k => k == "mutates" || k == "mutation" || k == "write" || k == "action")
          }

          private def _view_attribute_definitions(
            p: Section
          ): Vector[ViewAttributeDefinition] = {
            val fromTables = p.tableList.toVector.flatMap(_attribute_rows_for_view_or_aggregate_state).map {
              case (name, tpe, multi, props) =>
                ViewAttributeDefinition(name, tpe, multi, props)
            }
            val fromText = CmlSectionFormat.fieldDefinitions(_section_body_text(p)).map {
              case (name, tpe, multi) => ViewAttributeDefinition(name, Some(tpe), Some(multi), Map.empty)
            }
            val fromSections = p.sections.toVector.filter(_.nameForModel.nonEmpty).map { s =>
              val kv = _merged_key_values(s).toMap
              ViewAttributeDefinition(
                name = s.nameForModel,
                datatype = kv.get("type"),
                multiplicity = kv.get("multiplicity"),
                properties = kv
              )
            }
            _merge_named(fromTables ++ fromText ++ fromSections)(_.name) { (lhs, rhs) =>
              ViewAttributeDefinition(
                name = lhs.name,
                datatype = rhs.datatype.orElse(lhs.datatype),
                multiplicity = rhs.multiplicity.orElse(lhs.multiplicity),
                properties = lhs.properties ++ rhs.properties
              )
            }
          }

          private def _view_query_definition(
            p: Section
          ): ViewQueryDefinition = {
            val kv = _merged_key_values(p).toMap
            if (kv.contains("mutates") || kv.contains("mutation") || kv.contains("write") || kv.contains("action"))
              RAISE.syntaxErrorFault(s"View Query '${p.nameForModel}' cannot mutate command-side state.")
            ViewQueryDefinition(
              name = p.nameForModel,
              expression = kv.get("expression").orElse(kv.get("expr")),
              properties = kv
            )
          }

          private def _section_body_text(p: Section): String =
            p.getStringIfOnlyText.map(_.trim).filterNot(_.isEmpty).getOrElse {
              val lines = p.toText.linesIterator.toVector
              val body = lines.dropWhile(x => x.trim.startsWith("#")).takeWhile(x => !x.trim.startsWith("#")).mkString("\n").trim
              if (body.nonEmpty) body else p.toText.trim
            }

          private def _merged_key_values(p: Section): Vector[(String, String)] = {
            val fromtext = _key_values(_section_body_text(p))
            val fromdl = p.dls.toVector.flatMap(x => _key_values(x.toText))
            val fromsections = p.sections.toVector.flatMap { s =>
              val fromsectiontext = _key_values(_section_body_text(s))
              val fromsectiondl = s.dls.toVector.flatMap(x => _key_values(x.toText))
              val direct = fromsectiontext ++ fromsectiondl
              if (direct.nonEmpty)
                direct
              else {
                val key = s.keyForModel.toLowerCase
                val body = _section_body_text(s).linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
                if (key.isEmpty || body.isEmpty) Vector.empty else Vector(key -> body)
              }
            }
            fromtext ++ fromdl ++ fromsections
          }

          private def _attribute_rows_for_view_or_aggregate_state(
            table: Table
          ): Vector[(String, Option[String], Option[String], Map[String, String])] = {
            val records = SimpleModelerUtils.toRecords(table).toVector
            records.flatMap { r =>
              val name = r.getStringCaseInsensitive(nameName)
              name.filterNot(_.trim.isEmpty).map { n =>
                val dtype = r.getStringCaseInsensitive(typeName).map(_.trim).filterNot(_.isEmpty)
                val mult = r.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty)
                val props = r.fields.map(f => f.name.toLowerCase -> f.asString).toMap
                (n, dtype, mult, props)
              }
            }
          }

          private def _aggregate_member_rows(
            table: Table
          ): Vector[(String, String, String, Option[String], Option[String], Option[String], Map[String, String])] = {
            val records = SimpleModelerUtils.toRecords(table).toVector
            records.flatMap { r =>
              val name = r.getStringCaseInsensitive(nameName).map(_.trim).filterNot(_.isEmpty)
              val entity = r.getStringCaseInsensitive(objectRefName).map(_.trim).filterNot(_.isEmpty)
              (name, entity) match {
                case (Some(n), Some(e)) =>
                  val kind = r.getStringCaseInsensitive(Vector("kind")).map(_.trim).filterNot(_.isEmpty).getOrElse("composition")
                  val join = r.getStringCaseInsensitive(Vector("join", "join_strategy", "joinstrategy", "join_kind", "joinkind")).map(_.trim).filterNot(_.isEmpty)
                  val mult = r.getStringCaseInsensitive(multiplicityName).map(_.trim).filterNot(_.isEmpty)
                  val joinfield = r.getStringCaseInsensitive(Vector("join_field", "joinfield")).map(_.trim).filterNot(_.isEmpty)
                  val props = r.fields.map(f => f.name.toLowerCase -> f.asString).toMap
                  Some((n, e, kind, join, mult, joinfield, props))
                case _ =>
                  None
              }
            }
          }

          private def _split_list(p: String): Vector[String] =
            p.split(",").toVector.map(_.trim).filterNot(_.isEmpty)

          private def _merge_named[A](
            ps: Vector[A]
          )(name: A => String)(merge: (A, A) => A): Vector[A] = {
            case class Z(xs: Vector[(String, A)]) {
              def +(rhs: A): Z = {
                val key = name(rhs).trim.toLowerCase
                xs.indexWhere(_._1 == key) match {
                  case -1 => copy(xs = xs :+ (key -> rhs))
                  case i => copy(xs = xs.updated(i, key -> merge(xs(i)._2, rhs)))
                }
              }
              def result: Vector[A] = xs.map(_._2)
            }
            ps.foldLeft(Z(Vector.empty))(_ + _).result
          }

          private def _normalize_event_category(p: String, eventname: String): String =
            p.trim.toLowerCase match {
              case "actionevent" | "action" => "ActionEvent"
              case "nonactionevent" | "non-action" | "nonaction" => "NonActionEvent"
              case s =>
                RAISE.syntaxErrorFault(s"Event '$eventname' has invalid category: '$s'. Use ActionEvent or NonActionEvent.")
            }

          private def _selector_pair(p: String, eventname: String): (String, String) = {
            val i = p.indexOf("=")
            if (i <= 0)
              RAISE.syntaxErrorFault(s"Event '$eventname' selector requires key=value format: '$p'.")
            else {
              val k = p.substring(0, i).trim
              val v = p.substring(i + 1).trim
              if (k.isEmpty || v.isEmpty)
                RAISE.syntaxErrorFault(s"Event '$eventname' selector requires key=value format: '$p'.")
              else
                k -> v
            }
          }

          private def _to_int_or_raise(p: String, eventname: String): Int =
            scala.util.Try(p.trim.toInt).getOrElse(
              RAISE.syntaxErrorFault(s"Event '$eventname' has invalid priority: '$p'.")
            )

          private def _statemachine(p: Section): Option[StateMachineClass] = {
            val f = KaleidoxStateMachineLogic.Factory
            _parse_statemachine_cml_for_resource(f, p).orElse(
              StateMachineClass.parseBody(f, p.nameForModel, p.toText).toOption
            )
          }

          private def _parse_statemachine_cml_for_resource(
            factory: StateMachineLogic.Factory,
            p: Section
          ): Option[StateMachineClass] = {
            val statesection = p.sections.find(_.keyForModel.equalsIgnoreCase("state"))
            statesection.flatMap { ss =>
              val machinename = p.nameForModel
              val events = _event_names(p)
              val states = ss.sections.zipWithIndex.map {
                case (s, i) => _state_from_section(machinename, s, i)
              }
              if (states.isEmpty)
                None
              else {
                _validate_state_transitions(machinename, states, events)
                val rule = StateMachineRule(
                  name = Some(p.nameForModel),
                  kind = StateMachineKind.Resource,
                  states = states
                )
                Some(StateMachineClass(p.nameForModel, rule, factory.create(rule)))
              }
            }
          }

          private def _event_names(p: Section): Set[String] =
            p.sections.find(_.keyForModel.equalsIgnoreCase("event")).toVector.flatMap(_.sections.map(_.nameForModel.trim)).filterNot(_.isEmpty).toSet

          private def _state_from_section(
            machinename: String,
            p: Section,
            index: Int
          ): StateClass = {
            val entry = _activity_from_actions(_collect_action_lines(p, "entry"))
            val exit = _activity_from_actions(_collect_action_lines(p, "exit"))
            val ts = p.sections.filter(_.keyForModel.equalsIgnoreCase("transition")).map(_transition_from_section(machinename, p.nameForModel, _))
            StateClass(
              name = p.nameForModel,
              value = _state_value(p, index),
              stateMachinePath = None,
              transitions = Transitions.call(ts)
            ).withEntryActivity(entry).withExitActivity(exit)
          }

          private def _collect_action_lines(p: Section, key: String): Vector[String] = {
            p.sections.filter(_.keyForModel.equalsIgnoreCase(key)).flatMap { x =>
              _key_values(x.toText).collect {
                case (k, v) if k == "action" => v
              }
            }.toVector
          }

          private def _transition_from_section(
            machinename: String,
            statename: String,
            p: Section
          ): Transition = {
            val kv = _key_values(p.toText)
            val to = _required_transition_key(kv, "to", machinename, statename)
            val on = _required_transition_key(kv, "on", machinename, statename)
            val guard = kv.collectFirst {
              case (k, v) if k == "guard" => v
            }
            val actions = kv.collect {
              case (k, v) if k == "action" => v
            }
            Transition(
              guard = _transition_guard(on, guard),
              to = _transition_to(to),
              effect = _activity_from_actions(actions)
            )
          }

          private def _state_value(p: Section, index: Int): Int =
            _key_values(p.toText).collectFirst {
              case (k, v) if k == "value" => v.trim.stripPrefix("\"").stripSuffix("\"").toInt
            }.getOrElse(index + 1)

          private def _required_transition_key(
            kv: Vector[(String, String)],
            key: String,
            machinename: String,
            statename: String
          ): String =
            kv.collectFirst {
              case (k, v) if k == key => v
            }.getOrElse {
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition requires '$key'.")
            }

          private def _validate_state_transitions(
            machinename: String,
            states: Seq[StateClass],
            events: Set[String]
          ): Unit = {
            val statenames = states.map(_.name).toSet
            states.foreach { s =>
              s.transitions.call.foreach { t =>
                _validate_transition_target(machinename, s.name, t, statenames)
                _validate_transition_event(machinename, s.name, t, events)
              }
            }
          }

          private def _validate_transition_target(
            machinename: String,
            statename: String,
            t: Transition,
            statenames: Set[String]
          ): Unit =
            t.to match {
              case NameTransitionTo(name) =>
                if (!statenames.contains(name))
                  RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition target '$name' is not defined.")
              case _ =>
            }

          // Policy: when Event section is present, transitions must reference declared events.
          // When Event section is omitted, events are accepted as implicit declarations.
          private def _validate_transition_event(
            machinename: String,
            statename: String,
            t: Transition,
            events: Set[String]
          ): Unit = {
            val eventname = _event_name(t.guard).getOrElse {
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' transition requires 'on'.")
            }
            if (events.nonEmpty && !events.contains(eventname))
              RAISE.syntaxErrorFault(s"StateMachine '$machinename' state '$statename' references undeclared event '$eventname'.")
          }

          private def _event_name(guard: SmGuard): Option[String] =
            guard match {
              case EventNameGuard(name) => Some(name)
              case AndGuard(exprs) => exprs.toStream.flatMap(_event_name).headOption
              case OrGuard(exprs) => exprs.toStream.flatMap(_event_name).headOption
              case _ => None
            }

          private def _transition_guard(on: String, guard: Option[String]): SmGuard =
            guard.filterNot(_.isEmpty) match {
              case Some(g) => AndGuard(Vector(EventNameGuard(on), CmlExpressionGuard(g)))
              case None => EventNameGuard(on)
            }

          private def _transition_to(p: String): TransitionTo =
            if (p.equalsIgnoreCase(PROP_STATE_FINAL))
              FinalTransitionTo
            else if (p.equalsIgnoreCase(PROP_STATE_HISTORY))
              HistoryTransitionTo()
            else
              NameTransitionTo(p)

          private def _activity_from_actions(actions: Seq[String]): Activity =
            actions.toList match {
              case Nil => Activity.Empty
              case x :: Nil => Activity.Opaque(x)
              case xs => Activity.Opaque(xs.mkString("\n"))
            }

          private def _key_values(p: String): Vector[(String, String)] = {
            CmlSectionFormat.keyValues(p)
          }
        }

        p.elements.foldLeft(Z())(_+_).r
      }

      private def _is_property_table(p: Table) = p.getCaptionName.
        map(_ == "特性一覧").getOrElse(false)

      // Generic meta-grammar normalization:
      // YAML/HOCON/Table/List text -> IR(Record) sequence.
      private def _records_from_meta_text(p: String): Vector[Record] =
        CmlSectionFormat.recordMaps(p).map { x =>
          Record.create(x.toVector.map { case (k, v) => k -> v })
        }

      private def _delegate_records_from_meta_text(p: String): Vector[Record] = {
        val structured = _records_from_meta_text(p)
        if (structured.nonEmpty)
          structured
        else {
          val kv = CmlSectionFormat.keyValues(p)
          val fromKv = _delegate_records_from_key_values(kv)
          if (fromKv.nonEmpty)
            fromKv
          else
            CmlSectionFormat.valueLines(p).flatMap(_delegate_record_from_value_line)
        }
      }

      private def _delegate_records_from_key_values(p: Vector[(String, String)]): Vector[Record] = {
        case class Z(current: Vector[(String, String)] = Vector.empty, records: Vector[Record] = Vector.empty) {
          private def _flush: Z =
            if (current.isEmpty)
              this
            else
              copy(current = Vector.empty, records = records :+ Record.create(current))

          def +(rhs: (String, String)): Z = {
            val (k, v) = rhs
            val key = Option(k).map(_.trim.toLowerCase).getOrElse("")
            if (key == "name" || key == "delegate")
              _flush.copy(current = Vector(key -> v))
            else
              copy(current = current :+ (key -> v))
          }

          def result: Vector[Record] = _flush.records
        }
        p.foldLeft(Z())(_ + _).result.filter(r => _delegate_name_from_record(r).isDefined)
      }

      private def _delegate_record_from_value_line(p: String): Option[Record] =
        _normalize_delegate_name_line(p).map(n => Record.create(Vector("name" -> n)))

      private def _normalize_delegate_name_line(p: String): Option[String] = {
        val raw = Option(p).map(_.trim).getOrElse("")
        if (raw.isEmpty)
          None
        else {
          val s = if (raw.startsWith("- ")) raw.substring(2).trim else raw
          val hasMetaMarker = s.exists(ch => ch == ':' || ch == '=' || ch == '{' || ch == '}' || ch == '#')
          val validName = s.matches("[A-Za-z_][A-Za-z0-9_\\.\\-]*")
          if (hasMetaMarker || !validName) None else Some(s)
        }
      }

      private def _delegate_name_from_record(p: Record): Option[String] =
        p.getStringCaseInsensitive(SchemaClass.nameName).orElse(p.getStringCaseInsensitive(Vector("delegate"))).map(_.trim).filterNot(_.isEmpty)

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
        delegates: Seq[DelegateDefinition],
        props: Seq[Table],
        attrs: Seq[Record],
        assocs: Seq[Table],
        aggres: Seq[Table],
        compos: Seq[Table],
        powers: Seq[Table],
        sms: Seq[StateMachineClass],
        events: Seq[EventDefinition],
        aggregate: Option[AggregateDefinition],
        view: Option[ViewDefinition]
      ): Option[SchemaClass] = {
        val name = if (autoCapitalize) UString.capitalize(pname) else pname
        val fs: Option[Features] = _to_features_option(features).orElse(Some(Features.empty)).
          map(x => delegates.foldLeft(x)((z, d) => z.addDelegate(d.normalize))).
          flatMap(_.toOption)
        val xs: Seq[Slot] =
          _to_props(props) ++ _to_attrs(attrs) ++
        _to_assocs(assocs) ++ _to_aggres(aggres) ++ _to_comps(compos) ++
        _to_powers(powers) ++ _to_sms(sms)
        if (fs.isEmpty && xs.isEmpty)
          None
        else
          Some(SchemaClass(name, fs getOrElse Features.empty, xs.toVector, events.toVector, aggregate, view))
      }

      private def _to_features_option(ps: Seq[Table]): Option[Features] =
        ps.toVector.flatMap(_to_features).concatenate.toOption

      private def _to_props(ps: Seq[Table]): Seq[Slot] = {
        val rs = ps.toVector.foldMap(SimpleModelerUtils.toRecords)
        rs.flatMap(_slot)
      }

      private def _to_attrs(ps: Seq[Record]): Seq[Slot] = { // Attribute | Id
        ps.map(_attribute_or_id)
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
        ),
        _db_column_name(p),
        _db_column_type(p),
        _external_name(p)
      )

      private def _attribute(p: Record) = Attribute(
        _name(p),
        _label(p),
        ValueDomain(
          _datatype(p),
          _multiplicity(p),
          _constraints(p)
        ),
        p.getStringCaseInsensitive(Vector("description")).map(_.trim).filterNot(_.isEmpty),
        p.getStringCaseInsensitive(typeName).map(_.trim).filterNot(_.isEmpty),
        _db_column_name(p),
        _db_column_type(p),
        _external_name(p),
        _derived(p),
        _web(p)
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

      private def _name(p: Record): String = p.getStringCaseInsensitive(nameName).
        getOrElse {
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

      private def _constraints(p: Record): List[Constraint] = {
        val min = _int_value_flexible(p, minLengthName).map(org.goldenport.record.v2.CMinLength.apply).toList
        val max = _int_value_flexible(p, maxLengthName).map(org.goldenport.record.v2.CMaxLength.apply).toList
        val pattern = _string_value_flexible(p, patternName).map(x => org.goldenport.record.v2.CRegex(x.r)).toList
        val format = _string_value_flexible(p, formatName).toList.flatMap(_format_constraint)
        min ++ max ++ pattern ++ format
      }

      private def _db_column_name(p: Record): Option[String] =
        _string_value_flexible(p, dbColumnNameName).orElse(
          p.getStringCaseInsensitive(dbColumnNameName).map(_.trim).filterNot(_.isEmpty)
        )

      private def _db_column_type(p: Record): Option[String] =
        _string_value_flexible(p, dbColumnTypeName).orElse(
          p.getStringCaseInsensitive(dbColumnTypeName).map(_.trim).filterNot(_.isEmpty)
        )

      private def _external_name(p: Record): Option[String] =
        _string_value_flexible(p, externalNameName).orElse(
          p.getStringCaseInsensitive(externalNameName).map(_.trim).filterNot(_.isEmpty)
        )

      private def _derived(p: Record): Option[String] =
        _string_value_flexible(p, derivedName).orElse(
          p.getStringCaseInsensitive(derivedName).map(_.trim).filterNot(_.isEmpty)
        )

      private def _web(p: Record): Attribute.Web =
        Attribute.Web(
          label = _string_value_flexible(p, Seq("web-label", "webLabel")),
          controlType = _string_value_flexible(p, Seq("web-control-type", "web-controlType", "webControlType", "web-control", "webControl", "web-widget", "webWidget")),
          placeholder = _string_value_flexible(p, Seq("web-placeholder", "webPlaceholder")),
          help = _string_value_flexible(p, Seq("web-help", "webHelp")),
          required = _boolean_value_flexible(p, Seq("web-required", "webRequired")),
          hidden = _boolean_value_flexible(p, Seq("web-hidden", "webHidden")),
          readonly = _boolean_value_flexible(p, Seq("web-readonly", "webReadonly", "web-read-only", "webReadOnly")),
          minLength = _string_value_flexible(p, Seq("web-min-length", "webMinLength")),
          maxLength = _string_value_flexible(p, Seq("web-max-length", "webMaxLength")),
          min = _string_value_flexible(p, Seq("web-min", "webMin")),
          max = _string_value_flexible(p, Seq("web-max", "webMax")),
          step = _string_value_flexible(p, Seq("web-step", "webStep")),
          pattern = _string_value_flexible(p, Seq("web-pattern", "webPattern", "web-regex", "webRegex"))
        )

      private def _string_value_flexible(p: Record, keys: Seq[String]): Option[String] = {
        val normalized = keys.map(_normalize_key).toSet
        p.fields.collectFirst {
          case f if normalized.contains(_normalize_key(f.name)) => _field_string(f)
        }.map(_.trim).filterNot(_.isEmpty)
      }

      private def _field_string(p: Field): String =
        p.getValue.collect {
          case InlineMacro("pass", contents, _, _) => contents
        }.getOrElse(_normalize_inline_macro_string(p.asString))

      private val _inline_macro_string =
        """<inlinemacro\s+name="([^"]+)">([\s\S]*?)</inlinemacro>""".r

      private def _normalize_inline_macro_string(p: String): String =
        p match {
          case _inline_macro_string("pass", contents) => contents
          case _inline_macro_string(name, contents) => s"$name:$contents"
          case _ => p
        }

      private def _normalize_key(p: String): String =
        p.toLowerCase.replaceAll("[\\s_\\-　]+", "")

      private def _has_web_field(p: Record): Boolean =
        p.fields.exists(field => _normalize_key(field.name).startsWith("web"))

      private def _int_value_flexible(p: Record, keys: Seq[String]): Option[Int] =
        _string_value_flexible(p, keys).flatMap(x => Try(x.toInt).toOption)

      private def _boolean_value_flexible(p: Record, keys: Seq[String]): Option[Boolean] =
        _string_value_flexible(p, keys).map(_.trim.toLowerCase(java.util.Locale.ROOT)).collect {
          case "true" | "yes" | "on" | "1" => true
          case "false" | "no" | "off" | "0" => false
        }

      private def _format_constraint(p: String): Option[Constraint] = p.trim.toLowerCase match {
        case "" => None
        case "email" | "uuid" | "uri" | "url" |
             "date" | "time" | "date-time" | "datetime" | "date_time" |
             "phone" | "tel" | "e164" =>
          Some(org.goldenport.record.v2.CFormat(p.trim))
        case _ => None
      }
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
      rs.foldLeft(Z())(_+_).r
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
    def dbColumnName: Option[String] = None
    def dbColumnType: Option[String] = None
    def externalName: Option[String] = None
    def derived: Option[String] = None
    def toColumn: Column
    def unmarshall(p: Any): Consequence[Any]
  }

  case class Id(
    name: String,
    label: Option[I18NString],
    domain: ValueDomain,
    override val dbColumnName: Option[String] = None,
    override val dbColumnType: Option[String] = None,
    override val externalName: Option[String] = None,
    override val derived: Option[String] = None
  ) extends Slot {
    def toColumn = Column(
      name,
      domain.datatype,
      domain.multiplicity,
      constraints = domain.constraints,
      i18nLabel = label,
      aliases = externalName.toList,
      sql = _sql_column(dbColumnName, dbColumnType)
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
    domain: ValueDomain,
    descriptionText: Option[String] = None,
    rawTypeName: Option[String] = None,
    override val dbColumnName: Option[String] = None,
    override val dbColumnType: Option[String] = None,
    override val externalName: Option[String] = None,
    override val derived: Option[String] = None,
    web: Attribute.Web = Attribute.Web.empty
  ) extends Slot {
    def isRequired = domain.isRequired

    def verify(p: Any): Consequence[Any] = domain.verify(p)
    def verifyField(p: Field): Consequence[Field] = domain.verifyField(p)
    def verifyField(key: String, p: Any): Consequence[Field] = domain.verifyField(key, p)

    def toColumn = Column(
      name,
      domain.datatype,
      domain.multiplicity,
      constraints = domain.constraints,
      i18nLabel = label,
      aliases = externalName.toList,
      sql = _sql_column(dbColumnName, dbColumnType)
    )

    def unmarshall(p: Any): Consequence[Any] = verify(p)
  }

  object Attribute {
    case class Web(
      label: Option[String] = None,
      controlType: Option[String] = None,
      placeholder: Option[String] = None,
      help: Option[String] = None,
      required: Option[Boolean] = None,
      hidden: Option[Boolean] = None,
      readonly: Option[Boolean] = None,
      minLength: Option[String] = None,
      maxLength: Option[String] = None,
      min: Option[String] = None,
      max: Option[String] = None,
      step: Option[String] = None,
      pattern: Option[String] = None
    )
    object Web {
      val empty: Web = Web()
    }
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

  private def _sql_column(
    dbcolumnname: Option[String],
    dbcolumntype: Option[String]
  ): SqlColumn = {
    val n = dbcolumnname.map(_.trim).filterNot(_.isEmpty)
    val t = dbcolumntype.flatMap(_to_sql_datatype)
    if (n.isEmpty && t.isEmpty)
      NullSqlColumn
    else
      SqlColumn(
        name = n.orNull,
        datatype = t
      )
  }

  private def _to_sql_datatype(p: String): Option[sql.SqlDatatype] = {
    val trimmed = p.trim
    if (trimmed.isEmpty)
      None
    else {
      val upper = trimmed.toUpperCase
      val onearg = """^([A-Z_]+)\((\d+)\)$""".r
      upper match {
        case onearg("VARCHAR", length) => Some(sql.VARCHAR(length.toInt))
        case onearg("NVARCHAR", length) => Some(sql.NVARCHAR(length.toInt))
        case onearg("CHAR", length) => Some(sql.CHAR(length.toInt))
        case "INT" => Some(sql.INT())
        case "INTEGER" => Some(sql.INTEGER())
        case "BIGINT" => Some(sql.BIGINT())
        case "REAL" => Some(sql.REAL())
        case "FLOAT" => Some(sql.FLOAT())
        case "DOUBLE" => Some(sql.DOUBLE())
        case "BOOLEAN" => Some(sql.BOOLEAN())
        case "TEXT" => Some(sql.CLOB())
        case "DATE" => Some(sql.DATE())
        case "TIME" => Some(sql.TIME())
        case "TIMESTAMP" => Some(sql.TIMESTAMP())
        case "NUMERIC" => Some(sql.NUMERIC())
        case "DECIMAL" => Some(sql.DECIMAL())
        case "BLOB" => Some(sql.BLOB())
        case "CLOB" => Some(sql.CLOB())
        case _ => None
      }
    }
  }
}
