package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import com.typesafe.config.{Config => Hocon, ConfigFactory, ConfigObject}
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.goldenport.RAISE
import org.goldenport.context.Showable
import org.goldenport.hocon.RichConfig.Implicits._
import org.goldenport.parser._
import org.goldenport.record.v2.{Schema, Column, SqlSchema}
import org.goldenport.record.v3.IRecord
import org.goldenport.record.v3.Record
import org.goldenport.record.store.Query
import org.goldenport.sexpr.SSchema
import org.goldenport.sexpr.eval.LispContext
import org.goldenport.sexpr.eval.entity.{EntityClass => IEntityClass}
import org.goldenport.sexpr.eval.entity.Entity
import org.goldenport.sexpr.eval.entity.EntityId
import org.goldenport.context.Consequence
import org.goldenport.collection.VectorMap
import org.goldenport.sm.StateMachineClass
import org.goldenport.kaleidox._
import org.goldenport.kaleidox.lisp.Context
import org.goldenport.kaleidox.model.SchemaModel.SchemaClass
import org.goldenport.kaleidox.model.entity.KaleidoxEntity
import org.goldenport.kaleidox.model.entity.KaleidoxEntityFactory

/*
 * @since   Jun. 25, 2021
 *  version Jun. 26, 2021
 *  version Sep. 24, 2021
 *  version Oct. 31, 2021
 *  version Nov. 28, 2021
 *  version Dec. 31, 2021
 *  version Apr. 23, 2023
 *  version Aug. 21, 2023
 *  version Sep. 30, 2023
 *  version Oct. 22, 2023
 *  version Jul. 12, 2024
 * @version Sep.  6, 2024
 * @author  ASAMI, Tomoharu
 */
case class EntityModel(
  factory: KaleidoxEntityFactory,
  classes: VectorMap[String, EntityModel.EntityClass],
  description: Description = Description.name("entity")
) extends Model.ISubModel with ISchemaModel {
  import EntityModel._

  protected def display_String: String = classes.values.map(_.name).mkString(",")

  protected def print_String: String = classes.values.map(_.name).mkString(",")

  protected def show_String: String = classes.values.map(_.name).mkString(",")

  def isEmpty: Boolean = classes.isEmpty
  def toOption: Option[EntityModel] = if (isEmpty) None else Some(this)

  def get(name: String): Option[EntityClass] = classes.get(name)
  def getSchema(name: String): Option[Schema] = get(name).map(_.schema)
  def getSchemaClass(name: String): Option[ISchemaClass] = get(name).map(_.schemaClass)

  def +(rhs: EntityModel): EntityModel = copy(classes = classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.schema.${x.name}"
      Record.data(path -> SSchema(x.schema))
    }
    p.updateBindings(a)
  }

  def resolve(): EntityModel = {
    val r = Resolver.resolve(classes)
    copy(classes = r)
  }
}

object EntityModel {
//  val empty = EntityModel(VectorMap.empty[String, EntityClass])
  def empty(config: Config): EntityModel = empty(config.entityFactory)

  def empty(f: KaleidoxEntityFactory): EntityModel = EntityModel(f, VectorMap.empty[String, EntityModel.EntityClass])

  implicit object EntityModelSemigroup extends Semigroup[EntityModel] {
    def append(lhs: EntityModel, rhs: => EntityModel) = lhs + rhs
  }

  case class EntityClass(
    factory: KaleidoxEntityFactory,
    schemaClass: SchemaClass,
    parents: List[EntityClass.ParentRef],
    store: IEntityClass.Store = IEntityClass.Store(),
    packageName: String = "domain" // TODO
  ) extends IEntityClass with Showable.Base {
    def name = schemaClass.name
    def schema = schemaClass.schema
    def stateMachines: Vector[StateMachineClass] = schemaClass.stateMachines

    def isResolved: Boolean = parents.forall(_.isResolved)

    protected def print_String: String = schemaClass.print

    protected def display_String: String = schemaClass.display

    protected def show_String: String = schemaClass.show

    private def _id_name = "id" // TODO

    def create(p: IRecord)(implicit ctx: LispContext): Consequence[Entity] = {
//      implicit val sc = ctx.statemachineContext
      implicit val c = ctx.asInstanceOf[Context]
      for {
        attrs <- schemaClass.attributeRecordForCreate(p)
      } yield {
        val id = p.getString(_id_name) match {
          case Some(s) => EntityId(name, s)
          case None => EntityId.generate(name)
        }
        val sms = VectorMap(stateMachines.map(factory.spawn(_, id)).
          map(x => Symbol(x.name) -> x))
        KaleidoxEntity.create(this, id, attrs, sms)
      }
    }

    def reconstitute(p: IRecord): Consequence[Entity] = for {
      id <- schemaClass.idForReconstitute(p)
      attrs <- schemaClass.attributeRecordForReconstitute(p)
      sms <- schemaClass.stateMachineRecordForReconstitute(p, id)
    } yield {
      KaleidoxEntity.create(this, id, attrs, sms)
    }

    def unmarshallProperties(p: IRecord): Consequence[IRecord] =
      schemaClass.unmarshallProperties(p)

    def persistentQuery(p: Query): Query = p // TODO
  }
  object EntityClass {
    val PROP_ENTITY_NAME = "name"
    val PROP_ENTITY_TABLE = "table"
    val PROP_ENTITY_SCHEMA = "schema"
    val PROP_ENTITY_EXTENDS = "extends"

    sealed trait ParentRef {
      def isResolved: Boolean
    }
    object ParentRef {
      case class Name(name: String) extends ParentRef {
        def isResolved: Boolean = false
      }
      case class EntityKlass(entityClass: EntityClass) extends ParentRef {
        def isResolved: Boolean = true
      }
    }

    def parse(
      schemaModel: SchemaModel,
      factory: KaleidoxEntityFactory,
      name: String,
      p: String
    ): ParseResult[EntityClass] = {
      val hocon = ConfigFactory.parseString(p)
      parse(schemaModel, factory, name, hocon)
    }

    def parse(
      schemaModel: SchemaModel,
      factory: KaleidoxEntityFactory,
      name: String,
      hocon: Hocon
    ): ParseResult[EntityClass] = {
      for {
        schema <- _schema(schemaModel, hocon, name)
        parents <- _parents(hocon)
        store <- _store(hocon)
      } yield {
        EntityClass(factory, schema, parents, store)
      }
    }

    def parse(
      schemaModel: SchemaModel,
      factory: KaleidoxEntityFactory,
      name: String,
      pschema: SchemaClass,
      hocon: Hocon
    ): ParseResult[EntityClass] = {
      for {
        schema <- _schema(schemaModel, pschema, hocon, name)
        parents <- _parents(hocon)
        store <- _store(hocon)
      } yield {
        EntityClass(factory, schema, parents, store)
      }
    }

    private def _schema(
      smodel: SchemaModel,
      schema: SchemaClass,
      hocon: Hocon,
      name: String
    ): ParseResult[SchemaClass] = {
      hocon.getStringOption(PROP_ENTITY_SCHEMA) match {
        case Some(schemaname) => smodel.getSchemaClass(schemaname) match {
          case Some(s) => ParseResult.success(_add_features(schema.add(s).withName(name), hocon))
          case None => ParseResult.error(s"Schema not found: $schemaname")
        }
        case None => ParseResult.success(schema.withName(name))
      }
    }

    private def _schema(
      smodel: SchemaModel,
      hocon: Hocon,
      name: String
    ): ParseResult[SchemaClass] = {
      hocon.getStringOption(PROP_ENTITY_SCHEMA) match {
        case Some(schemaname) => smodel.getSchemaClass(schemaname) match {
          case Some(s) => ParseResult.success(_add_features(s.withName(name), hocon))
          case None => ParseResult.error(s"Schema not found: $schemaname")
        }
        case None => ParseResult.error(s"Schema not specified")
      }
    }

    private def _add_features(p: SchemaClass, hocon: Hocon): SchemaClass = {
      val a = hocon.getStringOption(PROP_ENTITY_TABLE).
        map(p.withTableName).
        getOrElse(p)
      hocon.getStringOption(PROP_ENTITY_EXTENDS).
        map(a.addParentName).
        getOrElse(a)
    }

    private def _parents(hocon: Hocon): ParseResult[List[ParentRef]] =
      for {
        xs <- hocon.parseStringList(PROP_ENTITY_EXTENDS)
      } yield xs.map(ParentRef.Name.apply)

    private def _store(hocon: Hocon): ParseResult[IEntityClass.Store] =
      ParseResult.success(IEntityClass.Store())

    def create(
      factory: KaleidoxEntityFactory,
      s: SchemaClass,
      hocon: Hocon
    ): EntityClass = {
      val a = for {
        parents <- _parents(hocon)
      } yield EntityClass(factory, _add_features(s, hocon), parents)
      a.take
    }
  }

  def apply(f: KaleidoxEntityFactory, p: EntityClass): EntityModel =
    EntityModel(f, VectorMap(p.name -> p))

  // def create(f: KaleidoxEntityFactory, p: LogicalSection): EntityModel = createOption(f, p) getOrElse EntityModel.empty(f)

  // // def createOption(f: KaleidoxEntityFactory, p: LogicalSection): Option[EntityModel] = {
  // //   SchemaClass.createOption(p).map(_to_model(f, _))
  // //   // TODO association
  // //   // TODO statemachine
  // // }

  // def createOption(f: KaleidoxEntityFactory, p: LogicalSection): Option[EntityModel] = {
  //     val name = p.nameForModel
  //     val s = p.text
  //     val r = EntityModel.parse(f, name, s)
  //     r.toOption // TODO
  // }

  // def create(f: KaleidoxEntityFactory, p: Section): EntityModel = createOption(f, p) getOrElse EntityModel.empty(f)

  // def createOption(f: KaleidoxEntityFactory, p: Section): Option[EntityModel] = {
  //   SchemaClass.createOption(p).map(_to_model(f, _))
  //   // TODO association
  //   // TODO statemachine
  // }

  // private def _to_model(f: KaleidoxEntityFactory, p: SchemaClass) = EntityModel(f, EntityClass(f, p))

  def create(s: Option[SchemaModel], f: KaleidoxEntityFactory, p: Section): EntityModel = Builder(s getOrElse SchemaModel.empty, f).build(p)

  def create(s: Option[SchemaModel], f: KaleidoxEntityFactory, p: LogicalSection): EntityModel = Builder(s getOrElse SchemaModel.empty, f).build(p)

  case class Builder(
    schemaModel: SchemaModel,
    factory: KaleidoxEntityFactory
  ) {
    def build(p: Section): EntityModel = 
      p.sectionsShallow match {
        case Nil => _build_config(p)
        case xs => _build_sections(p, xs)
      }

    private def _build_config(p: Section): EntityModel =
      createClassOption(p).
        map(EntityModel.apply(factory, _)).
        getOrElse(empty(factory))

    private def createClassOption(p: Section): Option[EntityClass] = {
      val name = p.nameForModel
      val s = p.toPlainText
      val r = EntityClass.parse(schemaModel, factory, name, s)
      r.toOption // TODO
    }

    private def _build_sections(p: Section, sections: List[Section]): EntityModel = {
      RAISE.notImplementedYetDefect
    }

    def build(p: LogicalSection): EntityModel = 
      p.sections.toList match {
        case Nil => _build_config(p)
        case xs => _build_sections(p, xs)
      }

    private def _build_config(p: LogicalSection): EntityModel =
      createClassOption(p).
        map(EntityModel.apply(factory, _)).
        getOrElse(empty(factory))

    private def createClassOption(p: LogicalSection): Option[EntityClass] = {
      val name = p.nameForModel
      val s = p.text
      val r = EntityClass.parse(schemaModel, factory, name, s)
      r.toOption // TODO
    }

    private def _build_sections(p: LogicalSection, sections: List[LogicalSection]): EntityModel = {
      val name = p.nameForModel
      case class Z(
        schema: Option[SchemaClass] = SchemaClass.createOption(p),
        props: Option[Hocon] = None
      ) {
        def r = {
          val ec: Option[EntityClass] = (schema, props) match {
            case (Some(s), Some(p)) => EntityClass.parse(schemaModel, factory, name, s, p).toOption
            case (Some(s), None) => Some(EntityClass(factory, s, Nil))
            case (None, Some(p)) => EntityClass.parse(schemaModel, factory, name, p).toOption
            case (None, None) => SchemaClass.createOption(p).map(EntityClass(factory, _, Nil)) // TODO
          }
          ec.
            map(EntityModel(factory, _)).
            getOrElse(ParseResult.error("No entity definition").RAISE)
        }

        def +(rhs: LogicalSection) =
          rhs.nameForModel match {
            case "features" => copy(props = _features(rhs.text))
//            case "schema" => copy(schema = _schema(rhs))
            case _ => this
          }

        private def _features(p: String) = Some(ConfigFactory.parseString(p))

        private def _schema(p: LogicalSection) = SchemaClass.createOption(p)
      }
      sections./:(Z())(_+_).r
    }
  }

  case class Resolver(
    resolved: VectorMap[String, EntityClass] = VectorMap.empty,
    unresolved: Vector[EntityClass] = Vector.empty
  ) {
    // def r: VectorMap[String, EntityClass] =
    //   if (unresolved.isEmpty) {
    //     resolved
    //   } else {
    //     Resolver.resolve(
    //   }

    def +(rhs: EntityClass) = {
      if (rhs.isResolved) {
        copy(resolved = resolved + (rhs.name -> rhs))
      } else {
        _resolve(rhs) match {
          case Right(r) => copy(resolved = resolved + (r.name -> r))
          case Left(l) => copy(unresolved = unresolved :+ l)
        }
      }
    }

    private def _resolve(p: EntityClass): Either[EntityClass, EntityClass] = {
      val a = p.parents.map {
        case m: EntityClass.ParentRef.Name => resolved.get(m.name) match {
          case Some(s) => EntityClass.ParentRef.EntityKlass(s)
          case None => m
        }
        case m: EntityClass.ParentRef.EntityKlass => m
      }
      if (a.forall(_.isResolved))
        Right(p.copy(parents = a))
      else
        Left(p.copy(parents = a))
    }
  }
  object Resolver {
    def resolve(ps: VectorMap[String, EntityClass]): VectorMap[String, EntityClass] = {
      val (resolved, unresolved) = ps.values.partition(_.isResolved)
      resolve(VectorMap(resolved.map(x => x.name -> x)), unresolved.toVector)
    }

    def resolve(
      resolved: VectorMap[String, EntityClass],
      unresolved: Vector[EntityClass]
    ): VectorMap[String, EntityClass] = {
      if (unresolved.isEmpty) {
        resolved
      } else {
        val b = unresolved./:(Resolver(resolved))(_+_)
        if (unresolved.size == b.unresolved.size)
          ParseResult.error("Unresolved parents").RAISE // TODO
        else
          resolve(b.resolved, b.unresolved)
      }
    }
  }
}
