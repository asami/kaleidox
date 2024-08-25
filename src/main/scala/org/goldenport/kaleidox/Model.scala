package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.io.File
import java.net.{URL, URI}
import com.typesafe.config.{Config => Hocon, ConfigFactory}
import org.smartdox.parser.Dox2Parser
import org.smartdox.{Dox, Section}
import org.smartdox.Description
import org.smartdox.Table
import org.smartdox.Doxes
import org.goldenport.RAISE
import org.goldenport.Strings
import org.goldenport.context.Showable
import org.goldenport.values.Designation
import org.goldenport.sexpr.{SExpr, SAtom, SKeyword, SList}
import org.goldenport.sexpr.IModel
import org.goldenport.exception.SyntaxErrorFaultException
import org.goldenport.i18n.I18NElement
import org.goldenport.collection.TreeMap
import org.goldenport.parser._
import org.goldenport.hocon.{RichConfig, HoconUtils}
import org.goldenport.bag.BufferBag
import org.goldenport.io.IoUtils
import org.goldenport.util.StringUtils
import org.goldenport.parser.ParseMessage
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v3.{IRecord, SingleValue, MultipleValue, EmptyValue}
import org.goldenport.record.v3.{Record, Field}
import org.goldenport.record.v3.HoconRecord
import org.goldenport.record.util.{HoconUtils => RHoconUtils}
import org.goldenport.statemachine.StateMachineClass
import org.goldenport.kaleidox.model._
import org.goldenport.kaleidox.model.entity.KaleidoxEntityFactory
import org.goldenport.kaleidox.model.vision.VisionModel
import org.goldenport.kaleidox.model.business.BusinessModel
import org.goldenport.kaleidox.model.requirement.RequirementModel
import org.goldenport.kaleidox.model.analysis.AnalysisModel

/*
 * @since   Sep. 24, 2018
 *  version Oct. 27, 2018
 *  version Feb. 16, 2019
 *  version Mar. 24, 2019
 *  version Apr. 18, 2019
 *  version May. 19, 2019
 *  version Jul. 15, 2019
 *  version Sep.  8, 2019
 *  version Nov. 16, 2019
 *  version Jan. 10, 2021
 *  version Feb. 24, 2021
 *  version Mar. 21, 2021
 *  version Apr. 18, 2021
 *  version May. 22, 2021
 *  version Jun. 27, 2021
 *  version Aug.  8, 2021
 *  version Sep. 17, 2021
 *  version Oct. 23, 2021
 *  version Dec. 31, 2021
 *  version Nov. 28, 2022
 *  version Jan. 23, 2023
 *  version Aug. 21, 2023
 *  version Sep. 18, 2023
 *  version Oct. 22, 2023
 *  version Jan.  3, 2024
 *  version Jul. 12, 2024
 * @version Aug.  5, 2024
 * @author  ASAMI, Tomoharu
 */
case class Model(
  config: Config,
  divisions: Vector[Model.Division],
  // importedModels: Model.ImportedModels,
  libraries: Libraries,
  errors: Vector[ErrorMessage] = Vector.empty,
  warnings: Vector[WarningMessage] = Vector.empty
) extends IModel {
  import Model._

  def entityFactory = config.entityFactory

  private lazy val _ctx = {
    val schemas = Vector(getSchemaModel, getEntityModel, getVoucherModel).flatten
    DataSet.Builder.Context(config, schemas)
  }

  // private lazy val _ctx = (getSchemaModel, getVoucherModel) match {
  //   case (None, None) => DataSet.Builder.Context(config)
  //   case (Some(s), None) => DataSet.Builder.Context(config, s)
  //   case (None, Some(v)) => DataSet.Builder.Context(config, v)
  //   case (Some(s), Some(v)) => DataSet.Builder.Context(config, s, v)
  // }

  private val _models: Vector[ISubModel] = Vector(
    getPreambleModel,
    getServiceModel,
    getDataTypeModel,
    getEntityModel,
    getSchemaModel,
    getValueModel,
    getPowertypeModel,
    getStateMachineModel,
    getDataSet,
    getDataStore,
    getDataBag,
    getXslModel
  ).flatten

  def display = {
    val s = _models.map(_.display).mkString(";")
    s"Model[$s]"
  }

  def print = {
    // divisions.map(_.print).mkString("\n")
    val s = _models.map(_.print).mkString("\n")
    s"Model[$s]"
  }

  def show = {
    val s = _models.map(_.display).mkString(";")
    s"Model[$s]"
  }

  def signature: Signature = NoneSignature

  def getPrologue(config: Config): Option[Script] = divisions.flatMap {
    case m: PrologueDivision => m.getScript(config)
    case m: DataBagDivision => m.getScript(_ctx)
    case _ => None
  }.concatenate.toOption

  def getEpilogue(config: Config): Option[Script] = divisions.flatMap {
    case m: EpilogueDivision => m.getScript(config)
    case _ => None
  }.concatenate.toOption

  def getScript: Option[Script] = divisions.collect {
    case m: Script => m
  }.concatenate.toOption

  // TODO skip libraries already imported.
  def getWholeScript: Option[Script] = getScript |+| libraries.getScript // importedModels.getWholeScript

  def getEnvironmentProperties: Option[IRecord] = {
    val a = divisions.collect {
      case m: EnvironmentDivision => m
    }
    a.headOption.map(x => a.tail./:(x.properties)((z, x) => z.update(x.properties))) // TODO concat
  }

  lazy val getVoucherModel: Option[VoucherModel] = {
    val a = divisions.collect {
      case m: VoucherDivision => m.makeModel
    }
    a.headOption // TODO concat
  }

  lazy val getSchemaModel: Option[SchemaModel] =
    divisions.collect {
      case m: SchemaDivision => m.makeModel
    }.concatenate.toOption

  lazy val takeSchemaModel: SchemaModel = getSchemaModel.orZero

  lazy val getValueModel: Option[ValueModel] =
    divisions.collect {
      case m: ValueDivision => m.makeModel
    }.concatenate.toOption

  lazy val getPowertypeModel: Option[PowertypeModel] =
    divisions.collect {
      case m: PowertypeDivision => m.makeModel
    }.concatenate.toOption

  lazy val takePowertypeModel: PowertypeModel = getPowertypeModel.orZero

  lazy val getSlipModel: Option[SlipModel] =
    divisions.collect {
      case m: SlipDivision => m.makeModel
    }.concatenate.toOption

  lazy val getEntityModel: Option[EntityModel] = {
    val a = divisions.collect {
      case m: EntityDivision => m.makeModel(getSchemaModel, entityFactory)
    }
    a.headOption.map(h => a.tail.foldLeft(h)((z, x) => z + x))
  }

  lazy val takeEntityModel: EntityModel = getEntityModel getOrElse EntityModel.empty(entityFactory)

  lazy val getPreambleModel: Option[PreambleModel] =
    divisions.collect {
      case m: PreambleDivision => m.makeModel(config)
    }.foldLeft(PreambleModel.empty(config))(_ |+| _).toOption

  lazy val getServiceModel: Option[ServiceModel] =
    divisions.collect {
      case m: ServiceDivision => m.makeModel(config)
    }.concatenate.toOption

  lazy val getDataTypeModel: Option[DataTypeModel] = 
    divisions.collect {
      case m: DataTypeDivision => m.makeModel(config)
    }.concatenate.toOption

  lazy val takeDataTypeModel = getDataTypeModel.orZero

  lazy val getEventModel: Option[EventModel] =
    divisions.collect {
      case m: EventDivision => m.makeModel(config)
    }.concatenate.toOption

  def eventModel: EventModel = getEventModel.orZero

  lazy val getStateMachineModel: Option[StateMachineModel] =
    divisions.collect {
      case m: StateMachineDivision => m.makeModel(config)
    }.concatenate.toOption

  lazy val takeStateMachineModel = getStateMachineModel.orZero

  def stateMachineModel: StateMachineModel = getStateMachineModel.orZero

  def takeStateMachineClasses: TreeMap[StateMachineClass] =
    getStateMachineModel.orZero.classes

  def getDataSet: Option[DataSet] = {
    val a = divisions.collect {
      case m: DataDivision => m.dataset(_ctx)
    }
    a.headOption // TODO concat
  }

  def getDataStore: Option[DataStoreModel] = {
    val a = divisions.collect {
      case m: DataStoreDivision => m.makeModel(_ctx)
    }
    a.concatenate.toOption
  }

  def getDataBag: Option[DataSet] = {
    val a = divisions.collect {
      case m: DataBagDivision => m.dataset(_ctx)
    }
    a.headOption // TODO concat
  }

  def getXslModel: Option[XslModel] = {
    val a = divisions.collect {
      case m: XslDivision => m.makeModel(config)
    }
    a.concatenate.toOption
  }

  def +(p: Model): Model = {
    case class Z(r: Vector[Division]) {
      def +(rhs: Division) = {
        copy(_divisions(r, rhs))
        // Z(r./:(ZZ())(_+_).r)
      }
    }
    val ds = p.divisions./:(Z(divisions))(_+_).r
    val ims = libraries + p.libraries // importedModels + p.importedModels
    val es = errors ++ p.errors
    val ws = warnings ++ p.warnings
    Model(p.config, ds, ims, es, ws)
  }

  private def _divisions(xs: Vector[Division], p: Division) = {
    case class Z(
      xs: Vector[Division] = Vector.empty,
      isdone: Boolean = false
    ) {
      def r = if (isdone) xs else xs :+ p

      def +(x: Division) =
        if (isdone)
          copy(xs = xs :+ x)
        else
          x.mergeOption(p).
            map(merged => copy(xs = xs :+ merged, isdone = true)).
            getOrElse(copy(xs = xs :+ x))
    }
    xs./:(Z())(_+_).r
  }
}

object Model {
  val empty = Model(Config.default, Vector.empty, Libraries.empty) // ImportedModels.empty)

  implicit object ModelMonoid extends Monoid[Model] {
    def zero = Model.empty
    def append(lhs: Model, rhs: => Model) = lhs + rhs
  }

  trait Division extends Showable with Doxes {
    def name: String

    override def toString() = try {
      super.toString()
    } catch {
      case NonFatal(e) => s"${getClass.getSimpleName}(${e})"
    }

    def mergeOption(p: Division): Option[Division]

    protected final def to_hocon(p: LogicalParagraph): RichConfig =
      p.lines.lines.map(x => HoconUtils.parse(x.text)).concatenate

    def print: String = s"[${StringUtils.capitalize(name)}]${print_Summary}\n${print_Description}"

    protected def print_Summary: String = ""
    protected def print_Description: String = ""

    protected final def make_description(c: Config, p: LogicalSection): Description =
      make_description(c.doxConfig, p)

    protected final def make_sections(c: Config, p: LogicalSection): List[Section] =
      make_sections(c.doxConfig, p)

    protected final def make_description_sections(c: Config, p: LogicalSection): (Description, List[Section]) =
      make_description_sections(c.doxConfig, p)
  }
  object Division {
    val elements = Vector(
      IdentificationDivision,
      ImportDivision,
      SignatureDivision,
      EnvironmentDivision,
      DataDivision, // TODO change semantics
      DataStoreDivision,
      DataBagDivision,
      DataSourceDivision,
      XslDivision,
      Script,
      DocumentDivision, // unused
      VoucherDivision, // unused
      SchemaDivision,
      DataTypeDivision,
      PowertypeDivision,
      ValueDivision,
      SlipDivision,
      ServiceDivision,
      EventDivision,
      StateMachineDivision,
      EntityDivision,
      PrologueDivision,
      EpilogueDivision,
      TestDivision,
      PreambleDivision
    )

    def take(p: LogicalSection): Division = elements.toStream.flatMap(_.accept(p)).headOption.getOrElse(DocumentDivision(p))
  }

  trait DivisionFactory {
    protected val name_Candidates: Vector[String] = Vector.empty

    def accept(p: LogicalSection): Option[Division] =
      if (is_Match(p.keyForModel))
        Some(to_Division(p))
      else
        None

    protected def is_Match(name: String): Boolean = name_Candidates.exists(_.equalsIgnoreCase(name))

    protected def to_Division(p: LogicalSection): Division
  }

  case class IdentificationDivision(section: LogicalSection) extends Division {
    val name = "identification"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: IdentificationDivision => copy(section + m.section)
    }
  }
  object IdentificationDivision extends DivisionFactory {
    override val name_Candidates = Vector("id", "identification")

    protected def to_Division(p: LogicalSection): Division = IdentificationDivision(p)
  }

  case class ImportDivision(section: LogicalSection) extends Division {
    val name = "import"

    def locators: Vector[/*ImportedModel.Locator*/Locator] = {
      val a = section.blocks.blocks map {
        case m: LogicalParagraph => _paragraph(m)
        case m: LogicalSection => _section(m)
        case m => Vector.empty
      }
      a.concatenate
    }

    private def _paragraph(p: LogicalParagraph): Vector[/*ImportedModel.Locator*/Locator] = {
      p.lines.lines.flatMap(_get_in_line)
    }

    private def _get_in_line(p: LogicalLine): Option[/*ImportedModel.Locator*/Locator] =
      _get_json(p) orElse _get_hocon(p) orElse _get_line(p)

    private def _get_json(p: LogicalLine): Option[/*ImportedModel.Locator*/Locator] = None

    private def _get_hocon(p: LogicalLine): Option[/*ImportedModel.Locator*/Locator] = None

    private def _get_line(p: LogicalLine): Option[/*ImportedModel.Locator*/Locator] =
      Strings.notblankp(p.text) option /*ImportedModel.Locator*/Locator.create(p.text.trim)

    private def _section(p: LogicalSection): Vector[/*ImportedModel.Locator*/Locator] =
      RAISE.notImplementedYetDefect

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ImportDivision => copy(section + m.section)
    }
  }
  object ImportDivision extends DivisionFactory {
    override val name_Candidates = Vector("import")

    protected def to_Division(p: LogicalSection): Division = ImportDivision(p)
  }

  case class SignatureDivision(section: LogicalSection) extends Division {
    val name = "signature"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: SignatureDivision => copy(section + m.section)
    }
  }
  object SignatureDivision extends DivisionFactory {
    override val name_Candidates = Vector("signature")

    protected def to_Division(p: LogicalSection): Division = SignatureDivision(p)
  }

  case class EnvironmentDivision(
    text: LogicalSection,
    properties: IRecord
  ) extends Division {
    val name = "environment"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: EnvironmentDivision => copy(text + m.text, properties + m.properties)
    }
  }
  object EnvironmentDivision extends DivisionFactory {
    override val name_Candidates = Vector("env", "environment")
    protected def to_Division(p: LogicalSection): Division =
      EnvironmentDivision.create(p)

    def create(p: LogicalSection): EnvironmentDivision = {
      val hocon = p.blocks.blocks.map(_to_hocon).concatenate
      EnvironmentDivision(p, RHoconUtils.toRecord(hocon.config))
    }

    private def _to_hocon(p: LogicalBlock): RichConfig = p match {
      case m: LogicalSection => RAISE.notImplementedYetDefect
      case m: LogicalParagraph => m.lines.lines.map(x => HoconUtils.parse(x.text)).concatenate
      case _ => RAISE.notImplementedYetDefect
    }

    def create(p: IRecord): EnvironmentDivision = EnvironmentDivision(
      LogicalSection.empty,
      p
    )

    def toHocon(p: LogicalBlock): RichConfig = _to_hocon(p)
  }

  // TODO change semantics more generic: refer COBOL data division
  case class DataDivision(
    section: LogicalSection
  ) extends Division {
    val name = "data"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.createData(ctx, section)
  }
  object DataDivision extends DivisionFactory {
    override val name_Candidates = Vector("data", "domain")
    protected def to_Division(p: LogicalSection): Division = DataDivision(p)
  }

  case class DataStoreDivision(
    section: LogicalSection
  ) extends Division {
    val name = "dataStore"

    def makeModel(context: DataSet.Builder.Context): DataStoreModel = {
      _make_datastores(context, section)
    }

    // def makeModel(config: Config): DataStoreModel = {
    //   val doxconfig = Dox2Parser.Config.default // TODO
    //   val dox = Dox2Parser.parse(doxconfig, section)
    //   // println(s"DataStoreDivision#makeModel $dox")
    //   _make(config, dox)
    // }

    // private def _make(config: Config, p: Dox): DataStoreModel = {
    //   // println(s"DataStoreModel#_make $p")
    //   p match {
    //     case m: Section =>
    //       if (m.keyForModel == "data-store") // TODO
    //         _make_datastores(config, m)
    //       else
    //         DataStoreModel.empty
    //     case m => m.elements.foldMap(_make(config, _))
    //   }
    // }

    private def _make_datastores(context: DataSet.Builder.Context, p: LogicalSection): DataStoreModel =
      p.sections.foldMap(_make_datastore(context, _))

    private def _make_datastore(context: DataSet.Builder.Context, p: LogicalSection): DataStoreModel =
      DataStoreModel.create(context, p)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataStoreDivision => copy(section + m.section)
    }
  }
  object DataStoreDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-store")
    protected def to_Division(p: LogicalSection): Division = DataStoreDivision(p)
  }

  case class DataBagDivision(
    section: LogicalSection
  ) extends Division {
    val name = "dataBag"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataBagDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.createData(ctx, section)

    def getScript(ctx: DataSet.Builder.Context): Option[Script] = DataSet.getScript(ctx, section)
  }
  object DataBagDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-bag")
    protected def to_Division(p: LogicalSection): Division = DataBagDivision(p)
  }

  case class DataSourceDivision(
    section: LogicalSection
  ) extends Division {
    val name = "dataSource"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataSourceDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.createDataSource(ctx, section)
  }
  object DataSourceDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-source")
    protected def to_Division(p: LogicalSection): Division = DataSourceDivision(p)
  }

  case class XslDivision(
    section: LogicalSection
  ) extends Division {
    val name = "Xsl"

    def makeModel(config: Config): XslModel = XslModel.create(config, section)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: XslDivision => copy(section + m.section)
    }

    // def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.createXsl(ctx, section)
  }
  object XslDivision extends DivisionFactory {
    override val name_Candidates = Vector("xsl")
    protected def to_Division(p: LogicalSection): Division = XslDivision(p)
  }

  // TODO literal document (not value object)
  case class DocumentDivision(section: LogicalSection) extends Division {
    val name = "document"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DocumentDivision => copy(section + m.section)
    }
  }
  object DocumentDivision extends DivisionFactory {
    override val name_Candidates = Vector("document")
    protected def to_Division(p: LogicalSection): Division = DocumentDivision(p)
  }

  case class VoucherDivision(section: LogicalSection) extends Division {
    val name = "voucher"

    def makeModel: VoucherModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"VoucherDivision#makeModel $dox")
      _make(dox)
    }

    private def _make(p: Dox): VoucherModel = {
      // println(s"VoucherModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "voucher") // TODO
            _make_vouchers(m)
          else
            VoucherModel.empty
        case m => m.elements.foldMap(_make)
      }
    }

    private def _make_vouchers(p: Section): VoucherModel = p.sections.foldMap(_make_voucher)

    private def _make_voucher(p: Section): VoucherModel =
      VoucherModel.VoucherClass.createOption(p).
        map(VoucherModel.apply).
        getOrElse(VoucherModel.empty)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: VoucherDivision => copy(section + m.section)
    }
  }
  object VoucherDivision extends DivisionFactory {
    override val name_Candidates = Vector("voucher")
    protected def to_Division(p: LogicalSection): Division = VoucherDivision(p)
  }

  case class DataTypeDivision(section: LogicalSection) extends Division {
    val name = "datatype"

    def makeModel(config: Config): DataTypeModel =
      DataTypeModel.create(config, section)

    // def makeModel(config: Config): DataTypeModel = {
    //   val doxconfig = Dox2Parser.Config.default // TODO
    //   val dox = Dox2Parser.parse(doxconfig, section)
    //   // println(s"DataTypeDivision#makeModel $dox")
    //   _make(dox)
    // }

    // private def _make(p: Dox): DataTypeModel = {
    //   // println(s"SchemaModel#_make $p")
    //   p match {
    //     case m: Section =>
    //       if (m.keyForModel == "schema") // TODO
    //         _make_schemas(m)
    //       else
    //         DataTypeModel.empty
    //     case m => m.elements.foldMap(_make)
    //   }
    // }

    // private def _make_schemas(p: Section): DataTypeModel = p.sections.foldMap(_make_schema)

    // private def _make_schema(p: Section): DataTypeModel =
    //   DataTypeModel.DataTypeClass.createOption(p).
    //     map(DataTypeModel.apply).
    //     getOrElse(DataTypeModel.empty)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataTypeDivision => copy(section + m.section)
    }
  }
  object DataTypeDivision extends DivisionFactory {
    override val name_Candidates = Vector("datatype")
    protected def to_Division(p: LogicalSection): Division = DataTypeDivision(p)
  }

  case class PowertypeDivision(section: LogicalSection) extends Division {
    val name = "datatype"

    def makeModel: PowertypeModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"PowertypeDivision#makeModel $dox")
      _make(dox)
    }

    private def _make(p: Dox): PowertypeModel = {
      // println(s"PowertypeModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "schema") // TODO
            _make_schemas(m)
          else
            PowertypeModel.empty
        case m => m.elements.foldMap(_make)
      }
    }

    private def _make_schemas(p: Section): PowertypeModel = p.sections.foldMap(_make_schema)

    private def _make_schema(p: Section): PowertypeModel =
      PowertypeModel.PowertypeClass.createOption(p).
        map(PowertypeModel.apply).
        getOrElse(PowertypeModel.empty)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: PowertypeDivision => copy(section + m.section)
    }
  }
  object PowertypeDivision extends DivisionFactory {
    override val name_Candidates = Vector("schema")
    protected def to_Division(p: LogicalSection): Division = PowertypeDivision(p)
  }

  case class SchemaDivision(section: LogicalSection) extends Division {
    val name = "schema"

    def makeModel: SchemaModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"SchemaDivision#makeModel $dox")
      _make(dox)
    }

    private def _make(p: Dox): SchemaModel = {
      // println(s"SchemaModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "schema") // TODO
            _make_schemas(m)
          else
            SchemaModel.empty
        case m => m.elements.foldMap(_make)
      }
    }

    private def _make_schemas(p: Section): SchemaModel = p.sections.foldMap(_make_schema)

    private def _make_schema(p: Section): SchemaModel =
      SchemaModel.SchemaClass.createOption(p).
        map(SchemaModel.apply).
        getOrElse(SchemaModel.empty)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: SchemaDivision => copy(section + m.section)
    }
  }
  object SchemaDivision extends DivisionFactory {
    override val name_Candidates = Vector("schema")
    protected def to_Division(p: LogicalSection): Division = SchemaDivision(p)
  }

  case class ServiceDivision(section: LogicalSection) extends Division {
    val name = "service"

    def makeModel(config: Config): ServiceModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"ServiceDivision#makeModel $dox")
      _make(config, dox)
    }

    private def _make(config: Config, p: Dox): ServiceModel = {
      // println(s"ServiceModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "service") // TODO
            _make_services(config, m)
          else
            ServiceModel.empty
        case m => m.elements.foldMap(_make(config, _))
      }
    }

    private def _make_services(config: Config, p: Section): ServiceModel =
      p.sections.foldMap(_make_service(config, _))

    private def _make_service(config: Config, p: Section): ServiceModel =
      ServiceModel.ServiceClass.createOption(config, p).
        map(ServiceModel.apply).
        getOrElse(ServiceModel.empty)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ServiceDivision => copy(section + m.section)
    }
  }
  object ServiceDivision extends DivisionFactory {
    override val name_Candidates = Vector("service")
    protected def to_Division(p: LogicalSection): Division = ServiceDivision(p)
  }

  case class ValueDivision(section: LogicalSection) extends Division {
    val name = "value"

    def makeModel: ValueModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"ValueDivision#makeModel $dox")
      _make(dox)
    }

    private def _make(p: Dox): ValueModel = {
      // println(s"ValueModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "value") // TODO
            _make_values(m)
          else
            ValueModel.empty
        case m => m.elements.foldMap(_make)
      }
    }

    private def _make_values(p: Section): ValueModel = p.sections.foldMap(_make_value)

    private def _make_value(p: Section): ValueModel = ValueModel.create(p)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: ValueDivision => copy(section + m.section)
    }
  }
  object ValueDivision extends DivisionFactory {
    override val name_Candidates = Vector("value")
    protected def to_Division(p: LogicalSection): Division = ValueDivision(p)
  }

  case class SlipDivision(section: LogicalSection) extends Division {
    val name = "slip"

    def makeModel: SlipModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"SlipDivision#makeModel $dox")
      _make(dox)
    }

    private def _make(p: Dox): SlipModel = {
      // println(s"SlipModel#_make $p")
      p match {
        case m: Section =>
          if (m.keyForModel == "slip") // TODO
            _make_slips(m)
          else
            SlipModel.empty
        case m => m.elements.foldMap(_make)
      }
    }

    private def _make_slips(p: Section): SlipModel = p.sections.foldMap(_make_slip)

    private def _make_slip(p: Section): SlipModel = SlipModel.create(p)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: SlipDivision => copy(section + m.section)
    }
  }
  object SlipDivision extends DivisionFactory {
    override val name_Candidates = Vector("slip")
    protected def to_Division(p: LogicalSection): Division = SlipDivision(p)
  }

  case class EntityDivision(section: LogicalSection) extends Division {
    val name = "entity"

    def makeModel(s: Option[SchemaModel], f: KaleidoxEntityFactory): EntityModel = {
      val a = section.sections.foldLeft(EntityModel.empty(f))((z, x) => z + EntityModel.create(s, f, x))
      a.resolve()
    }

    // def makeModel: EntityModel = {
    //   val doxconfig = Dox2Parser.Config.default // TODO
    //   val dox = Dox2Parser.parse(doxconfig, section)
    //   // println(s"EntityDivision#makeModel $dox")
    //   _make(dox)
    // }

    // private def _make(p: Dox): EntityModel = {
    //   // println(s"EntityModel#_make $p")
    //   p match {
    //     case m: Section =>
    //       if (m.keyForModel == "entity") // TODO
    //         _make_entities(m)
    //       else
    //         EntityModel.empty
    //     case m => m.elements.foldMap(_make)
    //   }
    // }

    // private def _make_entities(p: Section): EntityModel = p.sections.foldMap(_make_entity)

    // private def _make_entity(p: Section): EntityModel = EntityModel.create(p)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: EntityDivision => copy(section + m.section)
    }
  }
  object EntityDivision extends DivisionFactory {
    override val name_Candidates = Vector("entity")
    protected def to_Division(p: LogicalSection): Division = EntityDivision(p)
  }

  case class EventDivision(section: LogicalSection) extends Division {
    val name = "event"

    def makeModel(config: Config): EventModel = EventModel.create(config, section)

    // def makeModel(config: Config): EventModel = {
    //   val doxconfig = Dox2Parser.Config.default // TODO
    //   val dox = Dox2Parser.parse(doxconfig, section)
    //   // println(s"EventDivision#makeModel $dox")
    //   _make(config, dox)
    // }

    // private def _make(config: Config, p: Dox): EventModel = {
    //   // println(s"EventModel#_make $p")
    //   p match {
    //     case m: Section =>
    //       if (m.keyForModel == "event") // TODO
    //         _make_events(config, m)
    //       else
    //         EventModel.empty
    //     case m => m.elements.foldMap(_make(config, _))
    //   }
    // }

    // private def _make_events(config: Config, p: LogicalSection): EventModel =
    //   p.sections.foldMap(_make_event(config, _))

    // private def _make_event(config: Config, p: LogicalSection): EventModel =
    //   EventModel.create(config, p)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: EventDivision => copy(section + m.section)
    }
  }
  object EventDivision extends DivisionFactory {
    override val name_Candidates = Vector("event")
    protected def to_Division(p: LogicalSection): Division = EventDivision(p)
  }

  case class StateMachineDivision(section: LogicalSection) extends Division {
    val name = "stateMachine"

    def makeModel(config: Config): StateMachineModel = StateMachineModel.create(config, section)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: StateMachineDivision => copy(section + m.section)
    }
  }
  object StateMachineDivision extends DivisionFactory {
    override val name_Candidates = Vector("statemachine")
    protected def to_Division(p: LogicalSection): Division = StateMachineDivision(p)
  }

  case class PrologueDivision(section: LogicalSection) extends Division {
    val name = "prologue"

    def getScript(config: Config): Option[Script] = Script.parseOption(config, section)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: PrologueDivision => copy(section + m.section)
    }
  }
  object PrologueDivision extends DivisionFactory {
    override val name_Candidates = Vector("prologue")
    protected def to_Division(p: LogicalSection): Division = PrologueDivision(p)
  }

  case class EpilogueDivision(section: LogicalSection) extends Division {
    val name = "epilogue"

    def getScript(config: Config): Option[Script] = Script.parseOption(config, section)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: EpilogueDivision => copy(section + m.section)
    }
  }
  object EpilogueDivision extends DivisionFactory {
    override val name_Candidates = Vector("epilogue")
    protected def to_Division(p: LogicalSection): Division = EpilogueDivision(p)
  }

  case class TestDivision(section: LogicalSection) extends Division {
    val name = "test"

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: TestDivision => copy(section + m.section)
    }
  }
  object TestDivision extends DivisionFactory {
    override val name_Candidates = Vector("test")
    protected def to_Division(p: LogicalSection): Division = TestDivision(p)
  }

  case class PreambleDivision(divisions: Vector[Division]) extends Division {
    val name = "preamble"

    def makeModel(config: Config): PreambleModel = {
      case class Z(
        vision: Option[VisionModel] = None,
        business: Option[BusinessModel] = None,
        requirement: Option[RequirementModel] = None,
        analysis: Option[AnalysisModel] = None
      ) {
        def r = PreambleModel(
          Description.name("preamble"), // TODO
          vision getOrElse VisionModel.empty,
          business getOrElse BusinessModel.empty(config),
          requirement getOrElse RequirementModel.empty(config),
          analysis getOrElse AnalysisModel.empty(config)
        )

        def +(rhs: Division) = rhs match {
          case m: VisionModel.VisionDivision => copy(vision = Some(m.makeModel(config)))
          case m: BusinessModel.BusinessDivision => copy(business = Some(m.makeModel(config)))
          case m: RequirementModel.RequirementDivision => copy(requirement = Some(m.makeModel(config)))
          case m: AnalysisModel.AnalysisDivision => copy(analysis = Some(m.makeModel(config)))
          case m => this // TODO
        }
      }
      divisions./:(Z())(_+_).r
    }

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: PreambleDivision => copy(divisions = divisions ++ m.divisions)
    }
  }
  object PreambleDivision extends DivisionFactory {
    val divisions = Vector(
      VisionModel.VisionDivision,
      BusinessModel.BusinessDivision,
      RequirementModel.RequirementDivision,
      AnalysisModel.AnalysisDivision
    )

    override val name_Candidates = PreambleModel.divisionNames
    protected def to_Division(p: LogicalSection): Division = {
      val x = divisions.toStream.flatMap(_.accept(p)).headOption.getOrElse(DocumentDivision(p))
      PreambleDivision(Vector(x))
    }
  }

  trait ISubModel extends Showable.Base with Description.Holder {
  }

  trait ISchemaSubModel extends ISchemaModel with ISubModel {
  }

  def apply(config: Config, p: Division, ps: Division*): Model = Model(config, p +: ps.toVector, Libraries.empty) // ImportedModels.empty)

  // def parse(p: String): Model = parse(Config.default, p)

  def load(config: Config, p: File): Model = Builder(config).load(p)
  def load(config: Config, p: String): Model = Builder(config).load(p)
  def load(config: Config, p: URL): Model = Builder(config).load(p)
  def load(config: Config, p: URI): Model = Builder(config).load(p)

  def parse(config: Config, p: String): Model = Builder(config).parse(p)
  def parseExpression(config: Config, p: String): Model = Builder(config).parseExpression(p)

  // TODO schema for direct service
  def httpCall(config: Config, funcname: String, query: IRecord, form: IRecord): Model = {
    val a = SAtom(funcname) :: _params(query) ::: _params(form)
    val sexpr = SList.create(a)
    val script = Script(sexpr)
    Model(config, script)
  }

  def httpCall(config: Config, schema: Schema, funcname: String, query: IRecord, form: IRecord): Model = {
    val a = SAtom(funcname) :: _params(_normalize(schema, query)) ::: _params(_normalize(schema, form))
    val sexpr = SList.create(a)
    val script = Script(sexpr)
    Model(config, script)
  }

  private def _params(p: IRecord): List[SExpr] =
    p.fields.flatMap(x =>
      x.value match {
        case SingleValue(v) => List(SKeyword(x.name), SExpr.create(v))
        case MultipleValue(vs) => SKeyword(x.name) :: vs.map(SExpr.create).toList
        case EmptyValue => Nil
      }
    ).toList

  def httpEval(config: Config, schema: Schema, pscript: String, query: IRecord, form: IRecord): Model = {
    val env = Model.EnvironmentDivision.create(_normalize(schema, query + form))
    val script = Script.parse(config, pscript)
    // val a = SAtom(funcname) :: _params(query) ::: _params(form)
    // val sexpr = SList.create(a)
    // val script = Script(sexpr)
    // Model(config, script)
    Model(config, env, script)
  }

  private def _normalize(schema: Schema, p: IRecord): IRecord = {
    case class Z(xs: Vector[Field] = Vector.empty) {
      def r = Record(xs)

      def +(rhs: Column) = p.getField(rhs.name) match {
        case Some(s) =>
          val v = s.value.mapContent(_to_instance(rhs))
          val f = s.withValue(v)
          copy(xs = xs :+ f)
        case None => this
      }
    }
    schema.columns./:(Z())(_+_).r
  }

  private def _to_instance(c: Column)(d: Any): Any = c.datatype.toInstance(d)

  def parseWitoutLocation(config: Config, p: String): Model = parse(config.withoutLocation, p)

  def error(p: Throwable): Model = Model(Config.default, Vector.empty, Libraries.empty /* ImportedModels.empty*/, Vector(ErrorMessage(p)), Vector.empty)

  def error(url: URL, p: Throwable): Model = p match {
    case m: SyntaxErrorFaultException => _error(m.complementUrl(url))
    case m: ParseSyntaxErrorException => _error(m.complementUrl(url))
    case m => error(m)
  }

  def error(uri: URI, p: Throwable): Model = p match {
    case m: SyntaxErrorFaultException => _error(m.complementUri(uri))
    case m: ParseSyntaxErrorException => _error(m.complementUri(uri))
    case m => error(m)
  }

  def error(file: File, p: Throwable): Model = p match {
    case m: SyntaxErrorFaultException => _error(m.complementFile(file))
    case m: ParseSyntaxErrorException => _error(m.complementFile(file))
    case m => error(m)
  }

  private def _error(p: SyntaxErrorFaultException) =
    Model(Config.default, Vector.empty, Libraries.empty /*ImportedModels.empty*/, p.errorMessages, p.warningMessages)

  private def _error(p: ParseSyntaxErrorException) =
    Model(Config.default, Vector.empty, Libraries.empty /*ImportedModels.empty*/, p.errors, p.warnings)

  class Builder(config: Config) {
    def load(p: File): Model = try {
      val encoding = config.charset
      val s = IoUtils.toText(p, encoding)
      _parse(s)
    } catch {
      case NonFatal(e) => error(p, e)
    }

    def load(p: String): Model = try {
      val encoding = config.charset
      val s = IoUtils.toText(p, encoding)
      _parse(s)
    } catch {
      case NonFatal(e) => error(e)
    }

    def load(p: URL): Model = try {
      val encoding = config.charset
      val s = IoUtils.toText(p, encoding)
      _parse(s)
    } catch {
      case NonFatal(e) => error(p, e)
    }

    def load(p: URI): Model = try {
      val encoding = config.charset
      val s = IoUtils.toText(p, encoding)
      _parse(s)
    } catch {
      case NonFatal(e) => error(p, e)
    }

    def parse(p: String): Model = try {
      _parse(p)
    } catch {
      case NonFatal(e) => error(e)
    }

    private def _parse(p: String): Model =  {
      val bconfig = if (config.isLocation)
        LogicalBlocks.Config.default.forLisp
      else
        LogicalBlocks.Config.noLocation.forLisp
      val blocks = LogicalBlocks.parse(bconfig, p)
      _parse(blocks)
    }

    def parseExpression(p: String): Model = try {
      val bconfig = if (config.isLocation)
        LogicalBlocks.Config.expression.forLisp
      else
        LogicalBlocks.Config.expression.withoutLocation.forLisp
      val blocks = LogicalBlocks.parse(bconfig, p)
      _parse(blocks)
    } catch {
      case NonFatal(e) => error(e)
    }

    private def _parse(blocks: LogicalBlocks): Model = {
      val divs = blocks.blocks collect {
        case m: LogicalSection => Division.take(m)
      }
      if (divs.isEmpty)
        Model(config, Script.parse(config, blocks))
      else
        _import_divisions(divs)
    }

    private def _import_divisions(divs: Vector[Division]): Model = {
      val a = divs.collect { case m: ImportDivision => m }
      val b = _import_models(a)
      Model(config, divs, /*b.toImportedModels*/b.toLibraries, b.toErrorMessages, b.toWarningMessages)
    }

    private def _import_models(ps: Vector[ImportDivision]): /*ImportingModels*/LibraryHangar = {
      val init = /*ImportingModels*/LibraryHangar.empty
      ps./:(init)((z, x) => _import_models(z, x))
    }

    private def _import_models(im: /*ImportingModels*/LibraryHangar, p: ImportDivision): /*ImportingModels*/LibraryHangar =
      p.locators./:(im)(_import_models)

    private def _import_models(im: /*ImportingModels*/LibraryHangar, p: /*ImportedModel.Locator*/Locator): /*ImportingModels*/LibraryHangar =
      if (im.isExists(p))
        im
      else
        im.add(p.uri, load(p.uri))
  }
  object Builder {
    def apply(config: Config): Builder = new Builder(config)
  }

  trait ModelBuilderBase {
    type T
    def config: Config

    protected def is_Accept(p: LogicalSection): Boolean

    protected def create_Model(
      p: LogicalSection,
      ps: Vector[LogicalSection],
      desc: Description
    ): T

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      properties: IRecord
    ): T

    protected def create_Model(
      p: LogicalSection,
      desc: Description,
      tables: List[Table]
    ): T

    def createOption(p: LogicalSection): Option[T] = {
      if (is_Accept(p))
        Some(create_model(p))
      else
        None
    }

    protected def create_model(p: LogicalSection): T = {
      val xs = p.sections
      if (xs.isEmpty)
        create_model_flat(p)
      else
        create_model_sections(p, xs)
    }

    protected def create_model_flat(p: LogicalSection): T = {
      val tables = dox_table_list(p)
      if (tables.isEmpty)
        create_model_properties(p)
      else
        create_model_tables(p, tables)
    }

    protected def create_model_properties(p: LogicalSection): T = {
      val desc = dox_description_name(p)
      val props = dox_properties(p)
      create_Model(p, desc, props)
    }

    protected def create_model_tables(p: LogicalSection, tables: List[Table]): T = {
      val desc = dox_description(p)
      create_Model(p, desc, tables)
    }

    protected def create_model_sections(
      p: LogicalSection,
      ps: Vector[LogicalSection]
    ): T = {
      val desc = dox_description(p)
      create_Model(p, ps, desc)
    }

    import org.smartdox.parser.Dox2Parser

    val doxconfig = config.doxConfig

    protected final def dox_table_list(p: LogicalSection): List[Table] = {
      val dox = Dox2Parser.parseSection(doxconfig, p)
      dox.tableList
    }

    protected final def dox_content(p: LogicalSection): Dox =
      dox_content(p.blocks)

    protected final def dox_content(p: LogicalBlocks): Dox =
      Dox2Parser.parse(doxconfig, p)

    protected final def dox_description(p: LogicalSection): Description = {
      val title = p.title
      val designation = Designation(title.toI18NString)
      val prologue = p.prologue
      val content = dox_content(prologue)
      Description(designation, content)
    }

    protected final def dox_description_name(p: LogicalSection): Description = {
      val title = p.title
      val designation = Designation(title.toI18NString)
      Description(designation)
    }

    protected final def dox_properties(p: LogicalSection): IRecord = {
      val s = p.text
      val hocon = ConfigFactory.parseString(s)
      HoconRecord(RichConfig(hocon))
    }
  }
  object ModelBuilderBase {
  }
}
