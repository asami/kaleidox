package org.goldenport.kaleidox

import scalaz._, Scalaz._
import scala.util.control.NonFatal
import java.io.File
import java.net.{URL, URI}
import org.goldenport.RAISE
import org.goldenport.exception.SyntaxErrorFaultException
import org.goldenport.i18n.I18NElement
import org.goldenport.parser._
import org.goldenport.hocon.{RichConfig, HoconUtils}
import org.goldenport.bag.BufferBag
import org.goldenport.io.IoUtils
import org.smartdox.parser.Dox2Parser
import org.goldenport.kaleidox.model._

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
 * @version Feb. 24, 2021
 * @author  ASAMI, Tomoharu
 */
case class Model(
  divisions: Vector[Model.Division],
  errors: Vector[ErrorMessage] = Vector.empty,
  warnings: Vector[WarningMessage] = Vector.empty
) {
  import Model._

  def getPrologue(config: Config): Option[Script] = divisions.flatMap {
    case m: PrologueDivision => m.getScript(config)
    case _ => None
  }.headOption // TODO

  def getEpilogue(config: Config): Option[Script] = divisions.flatMap {
    case m: EpilogueDivision => m.getScript(config)
    case _ => None
  }.headOption // TODO

  def getScript: Option[Script] = divisions.collect {
    case m: Script => m
  }.headOption // TODO

  def getEnvironmentProperties: Option[RichConfig] = {
    val a = divisions.collect {
      case m: EnvironmentDivision => m
    }
    a.headOption.map(x => a.tail./:(x.properties)((z, x) => z.withFallback(x.properties))) // TODO concat
  }

  lazy val getVoucherModel: Option[VoucherModel] = {
    val a = divisions.collect {
      case m: VoucherDivision => m.makeModel
    }
    a.headOption // TODO concat
  }

  lazy val getSchemaModel: Option[SchemaModel] = {
    val a = divisions.collect {
      case m: SchemaDivision => m.makeModel
    }
    a.headOption // TODO concat
  }

  private lazy val _ctx = (getSchemaModel, getVoucherModel) match {
    case (None, None) => None
    case (Some(s), None) => Some(DataSet.Builder.Context(s))
    case (None, Some(v)) => Some(DataSet.Builder.Context(v))
    case (Some(s), Some(v)) => Some(DataSet.Builder.Context(s, v))
  }


  def getDataSet: Option[DataSet] = {
    val a = divisions.collect {
      case m: DataDivision => _ctx.map(m.dataset)
    }
    a.flatten.headOption // TODO concat
  }

  def getDataStore: Option[DataSet] = {
    val a = divisions.collect {
      case m: DataStoreDivision => _ctx.map(m.dataset)
    }
    a.flatten.headOption // TODO concat
  }

  def +(p: Model): Model = {
    case class Z(r: Vector[Division]) {
      def +(rhs: Division) = {
        copy(_divisions(r, rhs))
        // Z(r./:(ZZ())(_+_).r)
      }
    }
    val ds = p.divisions./:(Z(divisions))(_+_).r
    val es = errors ++ p.errors
    val ws = warnings ++ p.warnings
    Model(ds, es, ws)
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
  val empty = Model(Vector.empty)

  implicit object ModelMonoid extends Monoid[Model] {
    def zero = Model.empty
    def append(lhs: Model, rhs: => Model) = lhs + rhs
  }

  trait Division {
    def mergeOption(p: Division): Option[Division]
  }
  object Division {
    val elements = Vector(
      IdentificationDivision,
      SignatureDivision,
      EnvironmentDivision,
      DataDivision, // TODO change semantics
      DataStoreDivision,
      DataBagDivision,
      DataSourceDivision,
      Script,
      DocumentDivision, // unused
      VoucherDivision,
      SchemaDivision,
      EntityDivision,
      PrologueDivision,
      EpilogueDivision,
      TestDivision
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
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: IdentificationDivision => copy(section + m.section)
    }
  }
  object IdentificationDivision extends DivisionFactory {
    override val name_Candidates = Vector("id", "identification")

    protected def to_Division(p: LogicalSection): Division = IdentificationDivision(p)
  }

  case class SignatureDivision(section: LogicalSection) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: SignatureDivision => copy(section + m.section)
    }
  }
  object SignatureDivision extends DivisionFactory {
    override val name_Candidates = Vector("signature")

    protected def to_Division(p: LogicalSection): Division = IdentificationDivision(p)
  }

  case class EnvironmentDivision(
    text: LogicalSection,
    properties: RichConfig
  ) extends Division {
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
      EnvironmentDivision(p, hocon)
    }

    private def _to_hocon(p: LogicalBlock): RichConfig = p match {
      case m: LogicalSection => RAISE.notImplementedYetDefect
      case m: LogicalParagraph => m.lines.lines.map(x => HoconUtils.parse(x.text)).concatenate
      case _ => RAISE.notImplementedYetDefect
    }
  }

  // TODO change semantics more generic: refer COBOL data division
  case class DataDivision(
    section: LogicalSection
  ) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.create(ctx, section)
  }
  object DataDivision extends DivisionFactory {
    override val name_Candidates = Vector("data", "domain")
    protected def to_Division(p: LogicalSection): Division = DataDivision(p)
  }

  case class DataStoreDivision(
    section: LogicalSection
  ) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataStoreDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.create(ctx, section)
  }
  object DataStoreDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-store")
    protected def to_Division(p: LogicalSection): Division = DataStoreDivision(p)
  }

  case class DataBagDivision(
    section: LogicalSection
  ) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataBagDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.create(ctx, section)
  }
  object DataBagDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-bag")
    protected def to_Division(p: LogicalSection): Division = DataBagDivision(p)
  }

  case class DataSourceDivision(
    section: LogicalSection
  ) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DataSourceDivision => copy(section + m.section)
    }

    def dataset(ctx: DataSet.Builder.Context): DataSet = DataSet.create(ctx, section)
  }
  object DataSourceDivision extends DivisionFactory {
    override val name_Candidates = Vector("data-source")
    protected def to_Division(p: LogicalSection): Division = DataSourceDivision(p)
  }

  // TODO literal document (not value object)
  case class DocumentDivision(section: LogicalSection) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: DocumentDivision => copy(section + m.section)
    }
  }
  object DocumentDivision extends DivisionFactory {
    override val name_Candidates = Vector("document")
    protected def to_Division(p: LogicalSection): Division = DocumentDivision(p)
  }

  case class VoucherDivision(section: LogicalSection) extends Division {
    def makeModel: VoucherModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"VoucherDivision#makeModel $dox")
      _make(dox)
    }

    import org.smartdox._

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

  case class SchemaDivision(section: LogicalSection) extends Division {
    def makeModel: SchemaModel = {
      val doxconfig = Dox2Parser.Config.default // TODO
      val dox = Dox2Parser.parse(doxconfig, section)
      // println(s"SchemaDivision#makeModel $dox")
      _make(dox)
    }

    import org.smartdox._

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

  case class EntityDivision(section: LogicalSection) extends Division {
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: EntityDivision => copy(section + m.section)
    }
  }
  object EntityDivision extends DivisionFactory {
    override val name_Candidates = Vector("entity")
    protected def to_Division(p: LogicalSection): Division = EntityDivision(p)
  }

  case class PrologueDivision(section: LogicalSection) extends Division {
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
    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: TestDivision => copy(section + m.section)
    }
  }
  object TestDivision extends DivisionFactory {
    override val name_Candidates = Vector("test")
    protected def to_Division(p: LogicalSection): Division = TestDivision(p)
  }

  def apply(p: Division, ps: Division*): Model = Model(p +: ps.toVector)

  // def parse(p: String): Model = parse(Config.default, p)

  def load(config: Config, p: File): Model = try {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    _parse(config, s)
  } catch {
    case NonFatal(e) => error(p, e)
  }

  def load(config: Config, p: String): Model = try {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    _parse(config, s)
  } catch {
    case NonFatal(e) => error(e)
  }

  def load(config: Config, p: URL): Model = try {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    _parse(config, s)
  } catch {
    case NonFatal(e) => error(p, e)
  }

  def load(config: Config, p: URI): Model = try {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    _parse(config, s)
  } catch {
    case NonFatal(e) => error(p, e)
  }

  def parse(config: Config, p: String): Model = try {
    _parse(config, p)
  } catch {
    case NonFatal(e) => error(e)
  }

  private def _parse(config: Config, p: String): Model =  {
    val bconfig = if (config.isLocation)
      LogicalBlocks.Config.default.forLisp
    else
      LogicalBlocks.Config.noLocation.forLisp
    val blocks = LogicalBlocks.parse(bconfig, p)
    _parse(config, blocks)
  }

  def parseExpression(config: Config, p: String): Model = try {
    val bconfig = if (config.isLocation)
      LogicalBlocks.Config.expression.forLisp
    else
      LogicalBlocks.Config.expression.withoutLocation.forLisp
    val blocks = LogicalBlocks.parse(bconfig, p)
    _parse(config, blocks)
  } catch {
    case NonFatal(e) => error(e)
  }

  private def _parse(config: Config, blocks: LogicalBlocks): Model = {
    val divs = blocks.blocks collect {
      case m: LogicalSection => Division.take(m)
    }
    if (divs.isEmpty)
      Model(Script.parse(config, blocks))
    else
      Model(divs)
  }

  def parseWitoutLocation(config: Config, p: String): Model = parse(config.withoutLocation, p)

  def error(p: Throwable): Model = Model(Vector.empty, Vector(ErrorMessage(p)), Vector.empty)

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
    Model(Vector.empty, p.errorMessages, p.warningMessages)

  private def _error(p: ParseSyntaxErrorException) =
    Model(Vector.empty, p.errors, p.warnings)
}
