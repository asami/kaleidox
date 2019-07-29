package org.goldenport.kaleidox

import scalaz._, Scalaz._
import java.io.File
import java.net.{URL, URI}
import org.goldenport.RAISE
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
 * @version Jul. 15, 2019
 * @author  ASAMI, Tomoharu
 */
case class Model(
  divisions: Vector[Model.Division]
) {
  import Model._

  def getPrologue: Option[Script] = divisions.flatMap {
    case m: PrologueDivision => m.getScript
    case _ => None
  }.headOption // TODO

  def getEpilogue: Option[Script] = divisions.flatMap {
    case m: EpilogueDivision => m.getScript
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

  def getDataSet: Option[DataSet] = getVoucherModel.flatMap { model =>
    val ctx = DataSet.Builder.Context(model)
    val a = divisions.collect {
      case m: DataDivision => m.dataset(ctx)
    }
    a.headOption // TODO concat
  }

  def +(p: Model): Model = {
    case class Z(r: Vector[Division]) {
      def +(rhs: Division) = {
        copy(_divisions(r, rhs))
        // Z(r./:(ZZ())(_+_).r)
      }
    }
    Model(p.divisions./:(Z(divisions))(_+_).r)
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
      EnvironmentDivision,
      DataDivision,
      Script,
      DocumentDivision, // unused
      VoucherDivision,
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
      if (is_Match(p.title.toI18NString.key))
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
      println(s"VoucherDivision#makeModel $dox")
      _make(dox)
    }

    import org.smartdox._

    private def _make(p: Dox): VoucherModel = {
      println(s"VoucherModel#_make $p")
      p match {
        case m: Section =>
          if (m.titleName.toLowerCase == "voucher") // TODO
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
    def getScript: Option[Script] = Script.parseOption(section)

    def mergeOption(p: Division): Option[Division] = Option(p) collect {
      case m: PrologueDivision => copy(section + m.section)
    }
  }
  object PrologueDivision extends DivisionFactory {
    override val name_Candidates = Vector("prologue")
    protected def to_Division(p: LogicalSection): Division = PrologueDivision(p)
  }

  case class EpilogueDivision(section: LogicalSection) extends Division {
    def getScript: Option[Script] = Script.parseOption(section)

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

  def load(config: Config, p: File): Model = {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    parse(config, s)
  }

  def load(config: Config, p: String): Model = {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    parse(config, s)
  }

  def load(config: Config, p: URL): Model = {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    parse(config, s)
  }

  def load(config: Config, p: URI): Model = {
    val encoding = config.charset
    val s = IoUtils.toText(p, encoding)
    parse(config, s)
  }

  def parse(config: Config, p: String): Model = {
    // println(s"Model#parse: $p")
    val bconfig = if (config.isLocation)
      LogicalBlocks.Config.default.forLisp
    else
      LogicalBlocks.Config.noLocation.forLisp
    val blocks = LogicalBlocks.parse(bconfig, p)
    // println(s"Model#parse $p => $blocks")
    _parse(blocks)
  }

  private def _parse(blocks: LogicalBlocks): Model = {
    val divs = blocks.blocks collect {
      case m: LogicalSection => Division.take(m)
    }
    if (divs.isEmpty)
      Model(Script.parse(blocks))
    else
      Model(divs)
  }

  def parseWitoutLocation(config: Config, p: String): Model = parse(config.withoutLocation, p)
}
