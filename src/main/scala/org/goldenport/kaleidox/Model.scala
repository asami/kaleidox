package org.goldenport.kaleidox

import scalaz._, Scalaz._
import org.goldenport.i18n.I18NElement
import org.goldenport.parser._
import org.goldenport.util.HoconUtils, HoconUtils.RichConfig

/*
 * @since   Sep. 24, 2018
 *  version Oct. 27, 2018
 * @version Feb. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class Model(
  divisions: Vector[Model.Division]
) {
  def getScript: Option[Script] = divisions.collect {
    case m: Script => m
  }.headOption // TODO

  def getEnvironmentProperties: Option[RichConfig] = {
    val a = divisions.collect {
      case m: Model.EnvironmentDivision => m
    }
    a.headOption.map(x => a.tail./:(x.properties)((z, x) => z.withFallback(x.properties)))
  }
}

object Model {
  trait Division {
  }
  object Division {
    val elements = Vector(
      IdentificationDivision,
      EnvironmentDivision,
      DataDivision,
      Script,
      DocumentDivision
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
  }
  object IdentificationDivision extends DivisionFactory {
    override val name_Candidates = Vector("id", "identification")

    protected def to_Division(p: LogicalSection): Division = IdentificationDivision(p)
  }

  case class EnvironmentDivision(
    text: LogicalSection,
    properties: RichConfig
  ) extends Division {
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
      case m: LogicalSection => ???
      case m: LogicalParagraph => m.lines.lines.map(x => HoconUtils.parse(x.text)).concatenate
      case _ => ???
    }
  }

  case class DataDivision(section: LogicalSection) extends Division {
  }
  object DataDivision extends DivisionFactory {
    override val name_Candidates = Vector("data", "domain")
    protected def to_Division(p: LogicalSection): Division = DataDivision(p)
  }

  case class DocumentDivision(section: LogicalSection) extends Division {
  }
  object DocumentDivision extends DivisionFactory {
    override val name_Candidates = Vector("document")
    protected def to_Division(p: LogicalSection): Division = DocumentDivision(p)
  }

  def apply(p: Division, ps: Division*): Model = Model(p +: ps.toVector)

  def parse(p: String): Model = parse(Config.default, p)

  def parse(config: Config, p: String): Model = {
    // println(s"Model#parse: $p")
    val bconfig = if (config.isLocation)
      LogicalBlocks.Config.default.forLisp
    else
      LogicalBlocks.Config.noLocation.forLisp
    val blocks = LogicalBlocks.parse(bconfig, p)
    // println(s"Model#parse $p => $blocks")
    val divs = blocks.blocks collect {
      case m: LogicalSection => Model.Division.take(m)
    }
    if (divs.isEmpty)
      Model(Script.parse(blocks))
    else
      Model(divs)
  }

  def parseWitoutLocation(p: String): Model = parse(Config.noLocation, p)
}
