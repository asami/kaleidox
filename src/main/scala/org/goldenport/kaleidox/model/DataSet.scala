package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v3.{Record, RecordSequence, Field, SingleValue, Table}
import org.goldenport.sexpr.{SSchema, STable}
import org.goldenport.parser._
import org.goldenport.collection.VectorMap
import org.goldenport.xsv.Xsv
import org.goldenport.kaleidox._
import VoucherModel._

/*
 * @since   Jul.  6, 2019
 *  version Jul. 16, 2019
 *  version Dec. 29, 2019
 * @version Feb. 28, 2021
 * @author  ASAMI, Tomoharu
 */
case class DataSet(
  slots: VectorMap[Symbol, DataSet.Slot] = VectorMap.empty,
  parseMessages: ParseMessageSequence = ParseMessageSequence.empty
) {
  def setup(p: Space): Space = {
    val rs: Seq[Field] = slots.vector.map {
      case (k, v) => Field(k, SingleValue(v.data))
    }
    val rec = Record(rs)
    p.updateBindings(rec)
  }

  def +(rhs: DataSet): DataSet = DataSet(
    slots ++ rhs.slots,
    parseMessages + rhs.parseMessages
  )
}

object DataSet {
  val empty = DataSet()

  implicit object DataSetMonoid extends Monoid[DataSet] {
    def zero = DataSet.empty
    def append(lhs: DataSet, rhs: => DataSet) = lhs + rhs
  }

  case class Slot(
    model: ISchemaClass,
    data: STable
  )

  def create(ctx: Builder.Context, p: LogicalSection): DataSet =
    Builder(ctx)(p)

  def create(dataname: String, model: ISchemaClass, xs: Iterator[Record]): DataSet =
    create(dataname, model, RecordSequence(xs))

  def create(dataname: String, model: ISchemaClass, xs: RecordSequence): DataSet =
    DataSet(VectorMap(Symbol(dataname) -> DataSet.Slot(model, STable(Table.create(model.schema, xs)))))

  def warning(p: String): DataSet = DataSet(parseMessages = ParseMessageSequence.warning(p))

  case class Builder(ctx: Builder.Context) {
    private def _get_model_by_name(p: String): Option[ISchemaClass] = ctx.getSchemaClass(p)

    def apply(p: LogicalSection) = {
      p.blocks.blocks.foldMap {
        case m: LogicalSection => _model(m)
        case m: LogicalParagraph => DataSet.empty
        case m: LogicalVerbatim => DataSet.empty
        case StartBlock => DataSet.empty
        case EndBlock => DataSet.empty
      }
    }

    private def _model(p: LogicalSection): DataSet = {
      val modelname = p.nameForModel
      p.blocks.blocks.foldMap {
        case m: LogicalSection => _data(modelname, m)
        case m: LogicalParagraph => _data(modelname, m)
        case m: LogicalVerbatim => DataSet.empty
        case StartBlock => DataSet.empty
        case EndBlock => DataSet.empty
      }
    }

    private def _data(modelname: String, p: LogicalSection): DataSet =
      _get_model_by_name(modelname).map { model =>
        val dataname = p.nameForModel
        p.blocks.blocks.foldMap {
          case m: LogicalSection => DataSet.empty
          case m: LogicalParagraph => _data(model, dataname, m)
          case m: LogicalVerbatim => DataSet.empty
          case StartBlock => DataSet.empty
          case EndBlock => DataSet.empty
        }
      }.getOrElse(
        DataSet.warning(s"Unknown model: $modelname")
      )

    private def _data(modelname: String, p: LogicalParagraph): DataSet =
      _get_model_by_name(modelname).map { model =>
        _data(model, modelname, p)
      }.getOrElse(
        DataSet.warning(s"Unknown model: $modelname")
      )

    private def _data(
      model: ISchemaClass,
      dataname: String,
      p: LogicalParagraph
    ): DataSet = {
      val matrix = Xsv.parse(p)
      val schema = model.schema
      val xs = for (row <- matrix.rowIterator) yield {
        _record_create(schema, row.map(_.value))
      }
      DataSet.create(dataname, model, xs)
    }

    private def _record_create(schema: Schema, data: Seq[Any]): Record = {
      import org.joda.time._
      import org.goldenport.record.v2.{XDateTime}
      val xs = schema.columns.toVector.zip(data).map {
        case (c, d) => c.datatype match {
          case XDateTime => d match {
            case m: DateTime => Field.create(c.name, m)
            case m: LocalDateTime => Field.create(c.name, m)
            case m => Field.create(c, d)
          }
          case _ => Field.create(c, d)
        }
      }
      Record(xs)
    }
  }
  object Builder {
    case class Context(models: List[ISchemaModel]) {
      def getSchemaClass(name: String): Option[ISchemaClass] = models.toStream.flatMap(_.get(name)).headOption
    }
    object Context {
      def apply(p: ISchemaModel, ps: ISchemaModel*): Context = apply(p +: ps)

      def apply(ps: Seq[ISchemaModel]): Context = Context(ps.toList)
    }
  }
}
