package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import scala.util.Try
import org.smartdox.{Dox, Table => DTable}
import org.smartdox.Description
import org.smartdox.parser.Dox2Parser
import org.goldenport.RAISE
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v3.{Record, RecordSequence, Field, SingleValue, Table}
import org.goldenport.record.v3.ITable
import org.goldenport.sexpr.{SSchema, STable}
import org.goldenport.parser._
import org.goldenport.hocon.RichConfig
import org.goldenport.collection.VectorMap
import org.goldenport.xsv.Xsv
import org.goldenport.sexpr.util.SExprTextMaker
import org.goldenport.kaleidox._
import VoucherModel._

/*
 * DataBag
 * 
 * @since   Jul.  6, 2019
 *  version Jul. 16, 2019
 *  version Dec. 29, 2019
 *  version Feb. 28, 2021
 *  version Mar.  8, 2021
 *  version Jun. 25, 2021
 *  version Jan. 22, 2023
 *  version Aug. 21, 2023
 * @version Oct. 15, 2023
 * @author  ASAMI, Tomoharu
 */
case class DataSet(
  description: Description = Description.name("dataSet"),
  slots: VectorMap[Symbol, DataSet.Slot] = VectorMap.empty,
  parseMessages: ParseMessageSequence = ParseMessageSequence.empty
) extends Model.ISubModel {
  protected def display_String: String = "dataSet"

  protected def print_String: String = "dataSet"

  protected def show_String: String = "dataSet"

  def setup(p: Space): Space = {
    val rs: Seq[Field] = slots.vector.map {
      case (k, v) => Field(k, SingleValue(v.data))
    }
    val rec = Record(rs)
    p.updateBindings(rec)
  }

  def +(rhs: DataSet): DataSet = DataSet(
    slots = slots ++ rhs.slots,
    parseMessages = parseMessages + rhs.parseMessages
  )

  def getSlot(name: String) = slots.get(Symbol(name))
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

  def createData(ctx: Builder.Context, p: LogicalSection): DataSet = Builder(ctx, false)(p)

  def createDataStore(ctx: Builder.Context, p: LogicalSection): DataSet = Builder(ctx, true)(p)

  def createDataSource(ctx: Builder.Context, p: LogicalSection): DataSet = RAISE.notImplementedYetDefect

  def getScript(ctx: Builder.Context, p: LogicalSection): Option[Script] = Builder(ctx, false).getScript(p)

  def create(dataname: String, model: ISchemaClass, xs: Iterator[Record]): DataSet =
    create(dataname, model, RecordSequence(xs))

  def create(dataname: String, model: ISchemaClass, xs: RecordSequence): DataSet =
    DataSet(slots = VectorMap(Symbol(dataname) -> DataSet.Slot(model, STable(Table.create(model.schema, xs)))))

  def create(dataname: String, model: ISchemaClass, p: Xsv): DataSet = {
    val schema = model.schema
    val xs = for (row <- p.rowIterator) yield {
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

  def create(dataname: String, model: ISchemaClass, p: ITable): DataSet =
    DataSet(slots = VectorMap(Symbol(dataname) -> DataSet.Slot(model, STable(p))))

  def warning(p: String): DataSet = DataSet(parseMessages = ParseMessageSequence.warning(p))

  case class Builder(
    ctx: Builder.Context,
    isDataStore: Boolean = false
  ) {
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

    private def _model(p: LogicalSection): DataSet =
      if (isDataStore)
        _model_datastore(p)
      else
        _model_data(p)

    def createData(name: String, p: LogicalSection) = _model_data(p, name)

    // private def _is_nested(p: LogicalSection) =
    //   p.blocks.blocks.exists(_.isInstanceOf[LogicalSection])

    private def _model_data(p: LogicalSection): DataSet =
      _model_data(p, p.nameForModel)

    private def _model_data(p: LogicalSection, varname: String): DataSet = {
      case class Z(
        properties: RichConfig = RichConfig.empty,
        xsv: Option[Xsv] = None,
        table: Option[ITable] = None
      ) {
        def r = xsv.map(_to_dataset).orZero |+| table.map(_to_dataset).orZero

        private def _get_schema: Option[ISchemaClass] = {
          properties.getStringOption("schema").map(_name_to_schema) orElse
          properties.getStringOption("column").map(_column_to_schema) orElse
          Some(_name_to_schema(varname))
        }

        private def _name_to_schema(p: String): ISchemaClass =
          _get_model_by_name(p).getOrElse(RAISE.syntaxErrorFault(s"Unknown model: $p"))

        private def _column_to_schema(p: String): ISchemaClass =
          SchemaSchemaClass(Schema.createByColumnNamesWithComma(p))

        private def _to_dataset(p: Xsv): DataSet =
          _get_schema.map(_to_dataset(_, p)).getOrElse(RAISE.syntaxErrorFault(s"No model: $p"))

        private def _to_dataset(schema: ISchemaClass, p: Xsv): DataSet =
          DataSet.create(varname, schema, p)

        private def _to_dataset(p: ITable): DataSet =
          _get_schema.map(_to_dataset(_, p)).getOrElse {
            _to_dataset(SchemaSchemaClass(p.schema), p)
          }

        private def _to_dataset(schema: ISchemaClass, p: ITable): DataSet =
          DataSet.create(varname, schema, p)

        def +(rhs: LogicalBlock) = {
          _property_paragraph(rhs).map(_property) orElse
          _xsv_paragraph(rhs).map(_xsv) orElse
          _table_paragraph(rhs).map(_table) getOrElse this
        }

        private def _property(p: RichConfig) = copy(properties = properties + p)

        private def _xsv(p: Xsv) = copy(xsv = Some(p))

        private def _table(p: ITable) = copy(table = Some(p))
      }
      p.blocks.blocks./:(Z())(_+_).r
    }

    private def _is_property(p: LogicalBlock) = {
      val xs = p.lines.lines
      if (xs.isEmpty)
        false
      else
        xs.forall(_.text.contains('='))
    }

    private def _property_paragraph(p: LogicalBlock): Option[RichConfig] =
      if (_is_property(p)) {
        Some(Model.EnvironmentDivision.toHocon(p))
      } else {
        None
      }

    private def _is_xsv(p: LogicalBlock) = {
      val delimiters = Xsv.XsvStrategy.inputDelimiters
      val xs = p.lines.lines
      if (xs.isEmpty)
        false
      else
        !xs.exists(_.text.startsWith("|")) && xs.forall(_.text.exists(x => delimiters.contains(x)))
    }

    private def _xsv_paragraph(p: LogicalBlock): Option[Xsv] =
      if (_is_xsv(p)) {
        val xs = p.lines.lineVector
        Try(Xsv.parse(Xsv.XsvStrategy, xs)).toOption
      } else {
        None
      }

    private def _is_table(p: LogicalBlock) = {
      val xs = p.lines.lineVector
      if (xs.isEmpty)
        false
      else
        xs.forall(_.contains('|'))
    }

    private def _table_paragraph(p: LogicalBlock): Option[ITable] =
      if (_is_table(p)) {
        val doxconfig = Dox2Parser.Config.default // TODO
        val dox = Dox2Parser.parse(doxconfig, p)
        _table(dox)
      } else {
        None
      }

    private def _table(p: Dox): Option[Table] = Dox.findTable(p).map(_table)

    private def _table(p: DTable): Table = {
      val xs = p.toVectorMapStringVector
      val rs = RecordSequence.create(xs)
      Table.create(rs)
    }

    private def _model_datastore(p: LogicalSection): DataSet = {
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
      DataSet.create(dataname, model, matrix)
    }

    def getScript(p: LogicalSection): Option[Script] = {
      p.blocks.blocks.foldMap {
        case m: LogicalSection => _get_script(m)
        case m: LogicalParagraph => None
        case m: LogicalVerbatim => None
        case StartBlock => None
        case EndBlock => None
      }
      // import org.goldenport.sexpr._
      // val r = SExprParserNew.apply("(setq z 100)")
      // Some(Script(r))
    }

    private def _get_script(p: LogicalSection): Option[Script] = {
      val varname = p.nameForModel

      case class Z(
        properties: RichConfig = RichConfig.empty,
        xsv: Option[Xsv] = None,
        table: Option[ITable] = None
      ) {
        def r = _datastore_query |+| _datasource

        private def _datastore_query: Option[Script] =
          properties.getStringOption("data-store").map(ds =>
            properties.getStringOption("query").
              map(q => _script(s"""(setq $varname (store-select '$ds ${_sexpr_string(q)}))""")).
              getOrElse(_script(s"""(setq $varname (store-select '$ds))"""))
          )

        private def _datasource: Option[Script] =
          properties.getStringOption("data-source").map { ds =>
            val caption = properties.getStringOption("caption")
            val strategy = properties.getStringOption("strategy")
            val switches = Vector(
              caption.map(x => s":caption ${_sexpr_string(x)}"),
              strategy.map(x => s":strategy ${_sexpr_string(x)}")
            ).flatten.mkString(" ")
            _script(s"""(setq $varname (table-make $switches ${_sexpr_string(ds)}))""")
          }

        private def _script(p: String): Script = Script.parse(ctx.config, p)

        def +(rhs: LogicalBlock) = {
          _property_paragraph(rhs).map(_property) orElse
          _xsv_paragraph(rhs).map(_xsv) orElse
          _table_paragraph(rhs).map(_table) getOrElse this
        }

        private def _property(p: RichConfig) = copy(properties = properties + p)

        private def _xsv(p: Xsv) = copy(xsv = Some(p))

        private def _table(p: ITable) = copy(table = Some(p))
      }
      p.blocks.blocks./:(Z())(_+_).r
    }

    private def _sexpr_string(p: String) = SExprTextMaker.stringLiteral(p)
  }
  object Builder {
    case class Context(models: List[ISchemaModel] = Nil, config: Config) {
      def getSchemaClass(name: String): Option[ISchemaClass] = models.toStream.flatMap(_.getSchemaClass(name)).headOption

      def +(rhs: Context): Context = copy(models = models ++ rhs.models)
    }
    object Context {
      // val empty = Context()

      // implicit object ContextMonoid extends Monoid[Context] {
      //   def zero = Context.empty
      //   def append(lhs: Context, rhs: => Context) = lhs + rhs
      // }

      def apply(config: Config): Context = apply(config, Nil)

      def apply(config: Config, p: ISchemaModel, ps: ISchemaModel*): Context = apply(config, p +: ps)

      def apply(config: Config, ps: Seq[ISchemaModel]): Context = Context(ps.toList, config)
    }
  }
}
