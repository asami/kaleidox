package org.goldenport.kaleidox.model

import scalaz._, Scalaz._
import org.goldenport.record.v2.{Schema, Column}
import org.goldenport.record.v3.Record
import org.goldenport.sexpr.SSchema
import org.goldenport.collection.VectorMap
import org.goldenport.kaleidox._
import VoucherModel._

/*
 * @since   Apr. 17, 2019
 *  version Apr. 19, 2019
 * @version Jul.  7, 2019
 * @author  ASAMI, Tomoharu
 */
case class VoucherModel(
  classes: VectorMap[String, VoucherClass]
) {
  def get(name: String): Option[VoucherClass] = classes.get(name)

  def +(rhs: VoucherModel): VoucherModel = copy(classes ++ rhs.classes)

  def setup(p: Space): Space = {
    val a = classes.values.toVector.foldMap { x =>
      val path = s"model.voucher.${x.name}"
      Record.data(path -> SSchema(x.toSchema))
    }
    p.updateBindings(a)
  }
}

object VoucherModel {
  val empty = VoucherModel(VectorMap.empty[String, VoucherClass])

  implicit object VoucherModelMonoid extends Monoid[VoucherModel] {
    def zero = VoucherModel.empty
    def append(lhs: VoucherModel, rhs: => VoucherModel) = lhs + rhs
  }

  def apply(p: VoucherClass): VoucherModel = VoucherModel(VectorMap(p.name -> p))

  // TODO simplemodeler
  import org.smartdox._
  import com.asamioffice.goldenport.text.UString
  import org.goldenport.RAISE
  import org.goldenport.i18n.I18NString
  import org.goldenport.record.v2.{DataType, Multiplicity, XString, MOne}

  case class VoucherClass(
    name: String,
    slots: Vector[Slot]
  ) {
    def toSchema: Schema = {
      val columns = slots.map(_.toColumn)
      Schema(columns)
    }
  }
  object VoucherClass {
    // def apply(ps: Iterable[Slot]): VoucherClass = new VoucherClass(ps.toVector)

    def createOption(p: Section): Option[VoucherClass] = 
      p.elements.collect(_property_table).headOption.map(_to_voucher_class(p.titleName, _))

    private def _is_property_table(p: Table) = p.getCaptionName.
      map(_ == "特性一覧").getOrElse(false)

    private def _property_table: PartialFunction[Dox, Table] = {
      case m: Table if _is_property_table(m) => m
    }

    private def _to_voucher_class(pname: String, p: Table) = {
      val name = UString.capitalize(pname)
      val rs = SimpleModelerUtils.toRecords(p)
      val xs = rs.flatMap(_slot)
      VoucherClass(name, xs.toVector)
    }

    private def _slot(p: Record): Option[Slot] =
      p.getString("特性").collect {
        case "属性" => _attribute(p)
      }

    private def _attribute(p: Record) = Attribute(
      _name(p),
      _datatype(p),
      _multiplicity(p),
      _label(p)
    )

    private def _name(p: Record): String = p.getString("名前").getOrElse {
      RAISE.syntaxErrorFault("No name in table.")
    }

    private def _label(p: Record): Option[I18NString] = p.getString("ラベル").
      map(I18NString.apply)

    private def _datatype(p: Record): DataType = p.getString("型").
      flatMap(DataType.get).getOrElse(XString)

    private def _multiplicity(p: Record): Multiplicity = p.getString("多重度").
      flatMap(Multiplicity.get).getOrElse(MOne)
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
}

import org.goldenport.Strings
import org.goldenport.record.v3.{Table => _, _}
import org.smartdox._

object SimpleModelerUtils {
  def toRecords(p: Table): List[Record] = p.head.map { head =>
    val columns = head.columns
    p.body.records.map { x =>
      val fields = columns.zip(x.fields).flatMap {
        case (k, v) =>
          if (Strings.blankp(k))
            None
          else
            Some(Field(k, _to_field_value(v.contents)))
      }
      Record(fields)
    }
  }.getOrElse {
    p.body.records.map { x =>
      val fields = x.fields.zipWithIndex.map {
        case (v, k) => Field(s"${k + 1}", _to_field_value(v.contents))
      }
      Record(fields)
    }
  }

  private def _to_field_value(ps: List[Inline]): FieldValue = ps match {
    case Nil => EmptyValue
    case x :: Nil => x match {
      case m: Text => SingleValue(m.toText)
      case m => SingleValue(m)
    }
    case xs => SingleValue(Fragment(xs))
  }
}
