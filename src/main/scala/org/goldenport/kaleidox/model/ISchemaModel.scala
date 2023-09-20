package org.goldenport.kaleidox.model

import org.goldenport.context.Showable
import org.goldenport.record.v2.Schema

/*
 * @since   Feb. 23, 2021
 *  version Mar.  2, 2021
 *  version Jun. 25, 2021
 * @version Aug. 21, 2023
 * @author  ASAMI, Tomoharu
 */
trait ISchemaModel {
  def getSchemaClass(name: String): Option[ISchemaClass]
}

trait ISchemaClass extends Showable.Base {
  def schema: Schema

  protected def print_String: String = schema.print

  protected def display_String: String = schema.display

  protected def show_String: String = schema.show
}

case class SchemaSchemaClass(schema: Schema) extends ISchemaClass {
  val name = "Schema"
}
