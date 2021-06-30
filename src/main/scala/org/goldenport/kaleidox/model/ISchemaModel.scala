package org.goldenport.kaleidox.model

import org.goldenport.record.v2.Schema

/*
 * @since   Feb. 23, 2021
 *  version Mar.  2, 2021
 * @version Jun. 25, 2021
 * @author  ASAMI, Tomoharu
 */
trait ISchemaModel {
  def getSchemaClass(name: String): Option[ISchemaClass]
}

trait ISchemaClass {
  def schema: Schema
}

case class SchemaSchemaClass(schema: Schema) extends ISchemaClass {
}
