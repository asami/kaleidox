package org.goldenport.kaleidox.model

import org.goldenport.record.v2.Schema

/*
 * @since   Feb. 23, 2021
 * @version Feb. 25, 2021
 * @author  ASAMI, Tomoharu
 */
trait ISchemaModel {
  def get(name: String): Option[ISchemaClass]
}

trait ISchemaClass {
  def schema: Schema
}
