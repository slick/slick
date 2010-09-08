package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys().
 */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): UnitInvoker[MPrimaryKey] = ResultSetInvoker[MPrimaryKey](
      _.metaData.getPrimaryKeys(table.catalog_?, table.schema_?, table.name) ) { r =>
      MPrimaryKey(MQName.from(r), r<<, r<<, r<<)
  }
  def getPrimaryKeys(table: String): UnitInvoker[MPrimaryKey] = getPrimaryKeys(MQName.local(table))
}
