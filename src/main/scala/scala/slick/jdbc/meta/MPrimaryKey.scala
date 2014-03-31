package scala.slick.jdbc.meta

import scala.slick.jdbc.{ResultSetInvoker, Invoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys().
 */
case class MPrimaryKey(table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): Invoker[MPrimaryKey] = ResultSetInvoker[MPrimaryKey](
      _.metaData.getPrimaryKeys(table.catalog_?, table.schema_?, table.name) ) { r =>
      MPrimaryKey(MQName.from(r), r.<<, r.<<, r.<<)
  }
  def getPrimaryKeys(table: String): Invoker[MPrimaryKey] = getPrimaryKeys(MQName.local(table))
}
