package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTables().
 */
case class MSuperTable(table: MQName, superTable: String) {
  def getSuperTables = MSuperTable.getSuperTables(MQName(table.catalog, table.schema, superTable))
}

object MSuperTable {
  def getSuperTables(tablePattern: MQName) = ResultSetInvoker[MSuperTable](
      _.metaData.getSuperTables(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name) ) { r =>
      MSuperTable(MQName.from(r), r<<)
  }
}
