package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTables().
 */
case class MSuperTable(table: MQName, superTable: String) {
  def getSuperTables = MSuperTable.getSuperTables(MQName(table.catalog, table.schema, superTable))
}

object MSuperTable {
  def getSuperTables(tablePattern: MQName): UnitInvoker[MSuperTable] =
    ResultSetInvoker[MSuperTable](
      _.conn.getMetaData().getSuperTables(tablePattern.catalog.getOrElse(null), tablePattern.schema.getOrElse(null),
                                          tablePattern.name)) { r =>
      MSuperTable(MQName.from(r), r.nextString)
  }
}
