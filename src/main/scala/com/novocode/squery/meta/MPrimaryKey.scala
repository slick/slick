package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys().
 */
case class MPrimaryKey(
  table: MQName, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(table: MQName): UnitInvoker[MPrimaryKey] =
    ResultSetInvoker[MPrimaryKey](
      _.conn.getMetaData().getPrimaryKeys(table.catalog.getOrElse(null), table.schema.getOrElse(null), table.name) ) { r =>
      MPrimaryKey(MQName.from(r), r.nextString, r.nextShort, r.nextStringOption)
  }
  def getPrimaryKeys(table: String): UnitInvoker[MPrimaryKey] = getPrimaryKeys(MQName.local(table))
}
