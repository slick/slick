package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getPrimaryKeys().
 */
case class MPrimaryKey(
  cat: Option[String], schema: Option[String], table: String, column: String, keySeq: Short, pkName: Option[String])

object MPrimaryKey {
  def getPrimaryKeys(cat: Option[String], schema: Option[String], table: String): UnitInvoker[MPrimaryKey] =
    ResultSetInvoker[MPrimaryKey](
      _.conn.getMetaData().getPrimaryKeys(cat.getOrElse(null), schema.getOrElse(null), table) ) { r =>
      MPrimaryKey(r.nextStringOption, r.nextStringOption, r.nextString, r.nextString, r.nextShort, r.nextStringOption)
  }
  def getPrimaryKeys(table: String): UnitInvoker[MPrimaryKey] = getPrimaryKeys(Some(""), Some(""), table)
}
