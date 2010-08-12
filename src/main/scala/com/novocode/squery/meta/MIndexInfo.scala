package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getIndexInfo().
 */
case class MIndexInfo(table: MQName, nonUnique: Boolean, indexQualifier: Option[String],
  indexName: Option[String], indexType: Short, ordinalPosition: Short,
  columnName: Option[String], ascending: Option[Boolean],
  cardinality: Int, pages: Int, filterCondition: Option[String])

object MIndexInfo {
  def getIndexInfo(table: MQName, unique: Boolean = false, approximate: Boolean = false): UnitInvoker[MIndexInfo] =
    ResultSetInvoker[MIndexInfo](
      _.conn.getMetaData().getIndexInfo(table.catalog.getOrElse(null), table.schema.getOrElse(null),
                                        table.name, unique, approximate)) { r =>
      MIndexInfo(MQName.from(r), r.nextBoolean, r.nextStringOption,
        r.nextStringOption, r.nextShort, r.nextShort,
        r.nextStringOption, r.nextStringOption match {
          case Some("A") => Some(true)
          case Some("D") => Some(false)
          case _ => None
        },
        r.nextInt, r.nextInt, r.nextStringOption)
  }
}
