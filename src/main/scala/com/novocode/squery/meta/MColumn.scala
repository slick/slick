package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns().
 */
case class MColumn(
  cat: Option[String], schema: Option[String], table: String, column: String, sqlType: Int, typeName: String,
  columnSize: Int, decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPos: Int, isNullable: Option[Boolean],
  scopeCat: Option[String], scopeSchema: Option[String], scopeTable: Option[String],
  sourceDataType: Option[Int], isAutoInc: Option[Boolean])

object MColumn {
  def getColumns(cat: Option[String], schemaPattern: Option[String], tablePattern: String,
    columnPattern: String): UnitInvoker[MColumn] =
    ResultSetInvoker[MColumn](
      _.conn.getMetaData().getColumns(cat.getOrElse(null), schemaPattern.getOrElse(null),
                                      tablePattern, columnPattern)) { r =>
      MColumn(r.nextStringOption, r.nextStringOption, r.nextString, r.nextString, r.nextInt, r.nextString,
        r.nextInt, r.skip.nextIntOption, r.nextInt, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.nextStringOption,
        r.nextStringOption, r.skip.skip.nextInt, r.nextInt, r.nextString match {
          case "YES" => Some(true)
          case "NO" => Some(false)
          case _ => None
        },
        r.nextStringOption, r.nextStringOption, r.nextStringOption,
        r.nextIntOption, r.nextString match {
          case "YES" => Some(true)
          case "NO" => Some(false)
          case _ => None
        })
  }
  def getColumns(tablePattern: String, columnPattern: String): UnitInvoker[MColumn] =
    getColumns(Some(""), Some(""), tablePattern, columnPattern)
}
