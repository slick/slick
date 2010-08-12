package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns().
 */
case class MColumn(
  table: MQName, column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPos: Int, isNullable: Option[Boolean], scope: Option[MQName],
  sourceDataType: Option[Int], isAutoInc: Option[Boolean]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
  def getColumnPrivileges = MColumnPrivilege.getColumnPrivileges(table, column)
}

object MColumn {
  def getColumns(tablePattern: MQName, columnPattern: String): UnitInvoker[MColumn] =
    ResultSetInvoker[MColumn](
      _.conn.getMetaData().getColumns(tablePattern.catalog.getOrElse(null), tablePattern.schema.getOrElse(null),
                                      tablePattern.name, columnPattern)) { r =>
      MColumn(MQName.from(r), r.nextString, r.nextInt, r.nextString,
        r.nextIntOption, r.skip.nextIntOption, r.nextInt, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.nextStringOption,
        r.nextStringOption, r.skip.skip.nextInt, r.nextInt, DatabaseMeta.yesNoOpt(r),
        MQName.optionalFrom(r), r.nextIntOption, DatabaseMeta.yesNoOpt(r))
  }
  def getColumns(tablePattern: String, columnPattern: String): UnitInvoker[MColumn] =
    getColumns(MQName.local(tablePattern), columnPattern)
}
