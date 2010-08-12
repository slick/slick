package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getVersionColumns().
 */
case class MVersionColumn(
  column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], bufferLength: Int, decimalDigits: Option[Int], pseudoColumn: Option[Boolean]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MVersionColumn {
  def getVersionColumns(table: MQName): UnitInvoker[MVersionColumn] =
    ResultSetInvoker[MVersionColumn](
      _.conn.getMetaData().getVersionColumns(table.catalog.getOrElse(null), table.schema.getOrElse(null),
                                      table.name)) { r =>
      MVersionColumn(r.skip.nextString, r.nextInt, r.nextString,
        r.nextIntOption, r.nextInt, r.nextIntOption, r.nextInt match {
          case DatabaseMetaData.versionColumnPseudo => Some(true)
          case DatabaseMetaData.versionColumnNotPseudo => Some(false)
          case _ => None
        })
  }
}
