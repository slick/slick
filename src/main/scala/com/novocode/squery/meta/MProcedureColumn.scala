package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getProcedureColumns().
 */
case class MProcedureColumn(
  procedure: MQName, column: String, columnType: Short, sqlType: Int, typeName: String,
  precision: Option[Int], length: Int, scale: Option[Short], radix: Short,
  nullable: Option[Boolean], remarks: String, columnDef: Option[String], charOctetLength: Option[Int],
  ordinalPosition: Int, isNullable: Option[Boolean], specificName: String) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MProcedureColumn {
  def getProcedureColumns(procedurePattern: MQName, columnNamePattern: String = "%"): UnitInvoker[MProcedureColumn] =
    ResultSetInvoker[MProcedureColumn](
      _.conn.getMetaData().getProcedureColumns(procedurePattern.catalog.getOrElse(null), procedurePattern.schema.getOrElse(null),
                                               procedurePattern.name, columnNamePattern)) { r =>
      MProcedureColumn(MQName.from(r), r.nextString, r.nextShort, r.nextInt, r.nextString,
        r.nextIntOption, r.nextInt, r.nextShortOption, r.nextShort,
        r.nextShort match {
          case DatabaseMetaData.procedureNoNulls => Some(false)
          case DatabaseMetaData.procedureNullable => Some(true)
          case _ => None
        }, r.nextString, r.nextStringOption, r.skip.skip.nextIntOption,
        r.nextInt, DatabaseMeta.yesNoOpt(r), r.nextString)
  }
}
