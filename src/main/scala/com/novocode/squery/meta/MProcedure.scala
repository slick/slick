package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getProcedures().
 */
case class MProcedure(name: MQName, remarks: String, returnsResult: Option[Boolean], specificName: String) {
  def getProcedureColumns(columnNamePattern: String = "%") =
    MProcedureColumn.getProcedureColumns(name, columnNamePattern)
}

object MProcedure {
  def getProcedures(catalog: Option[String], schemaPattern: Option[String],
      procedureNamePattern: String = "%"): UnitInvoker[MProcedure] =
    ResultSetInvoker[MProcedure](
      _.conn.getMetaData().getProcedures(catalog.getOrElse(null), schemaPattern.getOrElse(null),
                                         procedureNamePattern)) { r =>
      MProcedure(MQName.from(r), r.skip.skip.skip.nextString, r.nextShort match {
          case DatabaseMetaData.procedureNoResult => Some(false)
          case DatabaseMetaData.procedureReturnsResult => Some(true)
          case _ => None
        }, r.nextString)
  }
}
