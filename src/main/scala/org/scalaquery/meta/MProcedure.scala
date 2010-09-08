package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getProcedures().
 */
case class MProcedure(name: MQName, remarks: String, returnsResult: Option[Boolean], specificName: Option[String]) {
  def getProcedureColumns(columnNamePattern: String = "%") =
    MProcedureColumn.getProcedureColumns(name, columnNamePattern)
}

object MProcedure {
  def getProcedures(namePattern: MQName) = ResultSetInvoker[MProcedure](
      _.metaData.getProcedures(namePattern.catalog_?, namePattern.schema_?, namePattern.name) ) { r =>
      MProcedure(MQName.from(r), r.skip.skip.skip<<, r.nextShort match {
          case DatabaseMetaData.procedureNoResult => Some(false)
          case DatabaseMetaData.procedureReturnsResult => Some(true)
          case _ => None
        }, r<<?)
  }
}
