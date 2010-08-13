package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getFunctions().
 */
case class MFunction(name: MQName, remarks: String, returnsTable: Option[Boolean], specificName: String) {
  def getFunctionColumns(columnNamePattern: String = "%") =
    MFunctionColumn.getFunctionColumns(name, columnNamePattern)
}

object MFunction {
  def getFunctions(namePattern: MQName) = ResultSetInvoker[MFunction](
      _.metaData.getFunctions(namePattern.catalog_?, namePattern.schema_?, namePattern.name)) { r =>
      MFunction(MQName.from(r), r<<, r.nextShort match {
          case DatabaseMetaData.functionNoTable => Some(false)
          case DatabaseMetaData.functionReturnsTable => Some(true)
          case _ => None
        }, r<<)
  }
}
