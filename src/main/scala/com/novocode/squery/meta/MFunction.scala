package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getFunctions().
 */
case class MFunction(name: MQName, remarks: String, returnsTable: Option[Boolean], specificName: String) {
  def getFunctionColumns(columnNamePattern: String = "%") =
    MFunctionColumn.getFunctionColumns(name, columnNamePattern)
}

object MFunction {
  def getFunctions(catalog: Option[String], schemaPattern: Option[String],
      functionNamePattern: String = "%"): UnitInvoker[MFunction] =
    ResultSetInvoker[MFunction](
      _.conn.getMetaData().getFunctions(catalog.getOrElse(null), schemaPattern.getOrElse(null),
                                        functionNamePattern)) { r =>
      MFunction(MQName.from(r), r.nextString, r.nextShort match {
          case DatabaseMetaData.functionNoTable => Some(false)
          case DatabaseMetaData.functionReturnsTable => Some(true)
          case _ => None
        }, r.nextString)
  }
}
