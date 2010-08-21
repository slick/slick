package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getFunctionColumns().
 */
case class MFunctionColumn(
  function: MQName, column: String, columnType: Short, sqlType: Int, typeName: String,
  precision: Option[Int], length: Int, scale: Option[Short], radix: Short,
  nullable: Option[Boolean], remarks: String, charOctetLength: Option[Int],
  ordinalPosition: Int, isNullable: Option[Boolean], specificName: String) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MFunctionColumn {
  private[this] val m = try { classOf[DatabaseMetaData].getMethod("getFunctionColumns",
      classOf[String], classOf[String], classOf[String], classOf[String]) }
    catch { case _:NoSuchMethodException => null }

  def getFunctionColumns(functionPattern: MQName, columnNamePattern: String = "%") = {
    /* Regular version, requires Java 1.6: 
    ResultSetInvoker[MFunctionColumn](
      _.metaData.getFunctionColumns(functionPattern.catalog_?, functionPattern.schema_?,
                                    functionPattern.name, columnNamePattern)) { r =>
      MFunctionColumn(MQName.from(r), r<<, r<<, r<<, r<<, r<<, r<<, r<<, r<<, r.nextShort match {
          case DatabaseMetaData.functionNoNulls => Some(false)
          case DatabaseMetaData.functionNullable => Some(true)
          case _ => None
        }, r<<, r<<, r<<, DatabaseMeta.yesNoOpt(r), r<<)
    }*/
    if(m == null) UnitInvoker.empty
    else ResultSetInvoker[MFunctionColumn]( s =>
      m.invoke(s.metaData, functionPattern.catalog_?, functionPattern.schema_?,
               functionPattern.name, columnNamePattern).asInstanceOf[ResultSet]) { r =>
      MFunctionColumn(MQName.from(r), r<<, r<<, r<<, r<<, r<<, r<<, r<<, r<<, r.nextShort match {
          case 0 /*DatabaseMetaData.functionNoNulls*/ => Some(false)
          case 1 /*DatabaseMetaData.functionNullable*/ => Some(true)
          case _ => None
        }, r<<, r<<, r<<, DatabaseMeta.yesNoOpt(r), r<<)
    }
  }
}
