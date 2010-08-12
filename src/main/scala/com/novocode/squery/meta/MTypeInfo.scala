package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTypeInfo().
 */
case class MTypeInfo(
  typeName: String, sqlType: Int, precision: Option[Int], literalPrefix: Option[String], literalSuffix: Option[String],
  createParams: Option[String], nullable: Option[Boolean], caseSensitive: Boolean, searchable: Short,
  unsignedAttribute: Boolean, fixedPrecScale: Boolean, autoIncrement: Boolean, localTypeName: Option[String],
  minScale: Short, maxScale: Short, numPrecRadix: Int) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MTypeInfo {
  def getTypeInfo: UnitInvoker[MTypeInfo] =
    ResultSetInvoker[MTypeInfo](_.conn.getMetaData().getTypeInfo()) { r =>
      MTypeInfo(r.nextString, r.nextInt, r.nextIntOption, r.nextStringOption, r.nextStringOption,
        r.nextStringOption, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.nextBoolean, r.nextShort,
        r.nextBoolean, r.nextBoolean, r.nextBoolean, r.nextStringOption,
        r.nextShort, r.nextShort, r.nextInt)
  }
}
