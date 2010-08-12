package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getAttributes().
 */
case class MAttribute(typeName: MQName, attrName: String, sqlType: Int, attrTypeName: String,
  attrSize: Int, decimalDigits: Option[Int], numPrecRadic: Int, nullable: Option[Boolean],
  remarks: Option[String], attrDef: Option[String], charOctetLength: Option[Int],
  ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName], sourceSqlType: Option[Int]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
  def sourceSqlTypeName = sourceSqlType.map(TypeMapperDelegate.typeNames.get _)
}

object MAttribute {
  def getAttributes(typePattern: MQName, attributeNamePattern: String = "%"): UnitInvoker[MAttribute] =
    ResultSetInvoker[MAttribute](
      _.conn.getMetaData().getAttributes(typePattern.catalog.getOrElse(null), typePattern.schema.getOrElse(null),
                                         typePattern.name, attributeNamePattern)) { r =>
      MAttribute(MQName.from(r), r.nextString, r.nextInt, r.nextString,
        r.nextInt, r.nextIntOption, r.nextInt, r.nextInt match {
          case DatabaseMetaData.attributeNoNulls => Some(false)
          case DatabaseMetaData.attributeNullable => Some(true)
          case _ => None
        },
        r.nextStringOption, r.nextStringOption, r.skip.skip.nextIntOption,
        r.nextInt, DatabaseMeta.yesNoOpt(r), MQName.optionalFrom(r), r.nextIntOption)
  }
}
