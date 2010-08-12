package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getUDTs().
 */
case class MUDT(
  typeName: MQName, className: String, sqlType: Int, remarks: String, baseType: Option[Short]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
  def getAttributes(attributeNamePattern: String = "%") =
    MAttribute.getAttributes(typeName, attributeNamePattern)
}

object MUDT {
  def getUDTs(catalog: Option[String], schemaPattern: Option[String],
      typeNamePattern: String = "%", types: Option[Seq[Int]] = None): UnitInvoker[MUDT] =
    ResultSetInvoker[MUDT](
      _.conn.getMetaData().getUDTs(catalog.getOrElse(null), schemaPattern.getOrElse(null),
                                   typeNamePattern, types.map(_.toArray)getOrElse(null))) { r =>
      MUDT(MQName.from(r), r.nextString, r.nextInt, r.nextString, r.nextShortOption)
  }
}
