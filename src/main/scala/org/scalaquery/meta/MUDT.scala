package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.ql.TypeMapperDelegate

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
  def getUDTs(typeNamePattern: MQName, types: Option[Seq[Int]] = None) = ResultSetInvoker[MUDT](
      _.metaData.getUDTs(typeNamePattern.catalog_?, typeNamePattern.schema_?,
                         typeNamePattern.name, types.map(_.toArray)getOrElse(null))) { r =>
      MUDT(MQName.from(r), r<<, r<<, r<<, r<<)
  }
}
