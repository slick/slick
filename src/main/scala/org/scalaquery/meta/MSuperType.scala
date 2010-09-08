package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTypes().
 */
case class MSuperType(typeName: MQName, superType: MQName) {
  def getSuperTypes = MSuperType.getSuperTypes(superType)
}

object MSuperType {
  def getSuperTypes(typePattern: MQName) = ResultSetInvoker[MSuperType](
      _.metaData.getSuperTypes(typePattern.catalog_?, typePattern.schema_?, typePattern.name) ) { r =>
      MSuperType(MQName.from(r), MQName.from(r))
  }
}
