package slick.jdbc.meta

import slick.jdbc.ResultSetAction

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getSuperTypes(). */
case class MSuperType(typeName: MQName, superType: MQName) {
  def getSuperTypes = MSuperType.getSuperTypes(superType)
}

object MSuperType {
  def getSuperTypes(typePattern: MQName) = ResultSetAction[MSuperType](
      _.metaData.getSuperTypes(typePattern.catalog_?, typePattern.schema_?, typePattern.name) ) { r =>
      MSuperType(MQName.from(r), MQName.from(r))
  }
}
