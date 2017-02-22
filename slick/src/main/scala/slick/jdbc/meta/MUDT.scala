package slick.jdbc.meta

import slick.jdbc.{ResultSetAction, JdbcTypesComponent}

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getUDTs(). */
case class MUDT(
  typeName: MQName, className: String, sqlType: Int, remarks: String, baseType: Option[Short]) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
  def getAttributes(attributeNamePattern: String = "%") =
    MAttribute.getAttributes(typeName, attributeNamePattern)
}

object MUDT {
  def getUDTs(typeNamePattern: MQName, types: Option[Seq[Int]] = None) = ResultSetAction[MUDT](
      _.metaData.getUDTs(typeNamePattern.catalog_?, typeNamePattern.schema_?,
                         typeNamePattern.name, types.map(_.toArray)getOrElse(null))) { r =>
      MUDT(MQName.from(r), r.<<, r.<<, r.<<, r.<<)
  }
}
