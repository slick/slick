package slick.jdbc.meta

import java.sql._
import slick.jdbc.{ResultSetAction, JdbcTypesComponent}

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTypeInfo(). */
case class MTypeInfo(
  typeName: String, sqlType: Int, precision: Option[Int], literalPrefix: Option[String], literalSuffix: Option[String],
  createParams: Option[String], nullable: Option[Boolean], caseSensitive: Boolean, searchable: Short,
  unsignedAttribute: Boolean, fixedPrecScale: Boolean, autoIncrement: Boolean, localTypeName: Option[String],
  minScale: Short, maxScale: Short, numPrecRadix: Int) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
}

object MTypeInfo {
  def getTypeInfo = ResultSetAction[MTypeInfo](_.metaData.getTypeInfo()) { r =>
      MTypeInfo(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.nextInt match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<)
  }
}
