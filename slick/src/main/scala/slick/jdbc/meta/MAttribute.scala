package slick.jdbc.meta

import java.sql._
import slick.jdbc.{ResultSetAction, JdbcTypesComponent}

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getAttributes(). */
case class MAttribute(typeName: MQName, attrName: String, sqlType: Int, attrTypeName: String,
  attrSize: Int, decimalDigits: Option[Int], numPrecRadic: Int, nullable: Option[Boolean],
  remarks: Option[String], attrDef: Option[String], charOctetLength: Option[Int],
  ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName], sourceSqlType: Option[Int]) {

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
  def sourceSqlTypeName = sourceSqlType.map(JdbcTypesComponent.typeNames.get _)
}

object MAttribute {
  def getAttributes(typePattern: MQName, attributeNamePattern: String = "%") = ResultSetAction[MAttribute](
      _.metaData.getAttributes(typePattern.catalog_?, typePattern.schema_?, typePattern.name, attributeNamePattern)) { r =>
      MAttribute(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.nextInt match {
          case DatabaseMetaData.attributeNoNulls => Some(false)
          case DatabaseMetaData.attributeNullable => Some(true)
          case _ => None
        }, r.<<, r.<<, r.skip.skip.<<, r.<<, DatabaseMeta.yesNoOpt(r), MQName.optionalFrom(r), r.<<)
  }
}
