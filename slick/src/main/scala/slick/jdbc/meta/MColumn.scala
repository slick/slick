package slick.jdbc.meta

import java.sql._

import slick.jdbc.{ResultSetAction, JdbcTypesComponent}
import slick.jdbc.meta.MColumn.MColumnExt

/** A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumns(). */
case class MColumn(
  table: MQName, name: String, sqlType: Int, typeName: String,
  size: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
  columnDef: Option[String], charOctetLength: Int, ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName],
  sourceDataType: Option[Any], isAutoInc: Option[Boolean]) {

  def isGenerated: Option[Boolean] = {
    this match {
      case ext: MColumnExt => ext.isGen
      case _ => None
    }
  }

  def sqlTypeName = JdbcTypesComponent.typeNames.get(sqlType)
  def getColumnPrivileges = MColumnPrivilege.getColumnPrivileges(table, name)
}

object MColumn {

  /** An extended wrapper, it is introduced to keep binary compatibility while adding isGenerated property.
   * It is reasonable to make isGenerated a property of MColumn at the binary-incompatible release.
   * */
  private[meta] class MColumnExt(
    table: MQName, name: String, sqlType: Int, typeName: String,
    size: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
    columnDef: Option[String], charOctetLength: Int, ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName],
    sourceDataType: Option[Any], isAutoInc: Option[Boolean], val isGen: Option[Boolean]
  ) extends MColumn(table: MQName, name: String, sqlType: Int, typeName: String,
    size: Option[Int], decimalDigits: Option[Int], numPrecRadix: Int, nullable: Option[Boolean], remarks: Option[String],
    columnDef: Option[String], charOctetLength: Int, ordinalPosition: Int, isNullable: Option[Boolean], scope: Option[MQName],
    sourceDataType: Option[Any], isAutoInc: Option[Boolean]) {}

  def getColumns(tablePattern: MQName, columnPattern: String) = ResultSetAction[MColumn](
      _.metaData.getColumns(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name, columnPattern)) { r =>
      new MColumnExt(MQName.from(r), r.<<, r.<<, r.<<, r.<<, r.skip.<<, r.<<, r.nextInt() match {
          case DatabaseMetaData.columnNoNulls => Some(false)
          case DatabaseMetaData.columnNullable => Some(true)
          case _ => None
        }, r.<<, r.<<, r.skip.skip.<<, r.<<, DatabaseMeta.yesNoOpt(r),
        if(r.hasMoreColumns) MQName.optionalFrom(r) else None,
        if(r.hasMoreColumns) r.nextObjectOption() else None,
        if(r.hasMoreColumns) DatabaseMeta.yesNoOpt(r) else None,
        if(r.hasMoreColumns) DatabaseMeta.yesNoOpt(r) else None
      )
  }

}
