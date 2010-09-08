package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.ql.TypeMapperDelegate

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getBestRowIdentifier().
 */
case class MBestRowIdentifierColumn(
  scope: MBestRowIdentifierColumn.Scope, column: String, sqlType: Int, typeName: String,
  columnSize: Option[Int], decimalDigits: Option[Short], pseudoColumn: Option[Boolean]) {

  def sqlTypeName = TypeMapperDelegate.typeNames.get(sqlType)
}

object MBestRowIdentifierColumn {
  def getBestRowIdentifier(table: MQName, scope: Scope, nullable: Boolean = false) =
    ResultSetInvoker[MBestRowIdentifierColumn](
      _.metaData.getBestRowIdentifier(table.catalog_?, table.schema_?, table.name, scope.value, nullable)) { r =>
      MBestRowIdentifierColumn(Scope(r<<), r<<, r<<, r<<, r<<, r.skip<<, r.nextShort match {
          case DatabaseMetaData.bestRowNotPseudo => Some(false)
          case DatabaseMetaData.bestRowPseudo => Some(true)
          case _ => None
        })
  }

  sealed abstract class Scope(val value: Int)

  object Scope {
    final case object Temporary extends Scope(DatabaseMetaData.bestRowTemporary)
    final case object Transaction extends Scope(DatabaseMetaData.bestRowTransaction)
    final case object Session extends Scope(DatabaseMetaData.bestRowSession)
    private[MBestRowIdentifierColumn] def apply(value: Short) = value match {
      case DatabaseMetaData.bestRowTemporary => Temporary
      case DatabaseMetaData.bestRowTransaction => Transaction
      case DatabaseMetaData.bestRowSession => Session
    }
  }
}
