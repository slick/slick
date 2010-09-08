package org.scalaquery.meta

import java.sql._
import org.scalaquery.{ResultSetInvoker, UnitInvoker}
import org.scalaquery.session._

/**
 * A common privilege type which is used by MTablePrivilege and MColumnPrivilege.
 */
case class MPrivilege(grantor: Option[String], grantee: String, privilege: String, grantable: Option[Boolean])

object MPrivilege {
  private[meta] def from(r: PositionedResult) = MPrivilege(r<<, r<<, r<<, DatabaseMeta.yesNoOpt(r))
}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTablePrivileges().
 */
case class MTablePrivilege(table: MQName, privilege: MPrivilege)

object MTablePrivilege {
  def getTablePrivileges(tablePattern: MQName) = ResultSetInvoker[MTablePrivilege](
      _.metaData.getTablePrivileges(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name)) { r =>
      MTablePrivilege(MQName.from(r), MPrivilege.from(r))
  }
}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumnPrivileges().
 */
case class MColumnPrivilege(table: MQName, column: String, privilege: MPrivilege)

object MColumnPrivilege {
  def getColumnPrivileges(tablePattern: MQName, columnPattern: String) = ResultSetInvoker[MColumnPrivilege](
      _.metaData.getColumnPrivileges(tablePattern.catalog_?, tablePattern.schema_?, tablePattern.name, columnPattern)) { r =>
      MColumnPrivilege(MQName.from(r), r<<, MPrivilege.from(r))
  }
}
