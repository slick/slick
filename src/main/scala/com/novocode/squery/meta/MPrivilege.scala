package com.novocode.squery.meta

import java.sql._
import com.novocode.squery.{ResultSetInvoker, UnitInvoker}
import com.novocode.squery.combinator.TypeMapperDelegate
import com.novocode.squery.session._
import com.novocode.squery.simple.Implicit._

/**
 * A common privilege type which is used by MTablePrivilege and MColumnPrivilege.
 */
case class MPrivilege(grantor: Option[String], grantee: String, privilege: String, grantable: Option[Boolean])

object MPrivilege {
  private[meta] def from(r: PositionedResult) =
    MPrivilege(r.nextStringOption, r.nextString, r.nextString, DatabaseMeta.yesNoOpt(r))
}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getTablePrivileges().
 */
case class MTablePrivilege(table: MQName, privilege: MPrivilege)

object MTablePrivilege {
  def getTablePrivileges(tablePattern: MQName): UnitInvoker[MTablePrivilege] =
    ResultSetInvoker[MTablePrivilege](
      _.conn.getMetaData().getTablePrivileges(tablePattern.catalog.getOrElse(null), tablePattern.schema.getOrElse(null),
                                              tablePattern.name)) { r =>
      MTablePrivilege(MQName.from(r), MPrivilege.from(r))
  }
}

/**
 * A wrapper for a row in the ResultSet returned by DatabaseMetaData.getColumnPrivileges().
 */
case class MColumnPrivilege(table: MQName, column: String, privilege: MPrivilege)

object MColumnPrivilege {
  def getColumnPrivileges(tablePattern: MQName, columnPattern: String): UnitInvoker[MColumnPrivilege] =
    ResultSetInvoker[MColumnPrivilege](
      _.conn.getMetaData().getColumnPrivileges(tablePattern.catalog.getOrElse(null), tablePattern.schema.getOrElse(null),
                                               tablePattern.name, columnPattern)) { r =>
      MColumnPrivilege(MQName.from(r), r.nextString, MPrivilege.from(r))
  }
}
