package com.typesafe.slick.docs

//#strictUpsertProfile
import slick.basic.Capability
import slick.jdbc.{JdbcCapabilities, MySQLProfile}

trait StrictUpsertMySQLProfile extends MySQLProfile {
  // Use the client-side emulation (select by primary key, then update or insert)
  // instead of INSERT ... ON DUPLICATE KEY UPDATE
  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities - JdbcCapabilities.insertOrUpdate
}

object StrictUpsertMySQLProfile extends StrictUpsertMySQLProfile
//#strictUpsertProfile
