package com.typesafe.slick.docs

//#strictUpsertProfile
import slick.basic.Capability
import slick.jdbc.{JdbcCapabilities, MySQLProfile}

trait StrictUpsertMySQLProfile extends MySQLProfile {
  // Use the client-side emulation (select by primary key, then update or insert)
  // instead of INSERT ... ON DUPLICATE KEY UPDATE
  trait StrictUpsertMySQLAPI extends MySQLAPI {
    override val insertOrUpdate = deregister(JdbcCapabilities.insertOrUpdate)
  }
  override val api: StrictUpsertMySQLAPI = new StrictUpsertMySQLAPI{}
}

object StrictUpsertMySQLProfile extends StrictUpsertMySQLProfile
//#strictUpsertProfile
