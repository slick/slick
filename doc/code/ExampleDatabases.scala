package com.typesafe.slick.docs

import com.typesafe.config.ConfigFactory

import slick.basic.BasicDatabaseConfig
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile

/** In-memory H2 databases on which the doctests run the examples. */
object ExampleDatabases {

  /** The URL of an in-memory H2 database that outlives its connections (DB_CLOSE_DELAY=-1), so that separately opened
    * Database instances see the same data until [[shutdown]] is called. */
  def h2Url(name: String): String = s"jdbc:h2:mem:$name;DB_CLOSE_DELAY=-1"

  /** A config for the database at `url` with a HikariCP connection pool. Like the pools most applications use, it
    * rejects connections once its Database is closed, so an example that keeps using a Database after closing it
    * fails. */
  def pooledH2(url: String): BasicDatabaseConfig[H2Profile] =
    DatabaseConfig.forProfileConfig(
      H2Profile,
      "",
      ConfigFactory.parseString(
        s"""db { url = "$url", driver = org.h2.Driver, connectionPool = HikariCP, numThreads = 2, maxConnections = 2 }"""
      )
    )

  /** Removes the database at `url`, so that the next run in the same JVM starts from an empty database. */
  def shutdown(url: String): Unit = {
    val connection = java.sql.DriverManager.getConnection(url)
    try connection.createStatement().execute("SHUTDOWN")
    finally connection.close()
  }
}
