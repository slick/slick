package com.typesafe.slick.examples.lifted

//#imports
// Use H2Driver to connect to an H2 database
import scala.slick.driver.H2Driver.simple._

// Use the implicit threadLocalSession
import Database.threadLocalSession
//#imports

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object GettingStartedOverview extends App {
  {
//#quick-imports
import scala.slick.driver.H2Driver.simple._
import Database.threadLocalSession
//#quick-imports

//#quick-schema
  object Coffees extends Table[(String, Double)]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def * = name ~ price
  }
//#quick-schema

//#quick-query
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
//#quick-query
    Coffees.ddl.create
//#quick-query
    ( for( c <- Coffees; if c.price < 10.0 ) yield c.name ).list
    // or
    Coffees.filter(_.price < 10.0).map(_.name).list
  }
//#quick-query
  }
}
