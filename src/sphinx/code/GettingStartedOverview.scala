package com.typesafe.slick.examples.lifted

//#imports
// Use H2Driver to connect to an H2 database
import scala.slick.driver.H2Driver.simple._
//#imports

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object GettingStartedOverview extends App {
//#quick-imports
import scala.slick.driver.H2Driver.simple._
//#quick-imports

//#quick-schema
  class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def * = (name, price)
  }
  val coffees = TableQuery[Coffees]
//#quick-schema

//#quick-query
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    implicit session =>
//#quick-query
    coffees.ddl.create
//#quick-query
    ( for( c <- coffees; if c.price < 10.0 ) yield c.name ).list
    // or
    coffees.filter(_.price < 10.0).map(_.name).list
  }
//#quick-query

  import scala.slick.jdbc.StaticQuery.interpolation
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    implicit session =>
    //#what-is-slick-micro-example
    val limit = 10.0
    // Your query could look like this:
    ( for( c <- coffees; if c.price < limit ) yield c.name ).list
    // or this:
    sql"select name from coffees where price < $limit".as[String].list
    //#what-is-slick-micro-example
  }
}
