package com.typesafe.slick.examples.lifted

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration

//#imports
// Use H2Driver to connect to an H2 database
import scala.slick.driver.H2Driver.api._
//#imports

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object GettingStartedOverview extends App {
//#quick-imports
import scala.slick.driver.H2Driver.api._
//#quick-imports

//#quick-schema
  class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def * = (name, price)
  }
  val coffees = TableQuery[Coffees]
//#quick-schema

  val f1 =
//#quick-query
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver").run(
//#quick-query
    coffees.schema.create andThen
//#quick-query
    ( for( c <- coffees; if c.price < 10.0 ) yield c.name ).result
//#quick-query
    andThen
//#quick-query
    // or
    coffees.filter(_.price < 10.0).map(_.name).result
  )
//#quick-query
  Await.result(f1, Duration.Inf)

  val f2 = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver").run(
    coffees.schema.create andThen {
      //#what-is-slick-micro-example
      val limit = 10.0

      // Your query could look like this:
      ( for( c <- coffees; if c.price < limit ) yield c.name ).result
      //#what-is-slick-micro-example
    } andThen {
      val limit = 10.0
      //#what-is-slick-micro-example

      // Or using Plain SQL String Interpolation:
      sql"select COF_NAME from COFFEES where PRICE < $limit".as[String]

      // Both queries result in SQL equivalent to:
      // select COF_NAME from COFFEES where PRICE < 10.0
      //#what-is-slick-micro-example
    }
  )
  Await.result(f2, Duration.Inf)

  //#features-scala-collections
  // Query that only returns the "name" column
  coffees.map(_.name)

  // Query that does a "where price < 10.0"
  coffees.filter(_.price < 10.0)
  //#features-scala-collections

  {
    val db = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")
    val f4 = {
      //#features-type-safe
      // The result of "select PRICE from COFFEES" is a Seq of Double
      // because of the type safe column definitions
      val coffeeNames: Future[Seq[Double]] = db.run(
        //#features-type-safe
        coffees.schema.create andThen
        //#features-type-safe
        coffees.map(_.price).result
      )

      // Query builders are type safe:
      coffees.filter(_.price < 10.0)
      // Using a string in the filter would result in a compilation error
      //#features-type-safe
      coffeeNames
    }
    Await.result(f4, Duration.Inf)
  }

  //#features-composable
  // Create a query for coffee names with a price less than 10, sorted by name
  coffees.filter(_.price < 10.0).sortBy(_.name).map(_.name)
  // The generated SQL is equivalent to:
  // select name from COFFEES where PRICE < 10.0 order by NAME
  //#features-composable
}
