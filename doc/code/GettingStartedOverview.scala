package com.typesafe.slick.docs

import _root_.cats.effect.IO
import _root_.cats.effect.unsafe.implicits.global
import slick.cats
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile

//#imports
// Use H2Profile to connect to an H2 database

import slick.jdbc.H2Profile.api.*
//#imports

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://docs.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object GettingStartedOverview {
  def main(args: Array[String]): Unit = {
    val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:gettingStarted", driver = "org.h2.Driver", keepAliveConnection = true)

    //#quick-schema
    class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
      def name = column[String]("COF_NAME", O.PrimaryKey)
      def price = column[Double]("PRICE")
      def * = (name, price)
    }
    val coffees = TableQuery[Coffees]
    //#quick-schema

    cats.Database.resource(dc).use { db =>

      //#quick-query
      db.run(
        coffees.schema.create andThen
      //#quick-query
          ( for( c <- coffees; if c.price < 10.0 ) yield c.name ).result
      //#quick-query
            andThen
      //#quick-query
          // or
          coffees.filter(_.price < 10.0).map(_.name).result
      //#quick-query
      ).flatMap { _ =>
      //#quick-query

        db.run({
          //#what-is-slick-micro-example
          val limit = 10.0

          // Your query could look like this:
          ( for( c <- coffees; if c.price < limit ) yield c.name ).result

          // Equivalent SQL: select COF_NAME from COFFEES where PRICE < 10.0
          //#what-is-slick-micro-example
        } andThen {
          //#what-is-slick-micro-example-plainsql
          val limit = 10.0

          sql"select COF_NAME from COFFEES where PRICE < $limit".as[String]

          // Automatically using a bind variable to be safe from SQL injection:
          // select COF_NAME from COFFEES where PRICE < ?
          //#what-is-slick-micro-example-plainsql
        })
      }.flatMap { _ =>

        //#features-scala-collections
        // Query that only returns the "name" column
        // Equivalent SQL: select NAME from COFFEES
        coffees.map(_.name)

        // Query that limits results by price < 10.0
        // Equivalent SQL: select * from COFFEES where PRICE < 10.0
        coffees.filter(_.price < 10.0)
        //#features-scala-collections

        //#features-type-safe
        // The result of "select PRICE from COFFEES" is a Seq of Double
        // because of the type safe column definitions
        val coffeeNames: IO[Seq[Double]] = db.run(
          //#features-type-safe
          coffees.map(_.price).result
          //#features-type-safe
        )

        // Query builders are type safe:
        coffees.filter(_.price < 10.0)
        // Using a string in the filter would result in a compilation error
        //#features-type-safe

        //#features-composable
        // Create a query for coffee names with a price less than 10, sorted by name
        coffees.filter(_.price < 10.0).sortBy(_.name).map(_.name)
        // The generated SQL is equivalent to:
        // select name from COFFEES where PRICE < 10.0 order by NAME
        //#features-composable

        coffeeNames
      }
    }.unsafeRunSync()
  }
}
