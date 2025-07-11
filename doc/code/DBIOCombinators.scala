package com.typesafe.slick.docs

import slick.jdbc.H2Profile.api._
import slick.util.LoggingContext

object DBIOCombinators extends App {
  class Coffees(tag: Tag) extends Table[(String, Double)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def price = column[Double]("PRICE")
    def * = (name, price)
  }
  val coffees = TableQuery[Coffees]
  ;{
    //#combinators1
    val ins1: DBIO[Int] = coffees += ("Colombian", 7.99)
    val ins2: DBIO[Int] = coffees += ("French_Roast", 8.99)

    val a1: DBIO[Unit] = DBIO.seq(ins1, ins2)

    val a2: DBIO[Int] = ins1 andThen ins2

    val a3: DBIO[(Int, Int)] = ins1 zip ins2

    val a4: DBIO[Vector[Int]] = DBIO.sequence(Vector(ins1, ins2))
    //#combinators1

    ()
  }
  ;{
    //#logging-context
    // Create logging context for traceability
    val userContext = LoggingContext("user" -> "barista", "operation" -> "coffee-management")
    
    // Attach context to individual actions
    val contextualInsert = (coffees += ("Espresso", 3.99)).withLoggingContext(userContext)
    
    // Use tagged convenience method for single key-value pairs
    val taggedQuery = coffees.filter(_.price < 5.0).result.tagged("query-type", "price-filter")
    
    // Context accumulates through action combinators  
    // Note: for-comprehensions on DBIOAction require an implicit ExecutionContext in real usage
    val complexAction = contextualInsert.andThen(taggedQuery).withLoggingContext(LoggingContext("component" -> "coffee-service"))
    
    // When executed, SQL statements will include context information in logs:
    // [user=barista, operation=coffee-management, component=coffee-service, phase=setup] create table "COFFEES" ...
    // [user=barista, operation=coffee-management, component=coffee-service] insert into "COFFEES" ...
    // [user=barista, operation=coffee-management, query-type=price-filter, phase=query, component=coffee-service] select ...
    //#logging-context

    ()
  }
}
