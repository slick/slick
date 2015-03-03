package com.typesafe.slick.docs

//#imports
// Use H2Driver to connect to an H2 database
import slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration
//#imports

import scala.collection.mutable.ArrayBuffer

/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object FirstExample extends App {
  val lines = new ArrayBuffer[Any]()
  def println(s: Any) = lines += s

//#tables
  // Definition of the SUPPLIERS table
  class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = (id, name, street, city, state, zip)
  }
  val suppliers = TableQuery[Suppliers]

  // Definition of the COFFEES table
  class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = (name, supID, price, sales, total)
    // A reified foreign key relation that can be navigated to create a join
    def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
  }
  val coffees = TableQuery[Coffees]
//#tables

  // Connect to the database and execute the following block within a session
//#setup
  val db = Database.forConfig("h2mem1")
  try {
    // ...
//#setup

//#create
    val setup = DBIO.seq(
      // Create the tables, including primary and foreign keys
      (suppliers.schema ++ coffees.schema).create,

      // Insert some suppliers
      suppliers += (101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199"),
      suppliers += ( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460"),
      suppliers += (150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966"),

      // Insert some coffees (using JDBC's batch insert feature, if supported by the DB)
      coffees ++= Seq(
        ("Colombian",         101, 7.99, 0, 0),
        ("French_Roast",       49, 8.99, 0, 0),
        ("Espresso",          150, 9.99, 0, 0),
        ("Colombian_Decaf",   101, 8.99, 0, 0),
        ("French_Roast_Decaf", 49, 9.99, 0, 0)
      )
    )

    Await.result(db.run(setup), Duration.Inf)
//#create

//#readall
    // Read all coffees and print them to the console
    println("Coffees:")
    Await.result(db.run(coffees.result), Duration.Inf).foreach {
      case (name, supID, price, sales, total) =>
        println("  " + name + "\t" + supID + "\t" + price + "\t" + sales + "\t" + total)
    }
//#readall

//#projection
    // Why not let the database do the string conversion and concatenation?
//#projection
    println("Coffees (concatenated by DB):")
//#projection
    val q1 = for(c <- coffees)
      yield LiteralColumn("  ") ++ c.name ++ "\t" ++ c.supID.asColumnOf[String] ++
        "\t" ++ c.price.asColumnOf[String] ++ "\t" ++ c.sales.asColumnOf[String] ++
        "\t" ++ c.total.asColumnOf[String]
    // The first string constant needs to be lifted manually to a LiteralColumn
    // so that the proper ++ operator is found

    val f = db.stream(q1.result).foreach(println)
    Await.result(f, Duration.Inf)
//#projection

//#join
    // Perform a join to retrieve coffee names and supplier names for
    // all coffees costing less than $9.00
//#join
    println("Manual join:")
//#join
    val q2 = for {
      c <- coffees if c.price < 9.0
      s <- suppliers if s.id === c.supID
    } yield (c.name, s.name)
//#join
    Await.result(db.run(q2.result), Duration.Inf).foreach(t =>
      println("  " + t._1 + " supplied by " + t._2)
    )

    // Do the same thing using the navigable foreign key
    println("Join by foreign key:")
//#fkjoin
    val q3 = for {
      c <- coffees if c.price < 9.0
      s <- c.supplier
    } yield (c.name, s.name)
//#fkjoin
    Await.result(db.run(q3.result), Duration.Inf).foreach { case (s1, s2) => println("  " + s1 + " supplied by " + s2) }

    lines.foreach(Predef.println _)
//#setup
  } finally db.close
//#setup
}
