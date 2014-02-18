package com.typesafe.slick.examples.jdbc

//#imports
import scala.slick.driver.JdbcDriver.backend.Database
import Database.dynamicSession
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
//#imports
//#imports.interpolation
import Q.interpolation
//#imports.interpolation

/**
 * A simple example that uses plain SQL queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
object PlainSQL extends App {

  // Case classes for our data
//#setup
  case class Supplier(id:Int, name:String, street:String, city:String, state:String, zip:String)
  case class Coffee(name:String, supID:Int, price:Double, sales:Int, total:Int)

//#setup
//#GetResult
  // Result set getters
  implicit val getSupplierResult = GetResult(r => Supplier(r.nextInt, r.nextString, r.nextString,
    r.nextString, r.nextString, r.nextString))
  implicit val getCoffeeResult = GetResult(r => Coffee(r.<<, r.<<, r.<<, r.<<, r.<<))
//#GetResult

  //#setup
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withDynSession {

//#setup
//#updateNA
    // Create the tables, including primary and foreign keys
    Q.updateNA("create table suppliers("+
      "id int not null primary key, "+
      "name varchar not null, "+
      "street varchar not null, "+
      "city varchar not null, "+
      "state varchar not null, "+
      "zip varchar not null)").execute
    Q.updateNA("create table coffees("+
      "name varchar not null, "+
      "sup_id int not null, "+
      "price double not null, "+
      "sales int not null, "+
      "total int not null, "+
      "foreign key(sup_id) references suppliers(id))").execute
//#updateNA

//#Q.u
    // Insert some suppliers
    (Q.u + "insert into suppliers values(101, 'Acme, Inc.', '99 Market Street', 'Groundsville', 'CA', '95199')").execute
    (Q.u + "insert into suppliers values(49, 'Superior Coffee', '1 Party Place', 'Mendocino', 'CA', '95460')").execute
    (Q.u + "insert into suppliers values(150, 'The High Ground', '100 Coffee Lane', 'Meadows', 'CA', '93966')").execute
//#Q.u

//#bindConcat
    def insert(c: Coffee) = (Q.u + "insert into coffees values (" +? c.name +
      "," +? c.supID + "," +? c.price + "," +? c.sales + "," +? c.total + ")").execute

    // Insert some coffees
    Seq(
      Coffee("Colombian", 101, 7.99, 0, 0),
      Coffee("French_Roast", 49, 8.99, 0, 0),
      Coffee("Espresso", 150, 9.99, 0, 0),
      Coffee("Colombian_Decaf", 101, 8.99, 0, 0),
      Coffee("French_Roast_Decaf", 49, 9.99, 0, 0)
    ).foreach(insert)
//#bindConcat

    // Iterate through all coffees and output them
    println("Coffees:")
//#queryNA
    Q.queryNA[Coffee]("select * from coffees") foreach { c =>
      println("  " + c.name + "\t" + c.supID + "\t" + c.price + "\t" + c.sales + "\t" + c.total)
    }
//#queryNA

//#query
    // Perform a join to retrieve coffee names and supplier names for
    // all coffees costing less than $9.00
//#query
    println("Manual join:")
//#query
    val q2 = Q.query[Double, (String, String)]("""
      select c.name, s.name
      from coffees c, suppliers s
      where c.price < ? and s.id = c.sup_id
    """)
    // This time we read the result set into a List
    val l2 = q2(9.0).list
    for (t <- l2) println("  " + t._1 + " supplied by " + t._2)
//#query

    // Append to a StaticQuery
//#applyQuery
    val supplierById = Q[Int, Supplier] + "select * from suppliers where id = ?"
    println("Supplier #49: " + supplierById(49).first)
//#applyQuery

//#interpolate.sql
    def coffeeByName(name: String) = sql"select * from coffees where name = $name".as[Coffee]
    println("Coffee Colombian: " + coffeeByName("Colombian").firstOption)
//#interpolate.sql

//#interpolate.sqlu
    def deleteCoffee(name: String) = sqlu"delete from coffees where name = $name".first
    val rows = deleteCoffee("Colombian")
    println(s"Deleted $rows rows")
//#interpolate.sqlu
    println("Coffee Colombian: " + coffeeByName("Colombian").firstOption)
//#setup
  }
//#setup
}
