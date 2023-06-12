package com.typesafe.slick.docs

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

//#imports
import slick.jdbc.H2Profile.api.*
//#imports
import slick.jdbc.GetResult

/** A simple example that uses plain SQL queries against an in-memory
  * H2 database. The example data comes from Oracle's JDBC tutorial at
  * http://docs.oracle.com/javase/tutorial/jdbc/basics/tables.html. */
object PlainSQL extends App {
  var out = new ArrayBuffer[String]()
  def println(s: String): Unit = out += s

  //#getresult
  // Case classes for our data
  case class Supplier(id: Int, name: String, street: String, city: String, state: String, zip: String)
  case class Coffee(name: String, supID: Int, price: Double, sales: Int, total: Int)

  // Result set getters
  implicit val getSupplierResult: GetResult[Supplier] =
    GetResult { r =>
      Supplier(r.nextInt(), r.nextString(), r.nextString(), r.nextString(), r.nextString(), r.nextString())
    }
  implicit val getCoffeeResult: GetResult[Coffee] = GetResult(r => Coffee(r.<<, r.<<, r.<<, r.<<, r.<<))
  //#getresult

  val db = Database.forConfig("h2mem1")
  try {
    val f: Future[_] = {

      val a: DBIO[Unit] = DBIO.seq(
        createSuppliers,
        createCoffees,
        insertSuppliers,
        insertCoffees,
        printAll,
        printParameterized,
        coffeeByName("Colombian").map { s =>
          println(s"Coffee Colombian: $s")
        },
        deleteCoffee("Colombian").map { rows =>
          println(s"Deleted $rows rows")
        },
        coffeeByName("Colombian").map { s =>
          println(s"Coffee Colombian: $s")
        }
      )
      db.run(a)
    }
    Await.result(f, Duration.Inf)
  } finally db.close

  out.foreach(Console.out.println)

  //#sqlu
  def createCoffees: DBIO[Int] =
    sqlu"""create table coffees(
      name varchar not null,
      sup_id int not null,
      price double not null,
      sales int not null,
      total int not null,
      foreign key(sup_id) references suppliers(id))"""

  def createSuppliers: DBIO[Int] =
    sqlu"""create table suppliers(
      id int not null primary key,
      name varchar not null,
      street varchar not null,
      city varchar not null,
      state varchar not null,
      zip varchar not null)"""

  def insertSuppliers: DBIO[Unit] = DBIO.seq(
    // Insert some suppliers
    sqlu"insert into suppliers values(101, 'Acme, Inc.', '99 Market Street', 'Groundsville', 'CA', '95199')",
    sqlu"insert into suppliers values(49, 'Superior Coffee', '1 Party Place', 'Mendocino', 'CA', '95460')",
    sqlu"insert into suppliers values(150, 'The High Ground', '100 Coffee Lane', 'Meadows', 'CA', '93966')"
  )
  //#sqlu

  def insertCoffees: DBIO[Unit] = {
    //#bind
    def insert(c: Coffee): DBIO[Int] =
      sqlu"insert into coffees values (${c.name}, ${c.supID}, ${c.price}, ${c.sales}, ${c.total})"
    //#bind

    // Insert some coffees. The SQL statement is the same for all calls:
    // "insert into coffees values (?, ?, ?, ?, ?)"
    //#sequence
    val inserts: Seq[DBIO[Int]] = Seq(
      Coffee("Colombian", 101, 7.99, 0, 0),
      Coffee("French_Roast", 49, 8.99, 0, 0),
      Coffee("Espresso", 150, 9.99, 0, 0),
      Coffee("Colombian_Decaf", 101, 8.99, 0, 0),
      Coffee("French_Roast_Decaf", 49, 9.99, 0, 0)
    ).map(insert)

    val combined: DBIO[Seq[Int]] = DBIO.sequence(inserts)
    combined.map(_.sum)
    //#sequence
  }

  def printAll: DBIO[Unit] =
    // Iterate through all coffees and output them
    sql"select * from coffees".as[Coffee].map { cs =>
      println("Coffees:")
      for(c <- cs)
        println("* " + c.name + "\t" + c.supID + "\t" + c.price + "\t" + c.sales + "\t" + c.total)
    }

  def namesByPrice(price: Double): DBIO[Seq[(String, String)]] = {
    //#sql
    sql"""select c.name, s.name
          from coffees c, suppliers s
          where c.price < $price and s.id = c.sup_id""".as[(String, String)]
    //#sql
  }

  def supplierById(id: Int): DBIO[Seq[Supplier]] =
    sql"select * from suppliers where id = $id".as[Supplier]

  def printParameterized: DBIO[Unit] = {
    // Perform a join to retrieve coffee names and supplier names for
    // all coffees costing less than $9.00
    namesByPrice(9.0).flatMap { l2 =>
      println("Parameterized StaticQuery:")
      for (t <- l2)
        println("* " + t._1 + " supplied by " + t._2)
      supplierById(49).map(s => println(s"Supplier #49: $s"))
    }
  }

  def coffeeByName(name: String): DBIO[Option[Coffee]] = {
    //#literal
    val table = "coffees"
    sql"select * from #$table where name = $name".as[Coffee].headOption
    //#literal
  }

  def deleteCoffee(name: String): DBIO[Int] =
    sqlu"delete from coffees where name = $name"
}

/* Can't test this properly because reference.conf is not on the compiler class path when it
   doesn't come from a JAR:

@StaticDatabaseConfig("file:common-test-resources/application.conf#tsql")
object TypedSQL extends App {
  //#staticdatabaseconfig
  val dc = DatabaseConfig.forAnnotation[JdbcProfile]
  import dc.profile.api._
  //#staticdatabaseconfig

  //#tsql
  def getSuppliers(id: Int): DBIO[Seq[(Int, String, String, String, String, String)]] =
    tsql"select * from suppliers where id > $id"
  //#tsql

  //#staticdatabaseconfig
  val db = dc.db
  //#staticdatabaseconfig
  try {

    val a: DBIO[Unit] =
      getSuppliers(50).map { s =>
        println("All suppliers > 50:")
        s.foreach(println)
      }

    val f: Future[Unit] = db.run(a)
    Await.result(f, Duration.Inf)
  } finally db.close
}

*/
