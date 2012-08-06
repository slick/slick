package scala.slick.examples.lifted

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
object FirstExample extends App {

//#tables
  // Definition of the SUPPLIERS table
  object Suppliers extends Table[(Int, String, String, String, String, String)]("SUPPLIERS") {
    def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
    def name = column[String]("SUP_NAME")
    def street = column[String]("STREET")
    def city = column[String]("CITY")
    def state = column[String]("STATE")
    def zip = column[String]("ZIP")
    // Every table needs a * projection with the same type as the table's type parameter
    def * = id ~ name ~ street ~ city ~ state ~ zip
  }

  // Definition of the COFFEES table
  object Coffees extends Table[(String, Int, Double, Int, Int)]("COFFEES") {
    def name = column[String]("COF_NAME", O.PrimaryKey)
    def supID = column[Int]("SUP_ID")
    def price = column[Double]("PRICE")
    def sales = column[Int]("SALES")
    def total = column[Int]("TOTAL")
    def * = name ~ supID ~ price ~ sales ~ total
    // A reified foreign key relation that can be navigated to create a join
    def supplier = foreignKey("SUP_FK", supID, Suppliers)(_.id)
  }
//#tables

  // Connect to the database and execute the following block within a session
//#setup
  Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {
    // The session is never named explicitly. It is bound to the current
    // thread as the threadLocalSession that we imported
//#setup

//#create
    // Create the tables, including primary and foreign keys
    (Suppliers.ddl ++ Coffees.ddl).create

    // Insert some suppliers
    Suppliers.insert(101, "Acme, Inc.",      "99 Market Street", "Groundsville", "CA", "95199")
    Suppliers.insert( 49, "Superior Coffee", "1 Party Place",    "Mendocino",    "CA", "95460")
    Suppliers.insert(150, "The High Ground", "100 Coffee Lane",  "Meadows",      "CA", "93966")

    // Insert some coffees (using JDBC's batch insert feature, if supported by the DB)
    Coffees.insertAll(
      ("Colombian",         101, 7.99, 0, 0),
      ("French_Roast",       49, 8.99, 0, 0),
      ("Espresso",          150, 9.99, 0, 0),
      ("Colombian_Decaf",   101, 8.99, 0, 0),
      ("French_Roast_Decaf", 49, 9.99, 0, 0)
    )
//#create

//#foreach
    // Iterate through all coffees and output them
//#foreach
    println("Coffees:")
//#foreach
    Query(Coffees) foreach { case (name, supID, price, sales, total) =>
      println("  " + name + "\t" + supID + "\t" + price + "\t" + sales + "\t" + total)
    }
//#foreach

//#projection
    // Why not let the database do the string conversion and concatenation?
//#projection
    println("Coffees (concatenated by DB):")
//#projection
    val q1 = for(c <- Coffees) // Coffees lifted automatically to a Query
      yield ConstColumn("  ") ++ c.name ++ "\t" ++ c.supID.asColumnOf[String] ++
        "\t" ++ c.price.asColumnOf[String] ++ "\t" ++ c.sales.asColumnOf[String] ++
        "\t" ++ c.total.asColumnOf[String]
    // The first string constant needs to be lifted manually to a ConstColumn
    // so that the proper ++ operator is found
    q1 foreach println
//#projection

//#join
    // Perform a join to retrieve coffee names and supplier names for
    // all coffees costing less than $9.00
//#join
    println("Manual join:")
//#join
    val q2 = for {
      c <- Coffees if c.price < 9.0
      s <- Suppliers if s.id === c.supID
    } yield (c.name, s.name)
//#join
    for(t <- q2) println("  " + t._1 + " supplied by " + t._2)

    // Do the same thing using the navigable foreign key
    println("Join by foreign key:")
//#fkjoin
    val q3 = for {
      c <- Coffees if c.price < 9.0
      s <- c.supplier
    } yield (c.name, s.name)
//#fkjoin
    // This time we read the result set into a List
    val l3: List[(String, String)] = q3.list
    for((s1, s2) <- l3) println("  " + s1 + " supplied by " + s2)

    // Check the SELECT statement for that query
    println(q3.selectStatement)

    // Compute the number of coffees by each supplier
    println("Coffees per supplier:")
    val q4 = (for {
      c <- Coffees
      s <- c.supplier
    } yield (c, s)).groupBy(_._2.id).map {
      case (_, q) => (q.map(_._2.name).min.get, q.length)
    }
    // .get is needed because Slick cannot enforce statically that
    // the supplier is always available (being a non-nullable foreign key),
    // thus wrapping it in an Option
    q4 foreach { case (name, count) =>
      println("  " + name + ": " + count)
    }
//#setup
  }
//#setup
}
