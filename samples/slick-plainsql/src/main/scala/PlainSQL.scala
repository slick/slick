import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import slick.jdbc.H2Profile.api._

/** A simple example that uses plain SQL queries against an in-memory
  * H2 database. The example data comes from Oracle's JDBC tutorial at
  * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html. */
object PlainSQL extends App with Interpolation with Transfer {
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
}
