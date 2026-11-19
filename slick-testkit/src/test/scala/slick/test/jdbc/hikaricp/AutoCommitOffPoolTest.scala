package slick.test.jdbc.hikaricp

import cats.effect.unsafe.implicits.global

import org.junit.Assert.*
import org.junit.Test
import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*

/** On a pool whose connections have auto-commit off, only what a transaction commits is kept. Writes outside
  * `transactionally` are rolled back when the connection goes back to the pool, `insertOrUpdate` included. */
class AutoCommitOffPoolTest {
  class Customers(tag: Tag) extends Table[(Int, String)](tag, "customers") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def * = (id, name)
  }
  val customers = TableQuery[Customers]

  @Test def onlyTransactionsAreCommitted(): Unit =
    Database.resource(DatabaseConfig.forProfileConfig(H2Profile, "h2mem-autocommit-off")).use { db =>
      for {
        _ <- db.run(customers.schema.create.transactionally)
        _ <- db.run((customers += ((0, "Alice"))).transactionally)
        aliceId <- db.run(customers.map(_.id).result.head)

        _ <- db.run(customers += ((0, "Bob")))
        _ <- db.run(customers.insertOrUpdate((aliceId, "Alice Smith")))
        _ <- db.run(customers.returning(customers.map(_.id)).insertOrUpdate((0, "Carol")))
        _ <- db.run(customers.result).map(assertEquals(Seq((aliceId, "Alice")), _))

        _ <- db.run((for {
          _ <- customers += ((0, "Bob"))
          _ <- customers.insertOrUpdate((aliceId, "Alice Smith"))
          _ <- customers.returning(customers.map(_.id)).insertOrUpdate((0, "Carol"))
        } yield ()).transactionally)
        _ <- db.run(customers.map(_.name).to[Set].result).map(assertEquals(Set("Alice Smith", "Bob", "Carol"), _))
      } yield ()
    }.unsafeRunSync()
}
