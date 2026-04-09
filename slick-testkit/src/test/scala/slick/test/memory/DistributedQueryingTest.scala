package slick.test.memory

import org.junit.Test
import org.junit.Assert._

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import slick.memory.{DistributedBackend, DistributedProfile}

/** Test for the DistributedProfile */
class DistributedQueryingTest {
  val dc1 = DatabaseConfig.forConfig[JdbcProfile]("distrib1")
  val dc2 = DatabaseConfig.forConfig[JdbcProfile]("distrib2")
  val dProfile = new DistributedProfile(dc1.profile, dc2.profile)

  val ts = {
    import dc1.profile.api._
    class T(tag: Tag) extends Table[(Int, Int, String)](tag, "tdb1_T") {
      def id = column[Int]("id", O.PrimaryKey)
      def a  = column[Int]("a")
      def b  = column[String]("b")
      def *  = (id, a, b)
    }
    TableQuery[T]
  }

  class U(tag: slick.lifted.Tag) extends dc2.profile.Table[(Int, Int, String)](tag, "tdb2_U") {
    import dc2.profile.api._
    def id = column[Int]("id", O.PrimaryKey)
    def a  = column[Int]("a")
    def b  = column[String]("b")
    def *  = (id, a, b)
  }
  val us = slick.lifted.TableQuery[U]

  val tData = Seq((1, 1, "a"), (2, 1, "b"), (3, 2, "c"), (4, 2, "d"), (5, 3, "e"), (6, 3, "f"))
  val uData = Seq((1, 1, "A"), (2, 1, "B"), (3, 2, "C"), (4, 2, "D"), (5, 3, "E"), (6, 3, "F"))

  @Test
  def test1: Unit = {
    val program = for {
      db1 <- dc1.profile.backend.Database.forConfig[IO]("distrib1.db")
      db2 <- dc2.profile.backend.Database.forConfig[IO]("distrib2.db")
    } yield (db1, db2)

    program.use { case (db1, db2) =>
      val distributedDb = DistributedBackend.Database(Seq(db1, db2))
      for {
        _ <- {
          import dc1.profile.api._
          db1.run(DBIO.seq(ts.schema.create, ts ++= tData))
        }
        _ <- {
          import dc2.profile.api._
          db2.run(DBIO.seq(us.schema.create, us ++= uData))
        }
        _ <- {
          import dProfile.api._
          distributedDb.run(DBIO.seq(
            ts.result.map(d => assertEquals(tData.toSet, d.toSet)),
            us.result.map(d => assertEquals(uData.toSet, d.toSet)),
            ts.flatMap(t => us.map(u => (t, u))).result.map(d => assertEquals(tData.flatMap(t => uData.map(u => (t, u))).toSet, d.toSet))
          ))
        }
      } yield ()
    }.unsafeRunSync()
  }
}
