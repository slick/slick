package slick.test.memory

import org.junit.Test
import org.junit.Assert._
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile
import slick.memory.{DistributedBackend, DistributedProfile}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global

/** Test for the DistributedProfile */
class DistributedQueryingTest {
  val dc1 = DatabaseConfig.forConfig[JdbcProfile]("distrib1")
  val dc2 = DatabaseConfig.forConfig[JdbcProfile]("distrib2")
  val dProfile = new DistributedProfile(dc1.profile, dc2.profile)

  val ts = {
    import dc1.profile.api._
    class T(tag: Tag) extends Table[(Int, Int, String)](tag, "tdb1_T") {
      def id = column[Int]("id", O.PrimaryKey)
      def a = column[Int]("a")
      def b = column[String]("b")
      def * = (id, a, b)
    }
    TableQuery[T]
  }

  class U(tag: slick.lifted.Tag) extends dc2.profile.Table[(Int, Int, String)](tag, "tdb2_U") {
    import dc2.profile.api._
    def id = column[Int]("id", O.PrimaryKey)
    def a = column[Int]("a")
    def b = column[String]("b")
    def * = (id, a, b)
  }
  val us = slick.lifted.TableQuery[U]

  val tData = Seq((1, 1, "a"), (2, 1, "b"), (3, 2, "c"), (4, 2, "d"), (5, 3, "e"), (6, 3, "f"))
  val uData = Seq((1, 1, "A"), (2, 1, "B"), (3, 2, "C"), (4, 2, "D"), (5, 3, "E"), (6, 3, "F"))

  @Test
  def test1: Unit = {
    try {
      try {
        val db = DistributedBackend.Database(Seq(dc1.db, dc2.db), ExecutionContext.global)
        ;{
          import dc1.profile.api._
          Await.result(dc1.db.run(DBIO.seq(ts.schema.create, ts ++= tData)), Duration.Inf)
        };{
          import dc2.profile.api._
          Await.result(dc2.db.run(DBIO.seq(us.schema.create, us ++= uData)), Duration.Inf)
        };{
          import dProfile.api._
          Await.result(db.run(DBIO.seq(
            ts.result.map(d => assertEquals(tData.toSet, d.toSet)),
            us.result.map(d => assertEquals(uData.toSet, d.toSet)),
            ts.flatMap(t => us.map(u => (t, u))).result.map(d => assertEquals(tData.flatMap(t => uData.map(u => (t, u))).toSet, d.toSet))
          )), Duration.Inf)
        }
      } finally dc2.db.close
    } finally dc1.db.close
  }
}
