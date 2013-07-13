package scala.slick.test.memory

import org.junit.Test
import org.junit.Assert._
import scala.slick.testutil.TestDBs
import scala.slick.memory.{DistributedDriver, DistributedBackend}

/** Test for the DistributedDriver */
class DistributedQueryingTest {
  val tdb1 = TestDBs.H2Mem
  val tdb2 = TestDBs.DerbyMem
  val dProfile = new DistributedDriver(tdb1.driver, tdb2.driver).profile

  val ts = {
    import tdb1.profile.simple._
    class T(tag: Tag) extends Table[(Int, Int, String)](tag, "tdb1_T") {
      def id = column[Int]("id", O.PrimaryKey)
      def a = column[Int]("a")
      def b = column[String]("b")
      def * = (id, a, b)
    }
    TableQuery[T]
  }

  class U(tag: scala.slick.lifted.Tag) extends tdb2.profile.Table[(Int, Int, String)](tag, "tdb2_U") {
    import tdb2.profile.simple._
    def id = column[Int]("id", O.PrimaryKey)
    def a = column[Int]("a")
    def b = column[String]("b")
    def * = (id, a, b)
  }
  val us = scala.slick.lifted.TableQuery[U]

  val tData = Seq((1, 1, "a"), (2, 1, "b"), (3, 2, "c"), (4, 2, "d"), (5, 3, "e"), (6, 3, "f"))
  val uData = Seq((1, 1, "A"), (2, 1, "B"), (3, 2, "C"), (4, 2, "D"), (5, 3, "E"), (6, 3, "F"))

  def runTest[T](f: (tdb1.profile.Backend#Session, tdb2.profile.Backend#Session, DistributedBackend#Session) => T) {
    tdb1.cleanUpBefore()
    try {
      val db1 = tdb1.createDB()
      tdb2.cleanUpBefore()
      try {
        val db2 = tdb2.createDB()
        val db = DistributedBackend.Database(db1, db2)
        db.withSession { s =>
          f(s.sessions(0).asInstanceOf[tdb1.profile.Backend#Session], s.sessions(1).asInstanceOf[tdb2.profile.Backend#Session], s)
        }
      } finally tdb2.cleanUpAfter()
    } finally tdb1.cleanUpAfter()
  }

  @Test
  def test1 = runTest { (s1, s2, sDist) =>
    {
      import tdb1.profile.simple._
      implicit val session = s1
      ts.ddl.create
      ts ++= tData
      assertEquals(tData.toSet, ts.run.toSet)
    }

    {
      import tdb2.profile.simple._
      implicit val session = s2
      us.ddl.create
      us ++= uData
      assertEquals(uData.toSet, us.run.toSet)
    }

    {
      import dProfile.simple._
      implicit val session = sDist
      assertEquals(tData.toSet, ts.run.toSet)
      assertEquals(uData.toSet, us.run.toSet)
      assertEquals(
        tData.flatMap(t => uData.map(u => (t, u))).toSet,
        ts.flatMap(t => us.map(u => (t, u))).run.toSet
      )
    }
  }
}
