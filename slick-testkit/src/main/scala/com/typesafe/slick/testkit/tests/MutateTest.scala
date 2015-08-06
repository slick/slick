package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class MutateTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testMutate = ifCapF(jcap.mutable) {
    class Data(tag: Tag) extends Table[(Int,String)](tag, "DATA") {
      def id = column[Int]("ID", O.PrimaryKey)
      def data = column[String]("DATA")
      def * = (id, data)
    }
    val data = TableQuery[Data]

    var seenEndMarker = false
    db.run(data.schema.create >> (data ++= Seq((1, "a"), (2, "b"), (3, "c"), (4, "d")))).flatMap { _ =>
      foreach(db.stream(data.mutate.transactionally)) { m =>
        if(!m.end) {
          if(m.row._1 == 1) m.row = m.row.copy(_2 = "aa")
          else if(m.row._1 == 2) m.delete
          else if(m.row._1 == 3) m += ((5, "ee"))
        } else seenEndMarker = true
      }
    }.flatMap { _ =>
      seenEndMarker shouldBe false
      db.run(data.sortBy(_.id).result).map(_ shouldBe Seq((1, "aa"), (3, "c"), (4, "d"), (5, "ee")))
    }
  }

  def testDeleteMutate = ifCapF(jcap.mutable) {
    class T(tag: Tag) extends Table[(Int, Int)](tag, "T_DELMUTABLE") {
      def a = column[Int]("A")
      def b = column[Int]("B", O.PrimaryKey)
      def * = (a, b)
    }
    val ts = TableQuery[T]
    def tsByA = ts.findBy(_.a)

    var seenEndMarker = false
    val a = seq(
      ts.schema.create,
      ts ++= Seq((1,1), (1,2), (1,3), (1,4)),
      ts ++= Seq((2,5), (2,6), (2,7), (2,8))
    ) andThen tsByA(1).mutate(sendEndMarker = true).transactionally

    foreach(db.stream(a)){ m =>
      if(!m.end) m.delete
      else {
        seenEndMarker = true
        m += ((3,9))
      }
    }.flatMap { _ =>
      seenEndMarker shouldBe true
      db.run(ts.to[Set].result).map(_ shouldBe Set((2,5), (2,6), (2,7), (2,8), (3,9)))
    }
  }
}
