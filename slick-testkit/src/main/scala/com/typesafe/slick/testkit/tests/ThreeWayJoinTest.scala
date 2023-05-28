package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, RelationalTestDB}



class ThreeWayJoinTest extends AsyncTest[RelationalTestDB] {
  import tdb.profile.api.*

  // ******************** Full many to many join test (i.e. four table) **********************
//  def testManyToManyJoin = {
//    class A(tag: Tag) extends Table[Int](tag, "a_manytomanyjoin") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def * = id
//      def bs = cs.filter(_.aId === id).flatMap(_.b)
//    }
//    lazy val as = TableQuery[A]
//    class B(tag: Tag) extends Table[(Int, Int)](tag, "b_manytomanyjoin") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def dId = column[Int]("dId")
//      def * = (id, dId)
//      def as = cs.filter(_.bId === id).flatMap(_.a)
//      def d = foreignKey("d_fk", dId, ds)(_.id)
//    }
//    lazy val bs = TableQuery[B]
//    class C(tag: Tag) extends Table[(Int, Int)](tag, "c_manytomanyjoin") {
//      def aId = column[Int]("aId")
//      def bId = column[Int]("bId")
//      def * = (aId, bId)
//      def a = foreignKey("a_fk", aId, as)(_.id)
//      def b = foreignKey("b_fk", bId, bs)(_.id)
//    }
//    lazy val cs = TableQuery[C]
//    class D(tag: Tag) extends Table[Int](tag, "d_manytomanyjoin") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def * = id
//    }
//    lazy val ds = TableQuery[D]
//
//    def q1 = for {
//      a <- as
//      b <- a.bs
//      d <- b.d
//    } yield (a, b.id, d)
//
//    def q2 = for {
//      a <- as
//      b <- a.bs.map(b => b)
//      d <- b.d
//    } yield (a, b.id, d)
//
//    def q3 = for {
//      a <- as
//      b <- a.bs.map(b => b).map(b => b)
//      d <- b.d
//    } yield (a, b.id, d)
//
//    DBIO.seq(
//      (as.schema ++ bs.schema ++ cs.schema ++ ds.schema).create,
//      as ++= Seq(1),
//      ds ++= Seq(3),
//      bs ++= Seq((2,3)),
//      cs ++= Seq((1,2)),
//      q1.result.named("q1").map(_.toSet shouldBe Set((1, 2, 3))),
//      q2.result.named("q2").map(_.toSet shouldBe Set((1, 2, 3))),
//      q3.result.named("q3").map(_.toSet shouldBe Set((1, 2, 3)))
//    )
//  }




  // ******************** Many to many join across two tables **********************
//  def testManyToManyJoinTwice = {
//    class A(tag: Tag) extends Table[Int](tag, "a_manytomanyjoin2") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def * = id
//      def bs = atbs.filter(_.aId === id).flatMap(_.b)
//    }
//    lazy val as = TableQuery[A]
//
//    class ATB(tag: Tag) extends Table[(Int, Int)](tag, "atb_manytomanyjoin2") {
//      def aId = column[Int]("aId")
//      def bId = column[Int]("bId")
//      def * = (aId, bId)
//      def a = foreignKey("a_fk2", aId, as)(_.id)
//      def b = foreignKey("b_fk2", bId, bs)(_.id)
//    }
//    lazy val atbs = TableQuery[ATB]
//
//    class B(tag: Tag) extends Table[Int](tag, "b_manytomanyjoin2") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def * = id
//      def cs = btcs.filter(_.bId === id).flatMap(_.c)
//    }
//    lazy val bs = TableQuery[B]
//
//    class BTC(tag: Tag) extends Table[(Int, Int)](tag, "btc_manytomanyjoin2") {
//      def bId = column[Int]("bId")
//      def cId = column[Int]("cId")
//      def * = (bId, cId)
//      def b = foreignKey("b_fk3", bId, bs)(_.id)
//      def c = foreignKey("c_fk3", cId, cs)(_.id)
//    }
//    lazy val btcs = TableQuery[BTC]
//
//    class C(tag: Tag) extends Table[Int](tag, "c_manytomanyjoin2") {
//      def id = column[Int]("id", O.PrimaryKey)
//      def * = id
//    }
//    lazy val cs = TableQuery[C]
//
//    def q1 = for {
//      a <- as
//      b <- a.bs
//      c <- b.cs
//    } yield (a, b.id, c)
//
//    DBIO.seq(
//      (as.schema ++ atbs.schema ++ bs.schema ++ btcs.schema ++ cs.schema).create,
//      as ++= Seq(1),
//      bs ++= Seq(2),
//      cs ++= Seq(3),
//      atbs ++= Seq((1, 2)),
//      btcs ++= Seq((2, 3)),
//      q1.result.named("q1").map(_.toSet shouldBe Set((1, 2, 3)))
//    )
//  }

}
