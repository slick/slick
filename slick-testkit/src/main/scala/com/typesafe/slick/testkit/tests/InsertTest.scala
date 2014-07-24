package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class InsertTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testSimple {

    class TestTable(tag: Tag, tname: String) extends Table[(Int, String)](tag, tname) {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }

    val src1 = TableQuery(new TestTable(_, "src1_q"))
    val dst1 = TableQuery(new TestTable(_, "dst1_q"))
    val dst2 = TableQuery(new TestTable(_, "dst2_q"))
    val dst3 = TableQuery(new TestTable(_, "dst3_q"))

    (src1.ddl ++ dst1.ddl ++ dst2.ddl ++ dst3.ddl).create

    src1.insert(1, "A")
    src1.map(_.ins).insertAll((2, "B"), (3, "C"))

    dst1.insert(src1)
    assertEquals(Set((1,"A"), (2,"B"), (3,"C")), dst1.list.toSet)

    val q2 = for(s <- src1 if s.id <= 2) yield s
    println("Insert 2: "+dst2.insertStatementFor(q2))
    dst2.insert(q2)
    assertEquals(Set((1,"A"), (2,"B")), dst2.list.toSet)

    val q3 = (42, "X".bind)
    println("Insert 3: "+dst2.insertStatementFor(q3))
    dst2.insertExpr(q3)
    assertEquals(Set((1,"A"), (2,"B"), (42,"X")), dst2.list.toSet)

    val q4comp = Compiled { dst2.filter(_.id < 10) }
    val dst3comp = Compiled { dst3 }
    dst3comp.insert(q4comp)
    assertEquals(Set((1,"A"), (2,"B")), dst3comp.run.toSet)

    /*val q4 = (43, "Y".bind)
    println("Insert 4: "+Dst2.shaped.insertStatementFor(q4))
    Dst2.shaped.insertExpr(q4)
    assertEquals(Set((1,"A"), (2,"B"), (42,"X"), (43,"Y")), Query(Dst2).list.toSet)*/
  }

  def testReturning {

    class A(tag: Tag) extends Table[(Int, String, String)](tag, "A") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def s1 = column[String]("S1")
      def s2 = column[String]("S2")
      def * = (id, s1, s2)
    }
    val as = TableQuery[A]
    def ins1 = as.map(a => (a.s1, a.s2)) returning as.map(_.id)
    def ins2 = as.map(a => (a.s1, a.s2)) returning as.map(a => (a.id, a.s1))
    def ins3 = as.map(a => (a.s1, a.s2)) returning as.map(_.id) into ((v, i) => (i, v._1, v._2))
    def ins4 = as.map(a => (a.s1, a.s2)) returning as.map(a => a)

    as.ddl.create

    ifCap(jcap.returnInsertKey) {
      val id1: Int = ins1.insert("a", "b")
      assertEquals(1, id1)

      ifCap(jcap.returnInsertOther) {
        val id2: (Int, String) = ins2.insert("c", "d")
        assertEquals((2, "c"), id2)
      }
      ifNotCap(jcap.returnInsertOther) {
        val id2: Int = ins1.insert("c", "d")
        assertEquals(2, id2)
      }

      val ids3 = ins1.insertAll(("e", "f"), ("g", "h"))
      assertEquals(Seq(3, 4), ids3)

      val id4 = ins3.insert("i", "j")
      assertEquals((5, "i", "j"), id4)

      ifCap(jcap.returnInsertOther) {
        val id5: (Int, String, String) = ins4.insert("k", "l")
        assertEquals((6, "k", "l"), id5)
      }
    }
  }

  def testForced = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_forced") {
      def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    ts.ddl.create

    ts.insert(101, "A")
    ts.map(_.ins).insertAll((102, "B"), (103, "C"))
    assertEquals(0, ts.filter(_.id > 100).length.run)

    ifCap(jcap.forceInsert) {
      ts.forceInsert(104, "A")
      ts.map(_.ins).forceInsertAll((105, "B"), (106, "C"))
      assertEquals(3, ts.filter(_.id > 100).length.run)
    }
  }

  def testInsertOrUpdatePlain {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    ts.ddl.create

    ts ++= Seq((1, "a"), (2, "b"))
    assertEquals(1, ts.insertOrUpdate((3, "c")))
    assertEquals(1, ts.insertOrUpdate((1, "d")))
    assertEquals(Seq((1, "d"), (2, "b"), (3, "c")), ts.sortBy(_.id).run)
  }

  def testInsertOrUpdateAutoInc {
    class T(tag: Tag) extends Table[(Int, String)](tag, "T_MERGE2") {
      def id = column[Int]("ID", O.AutoInc, O.PrimaryKey)
      def name = column[String]("NAME")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    ts.ddl.create

    ts ++= Seq((1, "a"), (2, "b"))
    assertEquals(1, ts.insertOrUpdate((0, "c")))
    assertEquals(1, ts.insertOrUpdate((1, "d")))
    assertEquals(Seq((1, "d"), (2, "b"), (3, "c")), ts.sortBy(_.id).run)

    ifCap(jcap.returnInsertKey) {
      val q = ts returning ts.map(_.id)
      assertEquals(Some(4), q.insertOrUpdate((0, "e")))
      assertEquals(None, q.insertOrUpdate((1, "f")))
      assertEquals(Seq((1, "f"), (2, "b"), (3, "c"), (4, "e")), ts.sortBy(_.id).run)
    }
  }
}
