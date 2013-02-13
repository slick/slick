package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class InsertTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  override val reuseInstance = true

  def testSimple {

    class TestTable(name: String) extends Table[(Int, String)](name) {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = id ~ name
    }

    object Src1 extends TestTable("src1")
    object Dst1 extends TestTable("dst1")
    object Dst2 extends TestTable("dst2")

    (Src1.ddl ++ Dst1.ddl ++ Dst2.ddl).create

    Src1.insert(1, "A")
    Src1.insertAll((2, "B"), (3, "C"))

    Dst1.insertExpr(Src1)
    assertEquals(Set((1,"A"), (2,"B"), (3,"C")), Query(Dst1).list.toSet)

    val q2 = for(s <- Src1 if s.id <= 2) yield s
    println("Insert 2: "+Dst2.insertStatementFor(q2))
    Dst2.insert(q2)
    assertEquals(Set((1,"A"), (2,"B")), Query(Dst2).list.toSet)

    val q3 = 42 ~ "X".bind
    println("Insert 3: "+Dst2.insertStatementFor(q3))
    Dst2.insertExpr(q3)
    assertEquals(Set((1,"A"), (2,"B"), (42,"X")), Query(Dst2).list.toSet)

    val q4 = 43 ~ "Y".bind
    println("Insert 4: "+Dst2.shaped.insertStatementFor(q4))
    Dst2.shaped.insertExpr(q4)
    assertEquals(Set((1,"A"), (2,"B"), (42,"X"), (43,"Y")), Query(Dst2).list.toSet)
  }

  def testReturning {

    object A extends Table[(Int, String, String)]("A") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def s1 = column[String]("S1")
      def s2 = column[String]("S2")
      def * = id ~ s1 ~ s2
      def ins1 = s1 ~ s2 returning id
      def ins2 = (s1, s2).shaped returning (id, s1)
      def ins3 = s1 ~ s2 returning id into ((v, i) => (i, v._1, v._2))
    }

    A.ddl.create

    ifCap(jcap.returnInsertKey) {
      val id1: Int = A.ins1.insert("a", "b")
      assertEquals(1, id1)

      ifCap(jcap.returnInsertOther) {
        val id2: (Int, String) = A.ins2.insert("c", "d")
        assertEquals((2, "c"), id2)
      }
      ifNotCap(jcap.returnInsertOther) {
        val id2: Int = A.ins1.insert("c", "d")
        assertEquals(2, id2)
      }

      val ids3 = A.ins1.insertAll(("e", "f"), ("g", "h"))
      assertEquals(Seq(3, 4), ids3)

      val id4 = A.ins3.insert("i", "j")
      assertEquals((5, "i", "j"), id4)
    }
  }
}
