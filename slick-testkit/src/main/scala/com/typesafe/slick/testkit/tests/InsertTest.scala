package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}
import slick.jdbc.DerbyProfile

class InsertTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  def testSimple = {
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

    val q2 = for(s <- src1 if s.id <= 2) yield s
    println("Insert 2: "+dst2.forceInsertStatementFor(q2))
    val q3 = (42, "X".bind)
    println("Insert 3: "+dst2.forceInsertStatementFor(q3))
    val q4comp = Compiled { dst2.filter(_.id < 10) }
    val dst3comp = Compiled { dst3 }

    DBIO.sequence(Seq(
      (src1.schema ++ dst1.schema ++ dst2.schema ++ dst3.schema).create,
      src1 += (1, "A"),
      src1.map(_.ins) ++= Seq((2, "B"), (3, "C")),
      dst1.forceInsertQuery(src1),
      dst1.to[Set].result.map(_ shouldBe Set((1,"A"), (2,"B"), (3,"C"))),
      dst2.forceInsertQuery(q2),
      dst2.to[Set].result.map(_ shouldBe Set((1,"A"), (2,"B"))),
      dst2.forceInsertExpr(q3),
      dst2.to[Set].result.map(_ shouldBe Set((1,"A"), (2,"B"), (42,"X"))),
      dst3comp.forceInsertQuery(q4comp),
      dst3comp.result.map(v => v.to[Set] shouldBe Set((1,"A"), (2,"B")))
    ))
  }

  def testEmptyInsert = {
    class A(tag: Tag) extends Table[Int](tag, "A_EMPTYINSERT") {
      def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
      def * = id
    }
    val as = TableQuery[A]

    DBIO.seq(
      as.schema.create,
      as += 42,
      as.result.map(_ shouldBe Seq(1))
    )
  }

  
  def testUniqueInsert = {
    case class ARow(email: String , id: Int = 0)
    class A(tag: Tag) extends Table[ARow](tag , "A_UNIQUEINSERT"){
      def id = column[Int]("id" , O.AutoInc , O.PrimaryKey)
      def email = column[String]("email" , O.Unique , O.Length(254))

      def * = (email , id)<>(ARow.tupled , ARow.unapply )
    }
    val atq = TableQuery[A]

    import scala.util.{Success, Failure}
    DBIO.seq(
      atq.schema.create,
      atq ++= Seq( ARow("unique@site.com") , ARow("user@site.com") ),
      ( atq += ARow("unique@site.com") ).asTry.map{
        case Failure(e:java.sql.SQLException) if e.getMessage.toLowerCase.contains("unique") => ()
        case Failure(e:java.sql.BatchUpdateException) if e.getMessage.toLowerCase.contains("unique") => ()
        case Failure( e ) => throw e
        case Success(_) => throw new Exception("Should have failed with UNIQUE constraint violation")
      },
      atq.result.map( _.size shouldBe 2 )
    )
  }

  def testReturning = ifCap(jcap.returnInsertKey) {
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

    (for {
      _ <- as.schema.create
      _ <- (ins1 += ("a", "b")) map { id1: Int => id1 shouldBe 1 }
      _ <- ifCap(jcap.returnInsertOther) {
        (ins2 += ("c", "d")) map { id2: (Int, String) => id2 shouldBe (2, "c") }
      }
      _ <- ifNotCap(jcap.returnInsertOther) {
        (ins1 += ("c", "d")) map { id2: Int => id2 shouldBe 2 }
      }
      _ <- (ins1 ++= Seq(("e", "f"), ("g", "h"))) map (_ shouldBe Seq(3, 4))
      _ <- (ins3 += ("i", "j")) map (_ shouldBe (5, "i", "j"))
      _ <- ifCap(jcap.returnInsertOther) {
        (ins4 += ("k", "l")) map { id5: (Int, String, String) => id5 shouldBe (6, "k", "l") }
      }
    } yield ()).withPinnedSession // Some database servers (e.g. DB2) preallocate ID blocks per session
  }

  def testForced = {
    class T(tname: String)(tag: Tag) extends Table[(Int, String, Int, Boolean, String, String, Int)](tag, tname) {
      def id = column[Int]("id", O.AutoInc, O.PrimaryKey)
      def name = column[String]("name")
      def i1 = column[Int]("i1")
      def b = column[Boolean]("b")
      def s1 = column[String]("s1", O.Length(10,varying=true))
      def s2 = column[String]("s2", O.Length(10,varying=true))
      def i2 = column[Int]("i2")

      def * = (id, name, i1, b, s1, s2, i2)
      def ins = (id, name, i1, b, s1, s2, i2)
    }
    val ts = TableQuery(new T("t_forced")(_))
    val src = TableQuery(new T("src_forced")(_))

    seq(
      (ts.schema ++ src.schema).create,
      ts += (101, "A", 1, false, "S1", "S2", 0),
      ts.map(_.ins) ++= Seq((102, "B", 1, false, "S1", "S2", 0), (103, "C", 1, false, "S1", "S2", 0)),
      ts.filter(_.id > 100).length.result.map(_ shouldBe 0),
      ifCap(jcap.forceInsert)(seq(
        ts.forceInsert(104, "A", 1, false, "S1", "S2", 0),
        ts.map(_.ins).forceInsertAll(Seq((105, "B", 1, false, "S1", "S2", 0), (106, "C", 1, false, "S1", "S2", 0))),
        ts.filter(_.id > 100).length.result.map(_ shouldBe 3),
        ts.map(_.ins).forceInsertAll(Seq((111, "D", 1, false, "S1", "S2", 0))),
        ts.filter(_.id > 100).length.result.map(_ shouldBe 4),
        src.forceInsert(90, "X", 1, false, "S1", "S2", 0),
        mark("forceInsertQuery", ts.forceInsertQuery(src)).map(_ shouldBe 1),
        ts.filter(_.id.between(90, 99)).result.headOption.map(_ shouldBe Some((90, "X", 1, false, "S1", "S2", 0)))
      ))
    )
  }

  def testInsertOrUpdatePlain = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((1, "a"), (2, "b"))
      _ <- ts.insertOrUpdate((3, "c")).map(_ shouldBe 1)
      _ <- ts.insertOrUpdate((1, "d")).map(_ shouldBe 1)
      _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "d"), (2, "b"), (3, "c")))
    } yield ()
  }

  def testInsertOrUpdateNoPK = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge_no_pk") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    val failed = try {
      ts.insertOrUpdate((3, "c"))
      false
    }
    catch {
      case _: SlickException => true
    }
    if (!failed) throw new RuntimeException("Should fail since insertOrUpdate is not supported on a table without PK.")
    DBIO.seq()
  }

  def testInsertOrUpdatePlainWithFuncDefinedPK: DBIOAction[Unit, _, _] = {
    //FIXME remove this after fixed checkInsert issue
    if (tdb.profile.isInstanceOf[DerbyProfile]) return DBIO.successful(())

    class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge3") {
      def id = column[Int]("id")
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
      def pk = primaryKey("t_merge_pk_a", id)
    }
    val ts = TableQuery[T]

    for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((1, "a"), (2, "b"))
      _ <- ts.insertOrUpdate((3, "c")).map(_ shouldBe 1)
      _ <- ts.insertOrUpdate((1, "d")).map(_ shouldBe 1)
      _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "d"), (2, "b"), (3, "c")))
    } yield ()
  }

  def testInsertOrUpdateAutoInc = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "T_MERGE2") {
      def id = column[Int]("ID", O.AutoInc, O.PrimaryKey)
      def name = column[String]("NAME")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]

    (for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((1, "a"), (2, "b"))
      _ <- ts.insertOrUpdate((0, "c")).map(_ shouldBe 1)
      _ <- ts.insertOrUpdate((1, "d")).map(_ shouldBe 1)
      _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "d"), (2, "b"), (3, "c")))
      _ <- ifCap(jcap.returnInsertKey) {
        val q = ts returning ts.map(_.id)
        for {
          _ <- q.insertOrUpdate((0, "e")).map(_ shouldBe Some(4))
          _ <- q.insertOrUpdate((1, "f")).map(_ shouldBe None)
          _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "f"), (2, "b"), (3, "c"), (4, "e")))
        } yield ()
      }
    } yield ()).withPinnedSession
  }
}
