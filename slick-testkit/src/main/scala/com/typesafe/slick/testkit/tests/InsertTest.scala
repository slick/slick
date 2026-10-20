package com.typesafe.slick.testkit.tests

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.util.{Failure, Success}
import cats.effect.IO
import slick.jdbc.{JdbcCapabilities, MySQLProfile, PostgresProfile}
import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class InsertTest extends AsyncTest[JdbcTestDB] {

  import tdb.profile.api.*


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
      dst3comp.result.map(v => v.toSet shouldBe Set((1,"A"), (2,"B")))
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

  def testInsertAllTupleWithSingleInsert = ifCap(jcap.insertMultipleRowsWithSingleStatement) {
    class A(tag: Tag) extends Table[(Int, Int)](tag, "insert_all") {
      def id = column[Int]("id", O.PrimaryKey)
      def v1 = column[Int]("v1")
      def * = (id, v1)
    }
    val as = TableQuery[A]
    val records = Seq((1, 10), (2, 20), (3, 30))
    DBIO.seq(
      as.schema.create,
      as.insertAll(records).map(_ shouldBe Some(3)),
      as.sortBy(_.id).result.map(_ shouldBe records)
    )
  }
  def testInsertAllProduct = ifCap(jcap.insertMultipleRowsWithSingleStatement) {
    case class E(id: Int, v1: Int)
    class A(tag: Tag) extends Table[E](tag, "insert_all_product") {
      def id = column[Int]("id", O.PrimaryKey)
      def v1 = column[Int]("v1")
      def * = (id, v1).mapTo[E]
    }
    val as = TableQuery[A]
    val records = Seq(E(1, 10), E(2, 20), E(3, 30))
    DBIO.seq(
      as.schema.create,
      as.insertAll(records).map(_ shouldBe Some(3)),
      as.sortBy(_.id).result.map(_ shouldBe records)
    )
  }
  def testInsertAllAutoInc = ifCap (jcap.returnInsertKey, jcap.insertMultipleRowsWithSingleStatement) {
    class A(tag: Tag) extends Table[(Int, Int)](tag, "insert_all_auto_inc") {
      def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
      def v1 = column[Int]("V1")
      def * = (id, v1)
    }
    val as = TableQuery[A]
    val records = Seq((10, 10), (20, 20), (30, 30))
    for {
      _ <- as.schema.create
      result <- as.returning(as.map(_.id)).insertAll(records)
      _ <- ifCap(jcap.returnMultipleInsertKey)(DBIO.successful(result shouldBe Seq(1, 2, 3)))
      _ <- ifNotCap(jcap.returnMultipleInsertKey)(DBIO.successful(result shouldBe Nil))
      _ <- as.sortBy(_.id).result.map(_ shouldBe Seq((1, 10), (2, 20), (3, 30)))
    } yield ()
  }

  def testInsertAllDefaultValue = ifCap(jcap.insertMultipleRowsWithSingleStatement) {
    class A(tag: Tag) extends Table[Int](tag, "insert_all_default") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def * = id
    }
    val as = TableQuery[A]
    DBIO.seq(
      as.schema.create,
      as.insertAll(Seq(1, 2, 3)),
      as.result.map(_ shouldBe Seq(1, 2, 3))
    )
  }


  def testUniqueInsert = {
    case class ARow(email: String , id: Int = 0)
    class A(tag: Tag) extends Table[ARow](tag , "A_UNIQUEINSERT"){
      def id = column[Int]("id" , O.AutoInc , O.PrimaryKey)
      def email = column[String]("email" , O.Unique , O.Length(254))

      def * = (email , id).mapTo[ARow]
    }
    val atq = TableQuery[A]

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
    def ins1 = as.map(a => (a.s1, a.s2)).returning(as.map(_.id))
    def ins2 = as.map(a => (a.s1, a.s2)).returning(as.map(a => (a.id, a.s1)))
    def ins3 = as.map(a => (a.s1, a.s2)).returning(as.map(_.id)).into((v, i) => (i, v._1, v._2))
    def ins4 = as.map(a => (a.s1, a.s2)).returning(as.map(a => a))

    (for {
      _ <- as.schema.create
      _ <- (ins1 += ("a", "b")) map { (id1: Int) => id1 shouldBe 1 }
      _ <- ifCap(jcap.returnInsertOther) {
        (ins2 += ("c", "d")) map { (id2: (Int, String)) => id2 shouldBe ((2, "c")) }
      }
      _ <- ifNotCap(jcap.returnInsertOther) {
        (ins1 += ("c", "d")) map { (id2: Int) => id2 shouldBe 2 }
      }
      _ <- (ins1 ++= Seq(("e", "f"), ("g", "h"))).map(_ shouldBe Seq(3, 4))
      _ <- (ins3 += ("i", "j")).map(_ shouldBe ((5, "i", "j")))
      _ <- ifCap(jcap.returnInsertOther) {
        (ins4 += ("k", "l")) map { (id5: (Int, String, String)) => id5 shouldBe ((6, "k", "l")) }
      }
    } yield ()).withPinnedSession // Some database servers (e.g. DB2) preallocate ID blocks per session
  }

  def testReturningColumnsThatNeedQuoting = ifCap(jcap.returnInsertKey) {
    // Lower-case names and reserved words are only valid as quoted identifiers
    class A(tag: Tag) extends Table[(Int, String, String)](tag, "a_quoted") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def s = column[String]("s")
      def comment = column[String]("COMMENT")
      def * = (id, s, comment)
    }
    val as = TableQuery[A]
    def ins = as.map(a => (a.s, a.comment))
    (for {
      _ <- as.schema.create
      _ <- (ins.returning(as.map(_.id)) += (("a", "x"))).map(_ shouldBe 1)
      _ <- ifCap(jcap.returnInsertOther) {
        (ins.returning(as) += (("b", "y"))).map(_ shouldBe ((2, "b", "y")))
      }
      _ <- ifCap(jcap.returnInsertOther) {
        (ins.returning(as.map(_.comment)) += (("c", "z"))).map(_ shouldBe "z")
      }
    } yield ()).withPinnedSession
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

  def testInsertOrUpdateAll = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "insert_or_update") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
      def ins = (id, name)
    }
    val ts = TableQuery[T]
    val isMySQL = tdb.profile.isInstanceOf[MySQLProfile]
    def prepare = DBIO.seq(ts.schema.create, ts ++= Seq((1, "a"), (2, "b")))
    if (tdb.capabilities.contains(JdbcCapabilities.insertOrUpdate)) {
      for {
        _ <- prepare
        // MySQL counts an updated row as two affected rows
        _ <- ts.insertOrUpdateAll(Seq((3, "c"), (1, "d"))).map(_.foreach(_ shouldBe (if (isMySQL) 3 else 2)))
        _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "d"), (2, "b"), (3, "c")))
      } yield ()
    } else {
      for {
        _ <- prepare
        _ <- ts.insertOrUpdateAll(Seq((3, "c"), (1, "d")))
      } yield ()
    }.asTry.map {
      case Failure(exception) => exception.isInstanceOf[SlickException] shouldBe true
      case _ => throw new RuntimeException("Should insertOrUpdateAll is not supported for this profile.")
    }
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

  def testInsertOrUpdatePlainWithFuncDefinedPK: DBIOAction[Unit, NoStream, Effect.All] = {
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

  // Regression test for https://github.com/slick/slick/issues/2114
  def testInsertOrUpdateWithFuncDefinedCompositePK: DBIOAction[Unit, NoStream, Effect.All] = {
    // Upper-case names because Oracle and DB2 cannot return columns with lower-case names
    class T(tag: Tag) extends Table[(Int, String, String)](tag, "T_MERGE_COMPOSITE") {
      def a = column[Int]("A")
      def b = column[String]("B", O.Length(20))
      def name = column[String]("NAME")
      def * = (a, b, name)
      def pk = primaryKey("T_MERGE_COMPOSITE_PK", (a, b))
    }
    val ts = TableQuery[T]

    (for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((1, "x", "a"), (2, "x", "b"))
      _ <- ts.insertOrUpdate((1, "y", "c")).map(_ shouldBe 1)
      _ <- ts.insertOrUpdate((1, "x", "d")).map(_ shouldBe 1)
      _ <- ts.sortBy(t => (t.a, t.b)).result.map(_ shouldBe Seq((1, "x", "d"), (1, "y", "c"), (2, "x", "b")))
      _ <- ifCap(jcap.returnInsertOther) {
        val q = ts.returning(ts)
        for {
          _ <- q.insertOrUpdate((3, "x", "e")).map(_ shouldBe Some((3, "x", "e")))
          _ <- q.insertOrUpdate((2, "x", "f")).map(_ shouldBe None)
          _ <- ts.sortBy(t => (t.a, t.b)).result.map(
            _ shouldBe Seq((1, "x", "d"), (1, "y", "c"), (2, "x", "f"), (3, "x", "e"))
          )
        } yield ()
      }
    } yield ()).withPinnedSession
  }

  def testInsertOrUpdateNotIncrement() =
    if (!tdb.capabilities.contains(JdbcCapabilities.insertOrUpdate)) {
      DBIO.successful(())
    } else {
      case class A(id1: Int, id2: Int)
      class ATable(tag: Tag) extends Table[A](tag, "ATABLE") {
        // using keyword for checking they are quoted
        def select = column[Int]("select")

        def from = column[Int]("from")

        val pk = primaryKey("pk_for_atable", (select, from))

        def * = (select, from) <> ((A.apply _).tupled, A.unapply)
      }
      case class B(id1: Int, id2: Int, v: Int)
      class BTable(tag: Tag) extends Table[B](tag, "BTABLE") {
        def id1 = column[Int]("id1")

        def id2 = column[Int]("id2")

        def v = column[Int]("v")

        val pk = primaryKey("pk_for_btable", (id1, id2))

        def * = (id1, id2, v) <> ((B.apply _).tupled, B.unapply)
      }
      case class E(id: Int)
      class ETable(tag: Tag) extends Table[E](tag, "ETABLE") {
        def id = column[Int]("id", O.PrimaryKey)

        def * = id.<>(E.apply, (e : E) => Option(e.id))
      }

      val a = TableQuery[ATable]
      val b = TableQuery[BTable]
      val e = TableQuery[ETable]
      for {
        _ <- a.schema.create
        _ <- a.insertOrUpdate(A(0, 0))
        _ <- a.insertOrUpdate(A(0, 0))
        allA <- a.result
        _ <- b.schema.create
        _ <- b.insertOrUpdate(B(0, 0, 1))
        _ <- b.insertOrUpdate(B(0, 0, 2))
        _ <- b.insertOrUpdate(B(1, 0, 2))
        allB <- b.result
        _ <- e.schema.create
        _ <- e.insertOrUpdate(E(0))
        _ <- e.insertOrUpdate(E(0))
        allE <- e.result
      } yield {
        allA shouldBe Seq(A(0, 0))
        allB.toSet shouldBe Set(B(0, 0, 2), B(1, 0, 2))
        allE shouldBe Seq(E(0))
      }
    }

  def testInsertOrUpdateWithInsertedWhen0IsSpecifiedForAutoInc: DBIOAction[Unit, NoStream, Effect.All] =
    // Uses MySQL's AUTO_INCREMENT DDL and tests how ON DUPLICATE KEY UPDATE treats an AutoInc value of 0
    if (!tdb.profile.isInstanceOf[MySQLProfile]) DBIO.successful(()) else {
      case class C(id1: Int, id2: Int)
      class CTable(tag: Tag) extends Table[C](tag, "CTABLE") {
        def id1 = column[Int]("id1", O.AutoInc)

        def id2 = column[Int]("id2")

        val pk = primaryKey("pk_for_ctable", (id1, id2))

        def * = (id1, id2) <> ((C.apply _).tupled, C.unapply)
      }
      case class D(id1: Int, id2: Int, v: Int)
      class DTable(tag: Tag) extends Table[D](tag, "DTABLE") {
        def id1 = column[Int]("id1", O.AutoInc)

          def id2 = column[Int]("id2")

          def v = column[Int]("v")

          val pk = primaryKey("pk_for_dtable", (id1, id2))

          def * = (id1, id2, v) <> ((D.apply _).tupled, D.unapply)
      }
      case class F(id: Int)
      class FTable(tag: Tag) extends Table[F](tag, "FTABLE") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def * = (id) <> (F.apply, (f: F) => Option(f.id))
      }
      val c = TableQuery[CTable]
      val d = TableQuery[DTable]
      val f = TableQuery[FTable]
      for {
        _ <- sqlu"CREATE TABLE CTABLE(id1 INT AUTO_INCREMENT, id2 INT, PRIMARY KEY(id1, id2))"
        _ <- c.insertOrUpdate(C(0, 1)) // inserted
        _ <- c.insertOrUpdate(C(0, 1)) // inserted
        allC <- c.result
        _ <- sqlu"CREATE TABLE DTABLE(id1 INT AUTO_INCREMENT, id2 INT, v INT, PRIMARY KEY(id1, id2))"
        _ <- d.insertOrUpdate(D(0, 0, 1)) // inserted
        _ <- d.insertOrUpdate(D(0, 0, 2)) // inserted
        _ <- d.insertOrUpdate(D(0, 0, 2)) // inserted
        _ <- d.insertOrUpdate(D(1, 0, 1)) // updated
        allD <- d.result
        _ <- f.schema.create
        _ <- f.insertOrUpdate(F(0)) // inserted
        _ <- f.insertOrUpdate(F(0)) // inserted
        allF <- f.result
      } yield {
        allC.toSet shouldBe Set(C(1, 1), C(2, 1))
        allD.toSet shouldBe Set(D(1, 0, 1), D(2, 0, 2), D(3, 0, 2))
        allF.toSet shouldBe Set(F(1), F(2))
      }
    }

  def testInertOrUpdateWithAutoIncAndUniqueColumn() =
    // Tests that MySQL's ON DUPLICATE KEY UPDATE also matches a unique column that is not the primary key
    if (!tdb.profile.isInstanceOf[MySQLProfile]) DBIO.successful(()) else {
      case class AnimalRow(id: Int, name: String, location: String)
      class Animals(tag: Tag) extends Table[AnimalRow](tag, "animals") {
        def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
        def name = column[String]("name", O.Unique, O.Length(100))
        def location = column[String]("location")
        def * = (id, name, location).mapTo[AnimalRow]
      }
      val animals = TableQuery[Animals]
      for {
        _ <- animals.schema.create
        _ <- animals.insertOrUpdate(AnimalRow(0, "tiger", "NY"))
        tigerAfterInsert <- animals.filter(_.name === "tiger").result.head
        _ <- animals.insertOrUpdate(AnimalRow(0, "tiger", "CA"))
        tigerAfterUpsertByUniqueName <- animals.filter(_.name === "tiger").result.head
      } yield {
        tigerAfterInsert.id shouldBe tigerAfterUpsertByUniqueName.id
        tigerAfterUpsertByUniqueName.location shouldBe "CA"
      }
    }

  def testInsertOrUpdateAutoInc: DBIOAction[Unit, NoStream, Effect.All] = {
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
        val q = ts.returning(ts.map(_.id))
        for {
          _ <- q.insertOrUpdate((0, "e")).map(_ shouldBe Some(4))
          _ <- q.insertOrUpdate((1, "f")).map(_ shouldBe None)
          _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "f"), (2, "b"), (3, "c"), (4, "e")))
        } yield ()
      }
    } yield ()).withPinnedSession
  }

  // Regression test for https://github.com/slick/slick/issues/3867: a table whose only column is an AutoInc primary
  // key has nothing to insert but generated values, so a new key inserts a row with a generated key and an existing
  // key leaves the table unchanged
  def testInsertOrUpdateOnlyAutoIncColumn: DBIOAction[Unit, NoStream, Effect.All] = {
    class T(tag: Tag) extends Table[Int](tag, "T_ONLY_AUTOINC") {
      def id = column[Int]("ID", O.AutoInc, O.PrimaryKey)
      def * = id
    }
    val ts = TableQuery[T]
    (for {
      _ <- ts.schema.create
      _ <- ts.insertOrUpdate(0).map(_ shouldBe 1)
      ids <- ts.result
      _ = ids.length shouldBe 1
      _ <- ts.insertOrUpdate(ids.head).map(_ shouldBe 1)
      _ <- ts.result.map(_ shouldBe ids)
      _ <- ifCap(jcap.returnInsertKey) {
        val q = ts.returning(ts.map(_.id))
        for {
          _ <- q.insertOrUpdate(ids.head).map(_ shouldBe None)
          _ <- ts.result.map(_ shouldBe ids)
          inserted <- q.insertOrUpdate(0)
          _ = inserted.isDefined shouldBe true
          _ <- ts.sortBy(_.id).result.map(_ shouldBe (ids ++ inserted).sorted)
        } yield ()
      }
    } yield ()).withPinnedSession
  }

  // Regression test for https://github.com/slick/slick/issues/1627
  def testInsertOrUpdateWithPrimaryKeyOnly: DBIOAction[Unit, NoStream, Effect.All] =
    if (!tdb.profile.capabilities.contains(JdbcCapabilities.insertOrUpdateWithPrimaryKeyOnly))
      DBIO.successful(())
    else {
      class T(tag: Tag) extends Table[Int](tag, "mytable") {
        def id = column[Int]("id", O.PrimaryKey)
        def * = id
      }
      case class V(id: Int, value: String)
      class T2(tag: Tag) extends Table[V](tag, "mytable2") {
        def id = column[Int]("id")
        def value = column[String]("value", O.Length(20))
        def * = (id, value) <> ((V.apply _).tupled, V.unapply)
        def pk = primaryKey("pk", (id, value))
      }
      // Upper-case names because Oracle and DB2 cannot return columns with lower-case names
      class T3(tag: Tag) extends Table[V](tag, "MYTABLE3") {
        def id = column[Int]("ID")
        def value = column[String]("VAL", O.Length(20))
        def * = (id, value) <> ((V.apply _).tupled, V.unapply)
        def pk = primaryKey("MYTABLE3_PK", (id, value))
      }
      val ts1 = TableQuery[T]
      val ts2 = TableQuery[T2]
      val ts3 = TableQuery[T3]
      // insertOrUpdate returns 1 whichever statements ran, so check that none of them has an empty SET or WHERE
      val emptyClause = """(?i)\bset\s+where\b|\bwhere\s*(;|$)""".r
      def checkClauses(statements: Iterable[String]): Unit =
        statements.foreach(s => emptyClause.findFirstIn(s) shouldBe None)
      checkClauses(ts1.insertOrUpdate(1).statements ++ ts2.insertOrUpdate(V(1, "a")).statements)
      (for {
        _ <- ts1.schema.create
        _ <- ts1 ++= Seq(1, 2)
        _ <- ts1.insertOrUpdate(0).map(_ shouldBe 1)
        _ <- ts1.insertOrUpdate(1).map(_ shouldBe 1)
        _ <- ts1.sortBy(_.id).result.map(_ shouldBe Seq(0, 1, 2))
        _ <- ts2.schema.create
        _ <- ts2 ++= Seq(V(1, "a"), V(2, "b"))
        _ <- ts2.insertOrUpdate(V(0, "c")).map(_ shouldBe 1)
        _ <- ts2.insertOrUpdate(V(1, "a")).map(_ shouldBe 1)
        _ <- ts2.insertOrUpdate(V(2, "d")).map(_ shouldBe 1)
        _ <- ts2.sortBy(t => (t.id, t.value)).result.map(_ shouldBe Seq(V(0, "c"), V(1, "a"), V(2, "b"), V(2, "d")))
        _ <- ifCap(jcap.returnInsertOther) {
          val q = ts3.returning(ts3)
          checkClauses(q.insertOrUpdate(V(1, "a")).statements)
          for {
            _ <- ts3.schema.create
            _ <- ts3 += V(1, "a")
            _ <- q.insertOrUpdate(V(2, "b")).map(_ shouldBe Some(V(2, "b")))
            _ <- q.insertOrUpdate(V(1, "a")).map(_ shouldBe None)
            _ <- ts3.sortBy(t => (t.id, t.value)).result.map(_ shouldBe Seq(V(1, "a"), V(2, "b")))
          } yield ()
        }
      } yield ()).withPinnedSession
    }

  // Regression test for https://github.com/slick/slick/issues/3846: SQLite's INSERT OR REPLACE deleted the other row
  def testInsertOrUpdateClashOnUniqueColumn: DBIOAction[Unit, NoStream, Effect.All] = {
    class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge_unique") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name", O.Length(20), O.Unique)
      def * = (id, name)
    }
    val ts = TableQuery[T]

    (for {
      _ <- ts.schema.create
      _ <- ts ++= Seq((2, "Abcd"), (3, "c"))
      result <- ts.insertOrUpdate((1, "Abcd")).asTry
      _ <- ts.sortBy(_.id).result.map { rows =>
        if (tdb.profile.isInstanceOf[MySQLProfile]) {
          // ON DUPLICATE KEY UPDATE matches the unique key and gives the other row the new primary key
          result.isSuccess shouldBe true
          rows shouldBe Seq((1, "Abcd"), (3, "c"))
        } else {
          result match {
            case Failure(_: java.sql.SQLException) => ()
            case other => throw new RuntimeException(s"insertOrUpdate should fail on the unique column, got $other")
          }
          rows shouldBe Seq((2, "Abcd"), (3, "c"))
        }
      }
    } yield ()).withPinnedSession
  }

  // Regression test for https://github.com/slick/slick/issues/3846: SQLite's INSERT OR REPLACE reset other columns
  def testInsertOrUpdateKeepsColumnsNotInProjection: DBIOAction[Unit, NoStream, Effect.All] = {
    class T(tag: Tag) extends Table[(Int, String, String)](tag, "t_merge_partial") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name", O.Length(20))
      def note = column[String]("note", O.Length(20), O.Default("none"))
      def * = (id, name, note)
    }
    val ts = TableQuery[T]
    val idAndName = ts.map(t => (t.id, t.name))

    (for {
      _ <- ts.schema.create
      _ <- ts += ((1, "a", "kept"))
      _ <- idAndName.insertOrUpdate((1, "b")).map(_ shouldBe 1)
      _ <- idAndName.insertOrUpdate((2, "c")).map(_ shouldBe 1)
      _ <- ts.sortBy(_.id).result.map(_ shouldBe Seq((1, "b", "kept"), (2, "c", "none")))
    } yield ()).withPinnedSession
  }

  def testInsertOrUpdateTableNamedExcluded: DBIOAction[Unit, NoStream, Effect.All] = {
    // SQLite's upsert refers to the proposed row as `excluded`, which must not resolve to the table itself
    class T(tag: Tag) extends Table[(Int, String)](tag, "excluded") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name", O.Length(20))
      def * = (id, name)
    }
    val ts = TableQuery[T]

    (for {
      _ <- ts.schema.create
      _ <- ts += ((1, "old"))
      _ <- ts.insertOrUpdate((1, "new"))
      _ <- ts.result.map(_ shouldBe Seq((1, "new")))
      _ <- ts.schema.drop
    } yield ()).withPinnedSession
  }

  // Regression test for https://github.com/slick/slick/issues/2970: PostgreSQL's upsert ran an UPDATE and then an
  // INSERT ... WHERE NOT EXISTS, so a row with the same key that another transaction inserted and committed while the
  // upsert was running made the upsert fail with a duplicate key or lose its update. INSERT ... ON CONFLICT waits for
  // the other transaction and then updates its row.
  def testInsertOrUpdateConcurrentInsert: IO[Unit] =
    if (!tdb.profile.isInstanceOf[PostgresProfile]) IO.unit
    else {
      class T(tag: Tag) extends Table[(Int, String)](tag, "t_merge_concurrent") {
        def id = column[Int]("id", O.PrimaryKey)
        def name = column[String]("name", O.Length(20))
        def * = (id, name)
      }
      val ts = TableQuery[T]
      val timeout = 10.seconds
      val otherInserted = new CountDownLatch(1)
      val commitOther = new CountDownLatch(1)
      val upsertPid = new AtomicInteger
      val upsertPidKnown = new CountDownLatch(1)

      def await(latch: CountDownLatch, what: String): Unit =
        if (!latch.await(timeout.toSeconds, TimeUnit.SECONDS))
          throw new RuntimeException(s"Timed out waiting for $what")

      // Inserts the key and keeps its transaction open until the upsert waits for it
      val otherInsert =
        (ts += ((1, "other"))).andThen(SimpleDBIO { _ =>
          otherInserted.countDown()
          await(commitOther, "the upsert to wait for the other transaction")
        }).transactionally
      val upsert =
        sql"select pg_backend_pid()".as[Int].head.flatMap { pid =>
          upsertPid.set(pid)
          upsertPidKnown.countDown()
          ts.insertOrUpdate((1, "new"))
        }.withPinnedSession
      def awaitUpsertWaitingForLock: IO[Unit] =
        db.run(sql"select wait_event_type from pg_stat_activity where pid = ${upsertPid.get}".as[Option[String]].head)
          .flatMap { waitEventType =>
            if (waitEventType.contains("Lock")) IO.unit
            else IO.sleep(10.millis) >> awaitUpsertWaitingForLock
          }

      for {
        _ <- db.run(ts.schema.create)
        other <- db.run(otherInsert).start
        _ <- IO.blocking(await(otherInserted, "the other transaction to insert its row"))
        upserting <- db.run(upsert).start
        _ <- IO.blocking(await(upsertPidKnown, "the upsert's connection"))
        _ <- awaitUpsertWaitingForLock.timeout(timeout).guarantee(IO(commitOther.countDown()))
        _ <- other.joinWithNever.timeout(timeout)
        _ <- upserting.joinWithNever.timeout(timeout)
        _ <- db.run(ts.result).map(_ shouldBe Seq((1, "new")))
      } yield ()
    }

  // Regression test for https://github.com/slick/slick/issues/2045
  def testInsertOrUpdateWithIntegrityError: DBIOAction[Unit, NoStream, Effect.All] = {
    case class Book(id: Long, title: String, code: Option[String] = None)
    class BooksTable(tag: Tag) extends Table[Book](tag, "books") {
      def id = column[Long]("id", O.PrimaryKey)
      def title = column[String]("title", O.Length(20))
      def code = column[Option[String]]("code", O.Length(20))
      override def * = (id, title, code) <> ((Book.apply _).tupled, Book.unapply)
    }
    val books = TableQuery[BooksTable]
    case class BookMeta(id: Long, bookId: Long, tag: Long)
    class BookMetaTable(tag: Tag) extends Table[BookMeta](tag, "book_meta") {
      def id    = column[Long]("id")
      def bookId = column[Long]("book_id")
      def bookTag  = column[Long]("tag")
      def pk = primaryKey("book_meta_pk", (bookId, id))
      def book = foreignKey("book_fk", bookId, books)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)
      override def * = (bookId, id, bookTag) <> ((BookMeta.apply _).tupled, BookMeta.unapply)
    }
    val meta = TableQuery[BookMetaTable]

    val bookMeta = BookMeta(-1, -1, 0) // fail, because there are no books.
    DBIO.seq(
      meta.insertOrUpdate(bookMeta).asTry.map {
        case Success(_) => throw new Exception("Insertion should be failed.")
        case _ => ()
      }
    )
  }

  // sqlite donesn't care this case
  def testInsertAndUpdateShouldNotTruncateData = if (!tdb.confName.contains("sqlite")) {
    case class Test(id: Int, name: String)
    class TestTable(tag: Tag) extends Table[Test](tag, "test") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name", O.Length(2))
      def * = (id, name) <> ((Test.apply _).tupled, Test.unapply)
    }
    val ts = TableQuery[TestTable]
    DBIO.seq(
      ts.schema.create,
      ts ++= Seq(Test(1, "a")),
      (ts ++= Seq(Test(2, "123"))).asTry.map {
        case Success(_) => throw new Exception("Data is truncated. It shouldn't be succeeded.")
        case _ => ()
      },
      ts.filter(_.id === 1).map(_.name).update("123").asTry.map {
        case Success(_) => throw new Exception("Data is truncated. It shouldn't be succeeded.")
        case _ => ()
      },
      ts.insertOrUpdate(Test(1, "123")).asTry.map {
        case Success(_) => throw new Exception("Data is truncated. It shouldn't be succeeded.")
        case _ => ()
      }
    )
  } else DBIO.seq()

  def testInsertAllEmptyList = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "insert_all_empty") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val as = TableQuery[A]

    // Test with empty List
    val emptyList = List[(Int, String)]()

    DBIO.seq(
      as.schema.create,
      as.insertAll(emptyList).map(_ shouldBe Some(0)),
      as.result.map(_ shouldBe Seq.empty)
    )
  }

  def testInsertAllEmptyVector = {
    class A(tag: Tag) extends Table[(Int, String)](tag, "insert_all_empty_vec") {
      def id = column[Int]("id", O.PrimaryKey)
      def name = column[String]("name")
      def * = (id, name)
    }
    val as = TableQuery[A]

    // Test with empty Vector
    val emptyVector = Vector[(Int, String)]()

    DBIO.seq(
      as.schema.create,
      as.insertAll(emptyVector).map(_ shouldBe Some(0)),
      as.result.map(_ shouldBe Seq.empty)
    )
  }

  def testInsertStatements = {
    class A(tname: String)(tag: Tag) extends Table[(Int, String)](tag, tname) {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name)
    }
    val as = TableQuery(new A("insert_statements")(_))
    val bs = TableQuery(new A("insert_statements_b")(_))
    val names = as.map(_.name)
    val rows = Seq((1, "a"), (2, "b"), (3, "c"))
    def placeholders(sql: String) = sql.count(_ == '?')

    // The soft insert skips the AutoInc column, the forced insert includes it
    placeholders(as.insertStatement) shouldBe 1
    placeholders(as.forceInsertStatement) shouldBe 2

    (as += ((1, "a"))).statements.toList shouldBe List(as.insertStatement)
    as.forceInsert((1, "a")).statements.toList shouldBe List(as.forceInsertStatement)
    (as ++= rows).statements.toList shouldBe List(as.insertStatement)
    as.forceInsertAll(rows).statements.toList shouldBe List(as.forceInsertStatement)
    (names += "a").statements.toList shouldBe List(names.insertStatement)

    // insertAll uses one multi-row statement by default where the profile supports it
    val insertAllStatements = as.insertAll(rows).statements.toList
    if (tdb.profile.defaultRowsPerStatement == slick.jdbc.RowsPerStatement.All) {
      insertAllStatements.size shouldBe 1
      placeholders(insertAllStatements.head) shouldBe rows.size
    } else insertAllStatements shouldBe List(as.insertStatement)

    val returning = as.returning(as.map(_.id))
    (returning += ((1, "a"))).statements.toList shouldBe List(returning.insertStatement)
    (returning ++= rows).statements.toList shouldBe List(returning.insertStatement)
    val into = returning.into((row, id) => row.copy(_1 = id))
    (into += ((1, "a"))).statements.toList shouldBe List(into.insertStatement)

    // A server-side upsert is a single statement; the emulation is a check, an update and an insert
    val upsertStatements = as.insertOrUpdate((1, "a")).statements.toList
    if (upsertStatements.size != 1) {
      upsertStatements.size shouldBe 3
      upsertStatements.last shouldBe as.insertStatement
    }
    upsertStatements.foreach(sql => placeholders(sql) should (_ > 0))

    val q = bs.filter(_.id > 1)
    as.forceInsertQuery(q).statements.toList shouldBe List(as.forceInsertStatementFor(q))
    as.forceInsertExpr((42, "x".bind)).statements.toList shouldBe List(as.forceInsertStatementFor((42, "x".bind)))

    DBIO.seq(
      (as.schema ++ bs.schema).create,
      // The statements of an insert action are the ones it executes, so overriding them redirects the insert
      (as += ((1, "a"))).overrideStatements(List(bs.insertStatement)).map(_ shouldBe 1),
      as.result.map(_ shouldBe Nil),
      bs.map(_.name).result.map(_ shouldBe Seq("a"))
    )
  }

}
