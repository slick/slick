package slick.test.cats

import cats.MonadError
import cats.effect.IO
import cats.syntax.all.*
import com.typesafe.config.ConfigFactory
import munit.CatsEffectSuite

import slick.cats.Database
import slick.dbio.*
import slick.jdbc.{DatabaseConfig, JdbcProfile}

/** Runs actions built with cats combinators against an in-memory H2 database. Scala 3 only
  * because it relies on `DBIOBase` being inferred for full `DBIOAction` types. */
class DBIOInstancesTest extends CatsEffectSuite {

  private val h2Config = ConfigFactory.parseString(
    """
      |mydb {
      |  profile = "slick.jdbc.H2Profile$"
      |  db {
      |    connectionPool = disabled
      |    driver = "org.h2.Driver"
      |    url = "jdbc:h2:mem:slickcatstest;DB_CLOSE_DELAY=-1"
      |  }
      |}
      |""".stripMargin
  )

  // not private: the `Rows` table class below refers to it in its type
  val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
  import dc.profile.api.*

  private val db = ResourceSuiteLocalFixture("db", Database.resource(dc))
  override def munitFixtures = List(db)

  class Rows(tag: Tag) extends Table[Int](tag, "CATS_ROWS") {
    def v = column[Int]("V")
    def * = v
  }
  private val rows = TableQuery[Rows]

  private def withTable[A](body: => IO[A]): IO[A] =
    db().run(rows.schema.dropIfExists >> rows.schema.create) *> body.guarantee(db().run(rows.schema.dropIfExists).void)

  private val F = MonadError[DBIOBase, Throwable]

  test("traverse over inserts, then query") {
    withTable {
      // `rows += i` is a profile-specific DBIOAction with a Write effect: no upcast needed
      val inserts = (1 to 5).toList.traverse { i => rows += i }
      for {
        counts <- db().run(inserts)
        result <- db().run(rows.sortBy(_.v).result)
      } yield {
        assertEquals(counts, List(1, 1, 1, 1, 1))
        assertEquals(result, Vector(1, 2, 3, 4, 5))
      }
    }
  }

  test("sequence, tupled and >> compose actions in order") {
    withTable {
      val program = (rows += 1) >> (rows += 2) >> (rows.sortBy(_.v).result, rows.length.result).tupled
      db().run(program).map { case (all, n) =>
        assertEquals(all, Vector(1, 2))
        assertEquals(n, 2)
      }
    }
  }

  test("a cats result can be used inside an ordinary transaction") {
    withTable {
      val inserts = List(1, 2, 3).traverse_ { i => rows += i }
      val failing = inserts.toDBIO >> DBIO.failed(new IllegalStateException("roll me back"))
      for {
        outcome <- db().run(failing.transactionally).attempt
        n <- db().run(rows.length.result)
      } yield {
        assert(outcome.isLeft)
        assertEquals(n, 0, "the transaction must have rolled back the inserts")
      }
    }
  }

  test("raiseError fails the action") {
    val failed: DBIOBase[Int] = F.raiseError(new IllegalArgumentException("boom"))
    interceptIO[IllegalArgumentException](db().run(failed))
  }

  test("handleErrorWith recovers, attempt reifies") {
    val failed: DBIOBase[Int] = F.raiseError(new IllegalArgumentException("boom"))
    for {
      recovered <- db().run(failed.handleErrorWith(_ => F.pure(42)))
      attempted <- db().run(failed.attempt)
      ok <- db().run(F.pure(1).attempt)
    } yield {
      assertEquals(recovered, 42)
      assert(attempted.left.exists(_.getMessage == "boom"))
      assertEquals(ok, Right(1))
    }
  }

  test("tailRecM is stack safe") {
    val n = 100000
    val loop = F.tailRecM(0)(i => DBIO.successful(if (i < n) Left(i + 1) else Right(i)))
    db().run(loop).assertEquals(n)
  }

  test("a long left-nested flatMap chain is stack safe") {
    val n = 100000
    val sum = (1 to n).foldLeft(F.pure(0L))((acc, i) => acc.flatMap(a => F.pure(a + i)))
    db().run(sum).assertEquals(n.toLong * (n + 1) / 2)
  }

  test("the DBIO instance behaves the same as the DBIOBase instance") {
    val G = MonadError[DBIO, Throwable]
    val program: DBIO[Int] = G.handleErrorWith(G.raiseError[Int](new RuntimeException("x")))(_ => G.pure(7))
    db().run(program).assertEquals(7)
  }
}
