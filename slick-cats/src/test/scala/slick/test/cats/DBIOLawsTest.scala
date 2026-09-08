package slick.test.cats

import cats.Eq
import cats.effect.unsafe.implicits.global
import cats.laws.discipline.MonadErrorTests
import cats.syntax.all.*
import com.typesafe.config.ConfigFactory
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}

import slick.cats.Database
import slick.cats.dbio.instances.*
import slick.dbio.*
import slick.jdbc.{DatabaseConfig, JdbcProfile}

/** Checks the cats `MonadError` laws for both instances. Actions are compared by running them
  * against an in-memory H2 database and comparing the outcomes. */
class DBIOLawsTest extends DisciplineSuite {

  private val h2Config = ConfigFactory.parseString(
    """
      |mydb {
      |  profile = "slick.jdbc.H2Profile$"
      |  db {
      |    connectionPool = disabled
      |    driver = "org.h2.Driver"
      |    url = "jdbc:h2:mem:slickcatslaws;DB_CLOSE_DELAY=-1"
      |  }
      |}
      |""".stripMargin
  )

  private val dc = DatabaseConfig.forConfig[JdbcProfile]("mydb", h2Config)
  private val db: Database = Database.make(dc).unsafeRunSync()

  override def afterAll(): Unit = {
    db.close()
    super.afterAll()
  }

  private def outcome[A](fa: DBIOBase[A]): Either[Throwable, A] =
    db.run(fa.toDBIO).attempt.unsafeRunSync()

  final class LawsException(msg: String) extends RuntimeException(msg)

  implicit val eqThrowable: Eq[Throwable] = Eq.by(t => (t.getClass.getName, t.getMessage))
  implicit val arbThrowable: Arbitrary[Throwable] = Arbitrary(Arbitrary.arbitrary[String].map(new LawsException(_)))
  implicit val cogenThrowable: Cogen[Throwable] = Cogen[String].contramap(t => s"${t.getClass.getName}:${t.getMessage}")

  implicit def eqDBIOBase[A: Eq]: Eq[DBIOBase[A]] = Eq.instance((x, y) => outcome(x) === outcome(y))
  implicit def eqDBIO[A: Eq]: Eq[DBIO[A]] = Eq.instance((x, y) => outcome(x) === outcome(y))

  private def genDBIO[A: Arbitrary]: Gen[DBIO[A]] = {
    val pure = Arbitrary.arbitrary[A].map(a => DBIO.successful(a): DBIO[A])
    val failed = Arbitrary.arbitrary[Throwable].map(t => DBIO.failed(t): DBIO[A])
    val nested = for { a <- pure; b <- Gen.frequency(3 -> pure, 1 -> failed) } yield (a.flatMap(_ => b): DBIO[A])
    Gen.frequency(4 -> pure, 1 -> failed, 2 -> nested)
  }

  implicit def arbDBIO[A: Arbitrary]: Arbitrary[DBIO[A]] = Arbitrary(genDBIO[A])
  implicit def arbDBIOBase[A: Arbitrary]: Arbitrary[DBIOBase[A]] = Arbitrary(genDBIO[A].map(a => a: DBIOBase[A]))

  checkAll("MonadError[DBIOBase, Throwable]", MonadErrorTests[DBIOBase, Throwable].monadError[Int, Int, Int])
  checkAll("MonadError[DBIO, Throwable]", MonadErrorTests[DBIO, Throwable].monadError[Int, Int, Int])
}
