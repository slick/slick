package slick.test.cats

import cats.Eq
import cats.effect.unsafe.implicits.global
import cats.laws.discipline.MonadErrorTests
import cats.syntax.all.*
import com.typesafe.config.ConfigFactory
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Cogen, Gen}

import slick.cats.Database
import slick.dbio.*
import slick.jdbc.{DatabaseConfig, JdbcProfile}

/** Checks the cats `MonadError` laws for the `DBIOBase[E, *]` and `SlickAction[E, *]` instances,
  * on every Scala version. Actions are compared by running them against an in-memory H2 database
  * and comparing the outcomes. */
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

  type BaseAll[A] = DBIOBase[Effect.All, A]
  type ReadAction[A] = SlickAction[Effect.Read, A]

  // Effect.All extends every effect, so an action of any effect is a DBIOBase[Effect.All, A]
  private def outcome[A](fa: DBIOBase[Effect.All, A]): Either[Throwable, A] =
    db.run(fa.toAction).attempt.unsafeRunSync()

  final class LawsException(msg: String) extends RuntimeException(msg)

  implicit val eqThrowable: Eq[Throwable] = Eq.by(t => (t.getClass.getName, t.getMessage))
  implicit val arbThrowable: Arbitrary[Throwable] = Arbitrary(Arbitrary.arbitrary[String].map(new LawsException(_)))
  implicit val cogenThrowable: Cogen[Throwable] = Cogen[String].contramap(t => s"${t.getClass.getName}:${t.getMessage}")

  implicit def eqBaseAll[A: Eq]: Eq[BaseAll[A]] = Eq.instance((x, y) => outcome(x) === outcome(y))
  implicit def eqDBIO[A: Eq]: Eq[DBIO[A]] = Eq.instance((x, y) => outcome(x) === outcome(y))
  implicit def eqReadAction[A: Eq]: Eq[ReadAction[A]] = Eq.instance((x, y) => outcome(x) === outcome(y))

  private def genAction[E <: Effect, A: Arbitrary]: Gen[SlickAction[E, A]] = {
    val pure = Arbitrary.arbitrary[A].map(a => DBIO.successful(a): SlickAction[E, A])
    val failed = Arbitrary.arbitrary[Throwable].map(t => DBIO.failed(t): SlickAction[E, A])
    val nested = for { a <- pure; b <- Gen.frequency(3 -> pure, 1 -> failed) } yield (a.flatMap(_ => b): SlickAction[E, A])
    Gen.frequency(4 -> pure, 1 -> failed, 2 -> nested)
  }

  implicit def arbDBIO[A: Arbitrary]: Arbitrary[DBIO[A]] = Arbitrary(genAction[Effect.All, A])
  implicit def arbBaseAll[A: Arbitrary]: Arbitrary[BaseAll[A]] = Arbitrary(genAction[Effect.All, A].map(a => a: BaseAll[A]))
  implicit def arbReadAction[A: Arbitrary]: Arbitrary[ReadAction[A]] = Arbitrary(genAction[Effect.Read, A])

  checkAll("MonadError[DBIOBase[Effect.All, *], Throwable]", MonadErrorTests[BaseAll, Throwable].monadError[Int, Int, Int])
  checkAll("MonadError[DBIO, Throwable]", MonadErrorTests[DBIO, Throwable].monadError[Int, Int, Int])
  checkAll("MonadError[SlickAction[Effect.Read, *], Throwable]", MonadErrorTests[ReadAction, Throwable].monadError[Int, Int, Int])
}
