package slick.dbio

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.Eq
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.*
import org.scalacheck.{Arbitrary, Gen}
import slick.memory.MemoryProfile

class SlickActionInstancesTest extends munit.DisciplineSuite {
  def run[R](a: SlickAction[NoStream, _, R]) = MemoryProfile.backend.Database().use(_.run(a)).unsafeRunSync()

  implicit val throwableEq: Eq[Throwable] = Eq.fromUniversalEquals

  implicit def actionEq[Effect <: slick.dbio.Effect, A: Eq]: Eq[SlickAction[NoStream, Effect, A]] = Eq.by(a => run(a.asTry))

  implicit def arbNEL[A: Arbitrary]: Arbitrary[NonEmptyList[A]] = Arbitrary {
    for {a <- Arbitrary.arbitrary[A]; size <- Arbitrary.arbitrary[Int]} yield NonEmptyList(a, List.fill(size.min(4))(a))
  }

  implicit def arbSlickAction[Effect <: slick.dbio.Effect, A: Arbitrary]: Arbitrary[SlickAction[NoStream, Effect, A]] = Arbitrary {
    val pure = Arbitrary.arbitrary[A].map(a => DBIO.successful(a))
    val failed = Arbitrary.arbitrary[Throwable].map(t => DBIO.failed(t))
    val nested = for {a <- pure; b <- Gen.frequency(3 -> pure, 1 -> failed)} yield a.flatMap(_ => b)
    Gen.frequency(4 -> pure, 1 -> failed, 2 -> nested)
  }

  private type Action[Effect <: slick.dbio.Effect, R] = SlickAction[NoStream, Effect, R]
  checkAll("DBIO[Int]", MonadErrorTests[DBIO, Throwable].monadError[Int, Int, Int])
  checkAll("DBIO[Int]", MonoidTests[DBIO[Int]].monoid)
  checkAll("DBIO[NonEmptyList[Int]]", SemigroupTests[DBIO[NonEmptyList[Int]]].semigroup)
  checkAll("SlickAction[Int]", MonadErrorTests[({type L[A] = Action[Effect, A]})#L, Throwable].monadError[Int, Int, Int])
  checkAll("SlickAction[Int]", MonoidTests[SlickAction[NoStream, Effect, Int]].monoid)
  checkAll("SlickAction[NonEmptyList[Int]]", SemigroupTests[SlickAction[NoStream, Effect, NonEmptyList[Int]]].semigroup)
  checkAll("SlickAction.Write[Int]", MonadErrorTests[({type L[A] = Action[Effect.Write, A]})#L, Throwable].monadError[Int, Int, Int])
  checkAll("SlickAction.Write[Int]", MonoidTests[SlickAction[NoStream, Effect.Write, Int]].monoid)
  checkAll("SlickAction.Write[NonEmptyList[Int]]", SemigroupTests[SlickAction[NoStream, Effect.Write, NonEmptyList[Int]]].semigroup)
}

