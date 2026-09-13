package slick.test.cats

import cats.syntax.all.*
import munit.FunSuite

import slick.jdbc.H2Profile.api.*
import slick.test.cats.TypeAssertions.*

/** These tests pass if they compile. On Scala 3 the profile-specific action subtypes returned by
  * the lifted API, including streaming ones, unify with a type class's `F[_]` as `DBIOBase[E, *]`
  * with their effect, so no upcast is needed. Scala 3 only; see `ProfileActionAliasInferenceTest`
  * for what works on every Scala version. */
class ProfileActionInferenceTest extends FunSuite {
  private def monad[F[_]: cats.Monad, A](fa: F[A]): F[A] = fa

  class T(tag: Tag) extends Table[Int](tag, "T") {
    def a = column[Int]("A")
    def * = a
  }
  private val ts = TableQuery[T]

  test("profile actions unify as DBIOBase[E, *] with their effect, no upcast needed") {
    exactly[DBIOBase[Effect.Read, Seq[Int]]](monad(ts.result)) // streaming action
    exactly[DBIOBase[Effect.Write, Int]](monad(ts += 1))
    exactly[DBIOBase[Effect.Schema, Unit]](monad(ts.schema.create))
    exactly[DBIOBase[Effect.Write, Int]](monad(ts.filter(_.a === 1).delete))
    exactly[DBIOBase[Effect.Write, Int]](monad(ts.map(_.a).update(2)))
    exactly[DBIOBase[Effect, Vector[Int]]](monad(sql"select 1".as[Int]))
    exactly[DBIOBase[Effect, Int]](monad(sqlu"delete from T"))
    exactly[DBIOBase[Effect.Read & Effect.Transactional, Seq[Int]]](monad(ts.result.transactionally))
    exactly[DBIOBase[Effect.Write, List[Int]]](List(1, 2).traverse(i => ts += i))
    exactly[DBIOBase[Effect.Read, List[Int]]](List(1, 2).traverse(i => ts.filter(_.a === i).result.head))
    exactly[DBIOBase[Effect.Write, Unit]](List(1, 2).traverse_(i => ts += i))
  }

  test("mixed effects infer the intersection where F is not fixed by a receiver") {
    exactly[DBIOBase[Effect.Read & Effect.Write, (Seq[Int], Int)]]((ts.result, ts += 1).tupled)
    exactly[DBIOBase[Effect.Write & Effect.Read, Seq[Int]]]((ts += 1) *> ts.result)
    typed[DBIOAction[Seq[Int], NoStream, Effect.Write & Effect.Read]]((ts += 1) >> ts.result)
  }

  test("cats results compose with Slick's combinators") {
    val inserts: DBIOBase[Effect.Write, List[Int]] = List(1, 2).traverse(i => ts += i)
    typed[DBIOAction[(List[Int], Seq[Int]), NoStream, Effect.Write & Effect.Read]](
      for { n <- inserts; all <- ts.result } yield (n, all)
    )
    typed[DBIOAction[List[Int], NoStream, Effect.Write & Effect.Transactional]](inserts.toAction.transactionally)
    typed[DBIOAction[List[Int], NoStream, Effect.Schema & Effect.Write]](ts.schema.create >> inserts)
  }
}
