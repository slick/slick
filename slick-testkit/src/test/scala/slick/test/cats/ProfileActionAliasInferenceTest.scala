package slick.test.cats

import cats.syntax.all.*
import munit.FunSuite

import slick.jdbc.H2Profile.api.*
import slick.test.cats.TypeAssertions.*

/** These tests pass if they compile. The lifted API returns profile-specific subtypes of
  * `DBIOAction`, which Scala 2 cannot unify with the `F[_]` of a type class. `toAction` converts
  * them to `SlickAction[E, R]`, which is monadic on every Scala version and keeps the effect, and
  * so are the results of Slick's own combinators. Scala 3 needs neither, see
  * `ProfileActionInferenceTest` in the `scala-3` test sources. */
class ProfileActionAliasInferenceTest extends FunSuite {
  class T(tag: Tag) extends Table[Int](tag, "T") {
    def a = column[Int]("A")
    def * = a
  }
  private val ts = TableQuery[T]

  test("toAction makes profile actions monadic and keeps their effect") {
    typed[SlickAction[Effect.Write, Int]]((ts += 1).toAction)
    typed[SlickAction[Effect.Schema, Unit]](ts.schema.create.toAction)
    typed[SlickAction[Effect.Read, Seq[Int]]](ts.result.toAction) // the streaming type is dropped
    typed[SlickAction[Effect.Read, Int]](ts.filter(_.a === 1).result.head.toAction)
    typed[SlickAction[Effect.Write, Int]](ts.filter(_.a === 1).delete.toAction)
    typed[SlickAction[Effect.Write, Int]](ts.map(_.a).update(2).toAction)
    typed[DBIO[Vector[Int]]](sql"select 1".as[Int].toAction)
    typed[DBIO[Int]](sqlu"delete from T".toAction)
    typed[SlickAction[Effect.Write, List[Int]]](List(1, 2).traverse(i => (ts += i).toAction))
    typed[SlickAction[Effect.Read, List[Int]]](List(1, 2).traverse(i => ts.filter(_.a === i).result.head.toAction))
    typed[SlickAction[Effect.Read, Unit]](ts.result.toAction.void)
    typed[DBIOBase[Effect.Write, List[Int]]](List(1, 2).traverse(i => (ts += i): DBIOBase[Effect.Write, Int]))
  }

  test("the results of Slick's combinators on profile actions are monadic without conversion") {
    typed[SlickAction[Effect.Write, Int]]((ts += 1).map(_ + 1))
    typed[SlickAction[Effect.Write, List[Int]]](List(1, 2).traverse(i => (ts += i).map(identity)))
    typed[SlickAction[Effect.Write, List[Unit]]](List(1, 2).traverse(i => (ts += i).void))
    typed[SlickAction[Effect.Write, List[Int]]](DBIO.sequence(List(ts += 1, ts += 2)))
    typed[SlickAction[Effect.Write, Unit]](DBIO.seq(ts += 1, ts += 2))
    typed[SlickAction[Effect.Write with Effect.Read, (Int, Seq[Int])]]((ts += 1).zip(ts.result))
  }
}
