package slick.dbio

import cats.Monad
import cats.syntax.all.*
import slick.jdbc.H2Profile

// check that instances are picked up when using either DBIO or SlickAction while preserving Effect type
class SlickActionInferenceTest extends munit.FunSuite {
  def monad[F[_] : Monad, A](fa: F[A]): F[A] = fa

  test("the slick-cats 'Known Issues' cases compile") {
    // SlickAction replaces DBIOAction
    val fail1: SlickAction[NoStream, Effect.All, String] = DBIO.successful("hello")
    val fail2 = DBIO.successful("hello")
    val success: DBIO[String] = DBIO.successful("hello")

    monad(fail1)
    monad(fail2)
    monad(success)
  }

  val action = DBIO.successful("hello")
  val io: DBIO[String] = action

  test("specific effect is preserved") {
    monad(action.flatMap(_ => action.transactionally)): SlickAction[NoStream, Effect.Transactional, String]
  }

  test("traverse picks up Monad") {
    List(1).traverse { i => action }
    List(1).traverse { i => io }
    List(1).traverse { i => action.transactionally }
  }

  test("cats syntax works") {
    action >>= (_ => action)
    io >>= (_ => action)
    action |+| action
    io |+| action
  }

  test("profile actions also work") {
    import H2Profile.api.*
    class T(tag: Tag) extends Table[Int](tag, "T") {
      def a = column[Int]("A");

      def * = a
    }
    val ts = TableQuery[T]

    // todo should not need explicit cast
    def monadDBIO[E <: Effect, A](d: SlickAction[NoStream, E, A]) = monad(d)

    monadDBIO(ts.result)
    monadDBIO(ts += 1)
    monadDBIO(ts.schema.create)
    monadDBIO(ts.result.transactionally)
    monadDBIO(ts.update(1))
  }
}

