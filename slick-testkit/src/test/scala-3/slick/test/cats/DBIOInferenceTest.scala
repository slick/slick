package slick.test.cats

import cats.MonadError
import cats.syntax.all.*
import munit.FunSuite

import slick.dbio.*
import slick.test.cats.TypeAssertions.*

/** These tests pass if they compile: they show which type constructor Scala 3 infers for cats
  * syntax on Slick actions, without any explicit upcast and without importing any instances.
  *
  * Scala 3 only: Scala 2 does not fall back to the `DBIOBase` base type when inferring `F[_]`
  * from a `DBIOAction`. See `DBIOAliasInferenceTest` for what works on every Scala version.
  *
  * Note: this file must not be compiled with `-source:3.0-migration`, which disables the fallback. */
class DBIOInferenceTest extends FunSuite {
  private def monad[F[_]: cats.Monad, A](fa: F[A]): F[A] = fa

  private def read: DBIOAction[Int, NoStream, Effect.Read] = DBIO.successful(1)
  private def write(i: Int): DBIOAction[String, NoStream, Effect.Write] = DBIO.successful(i.toString)

  test("cats syntax on DBIOAction values infers DBIOBase[E, *] and keeps the effect") {
    // https://github.com/slick/slick/issues/3665
    exactly[DBIOBase[Effect.Read, List[Int]]]((0 to 10).toList.traverse { i => read })
    exactly[DBIOBase[Effect.Read, List[Int]]](List(read, read).sequence)
    exactly[DBIOBase[Effect.Write, Unit]]((0 to 10).toList.traverse_ { i => write(i) })
    exactly[DBIOBase[Effect.Read, Int]](monad(read))
    // values typed with an alias keep the alias
    exactly[SlickAction[Effect, List[Int]]]((0 to 10).toList.traverse { i => DBIO.successful(i) })
    exactly[SlickAction[Effect.Read, Int]](List(read, read).foldM(0)((acc, a) => a.map(_ + acc)))
  }

  test("mixed effects infer the intersection where F is not fixed by a receiver") {
    exactly[DBIOBase[Effect.Read & Effect.Write, (Int, String)]]((read, write(1)).tupled)
    exactly[DBIOBase[Effect.Read & Effect.Write, String]](read *> write(1))
    exactly[DBIOBase[Effect.Read & Effect.Write, List[Int | String]]](List(read, write(1)).sequence)
    // Slick's own >> is a member and intersects the effects too
    typed[DBIOAction[String, NoStream, Effect.Read & Effect.Write]](read >> write(1))
    // where cats fixes F from the receiver, widen the receiver to DBIO, which accepts every effect
    exactly[DBIO[Int]]((read: DBIO[Int]).flatTap(_ => write(1)))
  }

  test("the slick-cats 'Known Issues' cases compile") {
    val fail1: DBIOAction[String, NoStream, Effect.All] = DBIO.successful("hello")
    val fail2 = DBIO.successful("hello")
    val success: DBIO[String] = DBIO.successful("hello")
    exactly[DBIOBase[Effect.All, String]](monad(fail1))
    exactly[SlickAction[Effect, String]](monad(fail2))
    exactly[DBIO[String]](monad(success))
  }

  test("cats results flow back into the ordinary DBIO API with their effect") {
    val fromCats: DBIOBase[Effect.Read, List[Int]] = (0 to 10).toList.traverse { i => read }
    // through the implicit view
    accepts[SlickAction[Effect.Read, List[Int]]](fromCats)
    accepts[DBIO[List[Int]]](fromCats)
    // explicitly
    typed[SlickAction[Effect.Read, List[Int]]](fromCats.toAction)
    // inside a for-comprehension mixing DBIOBase and DBIOAction: Slick's flatMap, effects intersected
    typed[DBIOAction[(Int, String), NoStream, Effect.Read & Effect.Write]](
      for { xs <- fromCats; i <- read; s <- write(i) } yield (xs.sum + i, s)
    )
    // as an argument of an ordinary combinator
    typed[DBIOAction[List[Int], NoStream, Effect.Read]](read.andThen(fromCats))
    typed[DBIOAction[List[Int], NoStream, Effect.Read]](read.flatMap(_ => fromCats))
    // Database.run accepts it
    def run[R](a: DBIOAction[R, NoStream, Nothing]): Unit = ()
    run(fromCats)
  }

  test("effect tracking of the ordinary API is unchanged") {
    typed[DBIOAction[(Int, String), NoStream, Effect.Read & Effect.Write]](read.zip(write(1)))
    typed[DBIOAction[String, NoStream, Effect.Read & Effect.Write]](for { i <- read; s <- write(i) } yield s)
    typed[DBIOAction[Int, NoStream, Effect.Read]](read.map(_ + 1))
  }

  test("MonadError instances are summonable for both type constructors and any effect") {
    typed[MonadError[[A] =>> DBIOBase[Effect.Read, A], Throwable]](
      MonadError[[A] =>> DBIOBase[Effect.Read, A], Throwable]
    )
    typed[MonadError[[A] =>> SlickAction[Effect.Write, A], Throwable]](
      MonadError[[A] =>> SlickAction[Effect.Write, A], Throwable]
    )
    typed[MonadError[DBIO, Throwable]](MonadError[DBIO, Throwable])
    typed[cats.Monad[DBIO]](cats.Monad[DBIO])
  }
}
