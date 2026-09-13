package slick.test.cats

import scala.util.Try

import cats.MonadError
import cats.syntax.all.*
import munit.FunSuite

import slick.dbio.*
import slick.test.cats.TypeAssertions.*

/** These tests pass if they compile. They cover what works on every supported Scala version
  * without importing any instances: cats syntax on values typed with the `SlickAction` or `DBIO`
  * alias, on the results of Slick's standard combinators, on values converted with `toAction`,
  * and on values explicitly typed as `DBIOBase`. Scala 3 additionally infers `DBIOBase[E, *]`
  * for full `DBIOAction` types, see `DBIOInferenceTest` in the `scala-3` test sources. */
class DBIOAliasInferenceTest extends FunSuite {
  private def monad[F[_]: cats.Monad, A](fa: F[A]): F[A] = fa

  private def read: DBIOAction[Int, NoStream, Effect.Read] = DBIO.successful(1)
  private def write(i: Int): DBIOAction[String, NoStream, Effect.Write] = DBIO.successful(i.toString)
  private def plain: DBIO[Int] = read
  private def r: SlickAction[Effect.Read, Int] = read

  test("cats syntax on DBIO-typed values infers DBIO and keeps the alias") {
    typed[DBIO[List[Int]]]((0 to 10).toList.traverse { i => plain })
    typed[DBIO[List[Int]]](List(plain, plain).sequence)
    typed[DBIO[Int]]((plain, plain).mapN(_ + _))
    typed[DBIO[(Int, Int)]]((plain, plain).tupled)
    typed[DBIO[Int]](plain >> plain)
    // the working case from slick-cats' README
    val success: DBIO[String] = DBIO.successful("hello")
    typed[DBIO[String]](monad(success))
  }

  test("cats syntax on SlickAction-typed values keeps the effect") {
    typed[SlickAction[Effect.Read, List[Int]]]((0 to 10).toList.traverse { i => r })
    typed[SlickAction[Effect.Read, List[Int]]](List(r, r).sequence)
    typed[SlickAction[Effect.Read, (Int, Int)]]((r, r).tupled)
    typed[SlickAction[Effect.Read, Int]](monad(r))
    // Effect.All extends every effect, so an action of any effect is a DBIO (and a DBIOBase[Effect.All, *])
    typed[DBIO[Int]](r)
    typed[DBIOBase[Effect.All, Int]](r)
  }

  test("the results of the standard combinators are SlickAction-typed and therefore monadic") {
    // the failing cases from slick-cats' README and #3665
    typed[SlickAction[Effect, String]](monad(DBIO.successful("hello")))
    typed[SlickAction[Effect, List[Int]]]((0 to 10).toList.traverse { i => DBIO.successful(i) })
    // combinator results on effect-typed actions
    typed[SlickAction[Effect.Read, List[Int]]]((0 to 10).toList.traverse { i => read.map(_ + i) })
    typed[SlickAction[Effect.Read, List[Int]]](monad(DBIO.sequence(List(read, read))))
    typed[SlickAction[Effect.Read, Int]](monad(DBIO.fold(Seq(read, read), 0)(_ + _)))
    typed[SlickAction[Effect.Read with Effect.Write, (Int, String)]](monad(read.zip(write(1))))
    typed[SlickAction[Effect.Read, Try[Int]]](monad(read.asTry))
    typed[SlickAction[Effect.Read, Unit]](monad(read.void))
  }

  test("toAction converts any action into a SlickAction with the same effect") {
    typed[SlickAction[Effect.Read, Int]](read.toAction)
    typed[SlickAction[Effect.Read, List[Int]]]((0 to 10).toList.traverse { i => read.toAction })
  }

  test("explicit DBIOBase types work on every Scala version") {
    val base: DBIOBase[Effect.Read, Int] = read
    typed[DBIOBase[Effect.Read, Unit]](base.void)
    typed[DBIOBase[Effect.Read, List[Int]]](List(base, base).sequence)
    typed[DBIOBase[Effect.Read, List[Int]]]((0 to 10).toList.traverse { i => read: DBIOBase[Effect.Read, Int] })
  }

  test("DBIOBase results flow back into the ordinary API with their effect") {
    val base: DBIOBase[Effect.Read, List[Int]] = List(r, r).sequence
    // through the implicit view
    accepts[SlickAction[Effect.Read, List[Int]]](base)
    accepts[DBIO[List[Int]]](base)
    // explicitly
    typed[SlickAction[Effect.Read, List[Int]]](base.toAction)
    // Slick's combinators are members of DBIOBase and intersect the effects
    typed[SlickAction[Effect.Read, Int]](base.map(_.sum))
    typed[DBIOAction[String, NoStream, Effect.Read with Effect.Write]](base.flatMap(xs => write(xs.sum)))
    typed[DBIOAction[String, NoStream, Effect.Read with Effect.Write]](base >> write(1))
    typed[DBIOAction[String, NoStream, Effect.Read with Effect.Write]](base.andThen(write(1)))
    typed[DBIOAction[(Int, String), NoStream, Effect.Read with Effect.Write]](
      for { xs <- base; i <- read; s <- write(xs.sum + i) } yield (i, s)
    )
    // as an argument or lambda result of an ordinary combinator, through the view
    typed[DBIOAction[List[Int], NoStream, Effect.Read with Effect.Write]](write(1) >> base)
    typed[DBIOAction[List[Int], NoStream, Effect.Read]](read.flatMap(_ => base))
    // extension methods of DBIOAction need the explicit conversion
    typed[DBIOAction[List[Int], NoStream, Effect.Read with Effect.Transactional]](base.toAction.transactionally)
    // Database.run accepts it
    def run[R](a: DBIOAction[R, NoStream, Nothing]): Unit = ()
    run(base)
  }

  test("MonadError instances are summonable for every effect") {
    type ReadAction[A] = SlickAction[Effect.Read, A]
    type ReadBase[A] = DBIOBase[Effect.Read, A]
    typed[MonadError[DBIO, Throwable]](MonadError[DBIO, Throwable])
    typed[MonadError[ReadAction, Throwable]](MonadError[ReadAction, Throwable])
    typed[MonadError[ReadBase, Throwable]](MonadError[ReadBase, Throwable])
    typed[cats.Monad[DBIO]](cats.Monad[DBIO])
  }
}
