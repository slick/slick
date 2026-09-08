package slick.test.cats

import cats.MonadError
import cats.syntax.all.*
import munit.FunSuite

import slick.dbio.*

/** These tests pass if they compile. They cover what works on every supported Scala version
  * without importing any instances: cats syntax on values typed with the `DBIO` alias, and on
  * values explicitly upcast to `DBIOBase`. Scala 3 additionally infers `DBIOBase` for full
  * `DBIOAction` types, see `DBIOInferenceTest` in the `scala-3` test sources. */
class DBIOAliasInferenceTest extends FunSuite {

  /** Compiles only if `t` conforms to `T`. */
  private def typed[T](t: T): Unit = ()

  private def read: DBIOAction[Int, NoStream, Effect.Read] = DBIO.successful(1)
  private def plain: DBIO[Int] = read

  test("cats syntax on DBIO-typed values infers DBIO and keeps the alias") {
    val p1 = (0 to 10).toList.traverse { i => plain }
    typed[DBIO[List[Int]]](p1)

    val p2 = List(plain, plain).sequence
    typed[DBIO[List[Int]]](p2)

    val p3 = plain.void
    typed[DBIO[Unit]](p3)

    val p4 = (plain, plain).mapN(_ + _)
    typed[DBIO[Int]](p4)

    val p5 = plain >> plain
    typed[DBIO[Int]](p5)

    // the working case from slick-cats' README
    def monad[F[_]: cats.Monad, A](fa: F[A]): F[A] = fa
    val success: DBIO[String] = DBIO.successful("hello")
    typed[DBIO[String]](monad(success))
  }

  test("explicit upcasts to DBIOBase work on every Scala version") {
    val base: DBIOBase[Int] = read
    typed[DBIOBase[Unit]](base.void)
    typed[DBIOBase[List[Int]]](List(base, base).sequence)
    typed[DBIOBase[List[Int]]]((0 to 10).toList.traverse { i => read: DBIOBase[Int] })
  }

  test("DBIOBase results flow back into the ordinary DBIO API") {
    val base: DBIOBase[Int] = read
    typed[DBIO[Int]](base)
    typed[DBIO[Int]](base.toDBIO)
    typed[DBIO[Int]](for { a <- base; b <- plain } yield a + b)
    typed[DBIO[Int]](read.flatMap(_ => base))
  }

  test("MonadError instances are summonable for both type constructors") {
    typed[MonadError[DBIOBase, Throwable]](MonadError[DBIOBase, Throwable])
    typed[MonadError[DBIO, Throwable]](MonadError[DBIO, Throwable])
  }
}
