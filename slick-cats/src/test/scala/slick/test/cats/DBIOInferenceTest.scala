package slick.test.cats

import cats.MonadError
import cats.syntax.all.*
import munit.FunSuite

import slick.cats.dbio.instances.*
import slick.dbio.*

/** These tests pass if they compile: they show which type constructor the compiler infers for
  * cats syntax on Slick actions, without any explicit upcast. */
class DBIOInferenceTest extends FunSuite {

  /** Compiles only if `t` conforms to `T`. */
  private def typed[T](t: T): Unit = ()

  private def read: DBIOAction[Int, NoStream, Effect.Read] = DBIO.successful(1)
  private def write(i: Int): DBIOAction[String, NoStream, Effect.Write] = DBIO.successful(i.toString)
  private def plain: DBIO[Int] = read

  test("cats syntax on DBIOAction values infers DBIOBase, no upcast needed") {
    // https://github.com/slick/slick/issues/3665: with and without the explicit upcast
    val p0 = (0 to 10).toList.traverse { i => DBIO.successful(i).toDBIO }
    typed[DBIO[List[Int]]](p0)
    val p1 = (0 to 10).toList.traverse { i => DBIO.successful(i) }
    typed[DBIOBase[List[Int]]](p1)

    val p2 = (0 to 10).toList.traverse { i => read }
    typed[DBIOBase[List[Int]]](p2)

    val p3 = List(read, read).sequence
    typed[DBIOBase[List[Int]]](p3)

    val p4 = read.void
    typed[DBIOBase[Unit]](p4)

    val p5 = (read, write(1)).tupled
    typed[DBIOBase[(Int, String)]](p5)

    val p6 = read >> write(2)
    typed[DBIOBase[String]](p6)

    val p7 = (0 to 10).toList.traverse_ { i => write(i) }
    typed[DBIOBase[Unit]](p7)

    val p8 = List(read, read).foldM(0)((acc, a) => a.map(_ + acc))
    typed[DBIOBase[Int]](p8)
  }

  test("cats syntax on DBIO-typed values infers DBIO and keeps the alias") {
    val p1 = (0 to 10).toList.traverse { i => plain }
    typed[DBIO[List[Int]]](p1)

    val p2 = List(plain, plain).sequence
    typed[DBIO[List[Int]]](p2)

    val p3 = plain.void
    typed[DBIO[Unit]](p3)

    val p4 = (plain, plain).mapN(_ + _)
    typed[DBIO[Int]](p4)
  }

  test("the slick-cats 'Known Issues' cases compile") {
    def monad[F[_]: cats.Monad, A](fa: F[A]): F[A] = fa

    val fail1: DBIOAction[String, NoStream, Effect.All] = DBIO.successful("hello")
    val fail2 = DBIO.successful("hello")
    val success: DBIO[String] = DBIO.successful("hello")

    typed[DBIOBase[String]](monad(fail1))
    typed[DBIOBase[String]](monad(fail2))
    typed[DBIO[String]](monad(success))
  }

  test("cats results flow back into the ordinary DBIO API") {
    val fromCats: DBIOBase[List[Int]] = (0 to 10).toList.traverse { i => read }

    // as a DBIO, through the implicit view
    typed[DBIO[List[Int]]](fromCats)
    // explicitly
    typed[DBIO[List[Int]]](fromCats.toDBIO)
    // inside a for-comprehension mixing DBIOBase and DBIOAction
    val comp = for { xs <- fromCats; i <- read; s <- write(i) } yield (xs.sum + i, s)
    typed[DBIO[(Int, String)]](comp)
    // as an argument of an ordinary combinator
    typed[DBIO[List[Int]]](read.andThen(fromCats))
    typed[DBIO[List[Int]]](read.flatMap(_ => fromCats))
    // Database.run accepts it
    def run[R](a: DBIOAction[R, NoStream, Nothing]): R = ???
    val _ = () => run(fromCats)
  }

  test("effect tracking of the ordinary API is unchanged") {
    typed[DBIOAction[(Int, String), NoStream, Effect.Read & Effect.Write]](read.zip(write(1)))
    typed[DBIOAction[String, NoStream, Effect.Read & Effect.Write]](for { i <- read; s <- write(i) } yield s)
    typed[DBIOAction[Int, NoStream, Effect.Read]](read.map(_ + 1))
  }

  test("MonadError instances are summonable for both type constructors") {
    typed[MonadError[DBIOBase, Throwable]](MonadError[DBIOBase, Throwable])
    typed[MonadError[DBIO, Throwable]](MonadError[DBIO, Throwable])
    typed[cats.Monad[DBIOBase]](cats.Monad[DBIOBase])
    typed[cats.Monad[DBIO]](cats.Monad[DBIO])
  }
}
