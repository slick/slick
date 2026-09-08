package slick.cats.dbio

import scala.util.{Failure, Success}

import cats.{MonadError, StackSafeMonad}

import slick.dbio.{DBIO, DBIOBase}

/** cats type class instances for Slick actions.
  *
  * Two instances are provided because the compiler infers two different type constructors:
  *
  *  - A value typed as a full `DBIOAction[R, S, E]` (the result of a query, an insert, `DBIO.successful`,
  *    ...) infers `F = DBIOBase`. Scala 3 rejects the naive `[E] =>> DBIOAction[R, S, E]` because `E`
  *    is bounded, and then falls back to the base type `DBIOBase[R]`.
  *  - A value typed with the `DBIO[R]` alias infers `F = DBIO` and never reaches the base type.
  *
  * Results of cats combinators are `DBIOBase` (or `DBIO` when the inputs were typed as such); a
  * `DBIOBase` can be passed to `Database.run`, `flatMap`, for-comprehensions and every other place
  * that accepts a `DBIO`, thanks to the implicit view `DBIOBase.toDBIO`. Use `.toDBIO` explicitly
  * where an extension method such as `transactionally` is needed, since Scala does not chain two
  * implicit conversions.
  *
  * Effect and streaming information is dropped when going through cats, exactly as with `DBIO`.
  *
  * Import them with `import slick.cats.dbio.instances.*`.
  *
  * The `DBIO` instance is defined in the subtrait so that it wins when both instances apply, e.g.
  * `List[DBIO[Int]].sequence`, where `G` is fixed by a `<:<` evidence and both `DBIO` and its
  * supertype `DBIOBase` are valid solutions; the result is then the more useful `DBIO[List[Int]]`.
  */
trait DBIOInstances extends DBIOBaseInstances {

  implicit val slickMonadErrorForDBIO: MonadError[DBIO, Throwable] =
    new MonadError[DBIO, Throwable] with StackSafeMonad[DBIO] {
      def pure[A](a: A): DBIO[A] = DBIO.successful(a)

      def flatMap[A, B](fa: DBIO[A])(f: A => DBIO[B]): DBIO[B] = fa.flatMap(f)

      override def map[A, B](fa: DBIO[A])(f: A => B): DBIO[B] = fa.map(f)

      def raiseError[A](e: Throwable): DBIO[A] = DBIO.failed(e)

      def handleErrorWith[A](fa: DBIO[A])(f: Throwable => DBIO[A]): DBIO[A] =
        fa.asTry.flatMap {
          case Success(a) => DBIO.successful(a)
          case Failure(t) => f(t)
        }

      override def attempt[A](fa: DBIO[A]): DBIO[Either[Throwable, A]] = fa.asTry.map(_.toEither)
    }
}

/** Lower-priority instances, see [[DBIOInstances]]. */
trait DBIOBaseInstances {

  implicit val slickMonadErrorForDBIOBase: MonadError[DBIOBase, Throwable] =
    new MonadError[DBIOBase, Throwable] with StackSafeMonad[DBIOBase] {
      def pure[A](a: A): DBIOBase[A] = DBIO.successful(a)

      def flatMap[A, B](fa: DBIOBase[A])(f: A => DBIOBase[B]): DBIOBase[B] =
        fa.toDBIO.flatMap(a => f(a).toDBIO)

      override def map[A, B](fa: DBIOBase[A])(f: A => B): DBIOBase[B] = fa.toDBIO.map(f)

      def raiseError[A](e: Throwable): DBIOBase[A] = DBIO.failed(e)

      def handleErrorWith[A](fa: DBIOBase[A])(f: Throwable => DBIOBase[A]): DBIOBase[A] =
        fa.toDBIO.asTry.flatMap {
          case Success(a) => DBIO.successful(a)
          case Failure(t) => f(t).toDBIO
        }

      override def attempt[A](fa: DBIOBase[A]): DBIOBase[Either[Throwable, A]] =
        fa.toDBIO.asTry.map(_.toEither)
    }
}

/** `import slick.cats.dbio.instances.*` brings the instances into scope. */
object instances extends DBIOInstances
