package slick.dbio

import cats.*
import cats.syntax.all.*

import scala.util.*

trait SlickActionInstances {
  private type Action[Effect <: slick.dbio.Effect, R] = SlickAction[NoStream, Effect, R]
  private type ActionMonad[Effect <: slick.dbio.Effect] =
    MonadError[({type L[A] = Action[Effect, A]})#L, Throwable] with StackSafeMonad[({type L[A] = Action[Effect, A]})#L]

  implicit def catsMonadInstanceForSlickAction[Effect <: slick.dbio.Effect]: ActionMonad[Effect] = new MonadError[({type L[A] = Action[Effect, A]})#L, Throwable] with StackSafeMonad[({type L[A] = Action[Effect, A]})#L] {
    override def pure[A](x: A) = DBIO.successful(x)

    override def map[A, B](fa: Action[Effect, A])(f: A => B) = fa.map(f)

    override def flatMap[A, B](fa: Action[Effect, A])(f: A => Action[Effect, B]) = fa.flatMap(f)

    override def raiseError[A](e: Throwable) = DBIO.failed(e)

    override def handleError[A](fea: Action[Effect, A])(f: Throwable => A) =
      fea.asTry.map {
        case Success(a) => a
        case Failure(t) => f(t)
      }

    override def handleErrorWith[A](fa: Action[Effect, A])(f: Throwable => Action[Effect, A]) =
      fa.asTry.flatMap {
        case Success(a) => DBIO.successful(a)
        case Failure(t) => f(t)
      }
  }

  implicit def catsMonoidInstanceForSlickAction[Effect <: slick.dbio.Effect, A: Monoid]: Monoid[Action[Effect, A]] =
    new CatsMonoidInstanceForSlickAction[Effect, A]

  implicit def catsSemigroupInstanceForSlickAction[Effect <: slick.dbio.Effect, A: Semigroup]: Semigroup[Action[Effect, A]] =
    new CatsSemigroupInstanceForSlickAction[Effect, A]

  private class CatsSemigroupInstanceForSlickAction[Effect <: slick.dbio.Effect, A: Semigroup] extends Semigroup[Action[Effect, A]] {
    override def combine(fx: Action[Effect, A], fy: Action[Effect, A]) =
      (fx zip fy).map { case (x, y) => x |+| y }
  }

  private class CatsMonoidInstanceForSlickAction[Effect <: slick.dbio.Effect, A](implicit A: Monoid[A]) extends CatsSemigroupInstanceForSlickAction[Effect, A] with Monoid[Action[Effect, A]] {
    def empty = DBIO.successful(A.empty)
  }
}
