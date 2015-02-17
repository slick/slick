package slick.util.iter

import slick.SlickException

/**
 * An iteratee for DB results.
 */
@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
sealed trait IterV[E, +A] {
  def runOption: Option[A]

  def run = runOption.getOrElse(throw new SlickException("Diverging iteratee"))

  def map[B](f: A => B): IterV[E, B]

  def flatMap[B](f: A => IterV[E, B]): IterV[E, B]
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
case class Done[E, +A](a: A, e: Input[E] = Empty) extends IterV[E, A] {
  def runOption = Some(a)

  def map[B](f: A => B): IterV[E, B] = Done(f(a), e)

  def flatMap[B](f: A => IterV[E, B]) = f(a) match {
    case Done(x, _) => Done(x, e)
    case Cont(k) => k(e)
  }
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
case class Cont[E, +A](k: Cont.K[E, A]) extends IterV[E, A] {
  def runOption = k(EOF) match {
    case Done(a, _) => Some(a)
    case Cont(_) => None
  }
  def map[B](f: A => B): IterV[E, B] = Cont { i => k(i).map(f) }
  def flatMap[B](f: A => IterV[E, B]) = Cont { i => k(i).flatMap(f) }
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
object Cont {
  type K[E, +A] = Input[E] => IterV[E, A]
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
sealed trait Input[+E] {
  def fold[R](el: E => R, empty: => R, eof: => R): R
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
case class El[+E](e: E) extends Input[E] {
  def fold[R](el: E => R, empty: => R, eof: => R): R = el(e)
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
case object Empty extends Input[Nothing] {
  def fold[R](el: Nothing => R, empty: => R, eof: => R): R = empty
}

@deprecated("Use Reactive Streams in the new Action-based API instead of Slick iteratees", "3.0")
case object EOF extends Input[Nothing] {
  def fold[R](el: Nothing => R, empty: => R, eof: => R): R = eof
}
