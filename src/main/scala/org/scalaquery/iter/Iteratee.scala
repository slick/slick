package org.scalaquery.iter

import org.scalaquery.SQueryException

/**
 * An iteratee for DB results.
 */

sealed trait IterV[E, +A] {
  def runOption: Option[A]
  def run = runOption.getOrElse(throw new SQueryException("Diverging iteratee"))
  def map[B](f: A => B): IterV[E, B]
  def flatMap[B](f: A => IterV[E, B]): IterV[E, B]
}

case class Done[E, +A](a: A, e: Input[E] = Empty) extends IterV[E, A] {
  def runOption = Some(a)
  def map[B](f: A => B): IterV[E, B] = Done(f(a), e)
  def flatMap[B](f: A => IterV[E, B]) = f(a) match {
    case Done(x, _) => Done(x, e)
    case Cont(k) => k(e)
  }
}

case class Cont[E, +A](k: Cont.K[E, A]) extends IterV[E, A] {
  def runOption = k(EOF) match {
    case Done(a, _) => Some(a)
    case Cont(_) => None
  }
  def map[B](f: A => B): IterV[E, B] = Cont { i => k(i).map(f) }
  def flatMap[B](f: A => IterV[E, B]) = Cont { i => k(i).flatMap(f) }
}

object Cont {
  type K[E, +A] = Input[E] => IterV[E, A]
}

sealed trait Input[+E] {
  def fold[R](el: E => R, empty: => R, eof: => R): R
}

case class El[+E](e: E) extends Input[E] {
  def fold[R](el: E => R, empty: => R, eof: => R): R = el(e)
}

case object Empty extends Input[Nothing] {
  def fold[R](el: Nothing => R, empty: => R, eof: => R): R = eof
}

case object EOF extends Input[Nothing] {
  def fold[R](el: Nothing => R, empty: => R, eof: => R): R = eof
}
