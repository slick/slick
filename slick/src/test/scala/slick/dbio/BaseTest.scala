package slick.dbio

import cats.Monad
import slick.jdbc.H2Profile.api.*

class BaseTest extends munit.FunSuite {
  def monad[F[_] : Monad, A](fa: F[A]): F[A] = fa

  class T(tag: Tag) extends Table[Int](tag, "T") {
    def * = column[Int]("A")
  }
  val ts = TableQuery[T]

  def exactly[T] = new exactly[T]
  class exactly[T] {
    def apply[U](u: U)(implicit tu: U =:= T): T = u
  }
}
