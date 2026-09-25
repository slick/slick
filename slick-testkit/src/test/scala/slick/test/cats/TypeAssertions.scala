package slick.test.cats

/** Compile-time assertions about the static type of an expression. `typed` and `exactly` are not
  * satisfied through an implicit view such as `DBIOBase.toAction`, because the argument is typed
  * on its own and the relation is then checked with `<:<` / `=:=` evidence. */
object TypeAssertions {
  final class Typed[T] {
    def apply[U](u: U)(implicit ev: U <:< T): Unit = ()
  }
  final class Exactly[T] {
    def apply[U](u: U)(implicit ev: U =:= T): Unit = ()
  }

  /** Compiles only if the static type of the argument conforms to `T` without a conversion. */
  def typed[T]: Typed[T] = new Typed[T]

  /** Compiles only if the static type of the argument is `T`. */
  def exactly[T]: Exactly[T] = new Exactly[T]

  /** Compiles if the argument can be passed where a `T` is expected, including through an
    * implicit view. */
  def accepts[T](t: T): Unit = ()
}
