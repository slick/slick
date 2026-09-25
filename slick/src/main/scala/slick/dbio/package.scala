package slick

/** The `dbio` package contains the Database I/O Action implementation.
  * See [[DBIOAction]] for details. */
package object dbio {
  /** Simplified type for a streaming [[DBIOAction]] without effect tracking */
  type StreamingDBIO[+R, +T] = DBIOAction[R, Streaming[T], Effect.All]

  /** Simplified type for a [[DBIOAction]] without streaming or effect tracking */
  type DBIO[+R] = DBIOAction[R, NoStream, Effect.All]

  /** Simplified type for a [[DBIOAction]] without streaming but with effect tracking. The result
    * type is the last parameter, so that `DBIOEffect[E, *]` can be the `F[_]` of a type class such
    * as `cats.Monad`; `DBIO[R]` is `DBIOEffect[Effect.All, R]`. See [[DBIOBase]]. */
  type DBIOEffect[-E <: Effect, +R] = DBIOAction[R, NoStream, E]
  val DBIO = DBIOAction
}
