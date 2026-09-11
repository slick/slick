package slick

/** The `dbio` package contains the Database I/O Action implementation.
 * See [[SlickAction]] for details. */
package object dbio {
  /** Simplified type for a streaming [[SlickAction]] without effect tracking */
  type StreamingDBIO[+R, +T] = SlickAction[Streaming[T], Effect.All, R]

  /** Simplified type for a [[SlickAction]] without streaming or effect tracking */
  type DBIO[+R] = SlickAction[NoStream, Effect.All, R]
  val DBIO = SlickAction

  /** Alias for [[SlickAction]] for compatibility with Slick3 code.
   * Doesn't allow the use of cats instances defined for [[SlickAction]] */
  type DBIOAction[+R, +S <: NoStream, -E <: Effect] = SlickAction[S, E, R]
  val DBIOAction = SlickAction
}
