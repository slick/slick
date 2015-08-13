package slick

/** The `dbio` package contains the Database I/O Action implementation.
  * See [[DBIOAction]] for details. */
package object dbio {
  /** Simplified type for a streaming [[DBIOAction]] without effect tracking */
  type StreamingDBIO[+R, +T] = DBIOAction[R, Streaming[T], Effect.All]

  /** Simplified type for a [[DBIOAction]] without streaming or effect tracking */
  type DBIO[+R] = DBIOAction[R, NoStream, Effect.All]
  val DBIO = DBIOAction
}
