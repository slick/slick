package slick

import scala.util.control.NonFatal

/** Helper code for various things. Tuples, Logging, SQL, ... */
package object util {

  /** Throw an `UnsupportedOperationException`. Like `???` but `NonFatal`. */
  def ?? = throw new UnsupportedOperationException

  /** An exception handler which ignores `NonFatal` exceptions. It is used when running cleanup
    * code inside of another exception handler to prevent an exception during cleanup from
    * overriding the original one. */
  val ignoreFollowOnError: PartialFunction[Throwable, Unit] = {
    case NonFatal(_) => ()
  }
}
