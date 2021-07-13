package com.typesafe.slick.testkit.util

import scala.compiletime.testing._

/**
 * A macro that ensures that a code snippet does not typecheck.
 */
object ShouldNotTypecheck {
  inline def apply(code: String): Unit = assert(!typeChecks(code))
  inline def apply(code: String, expected: String): Unit = apply(code)
}
