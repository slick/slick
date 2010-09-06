package org.scalaquery

class SQueryException(msg: String, parent: Throwable) extends RuntimeException(msg, parent) {
  def this(msg: String) = this(msg, null)
}
