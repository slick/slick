package com.novocode.squery.session

import java.io.Closeable


/**
 * An Iterator with a close() method to close the underlying data source.
 */

trait CloseableIterator[+T] extends Iterator[T] with Closeable {

  override def close(): Unit

  final def use[R](f: (Iterator[T] => R)): R =
    try f(this) finally close()

  final def use[R](f: =>R): R =
    try f finally close()
}
