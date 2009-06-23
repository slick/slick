package com.novocode.squery.session

import java.io.Closeable


/**
 * An Iterator with a close() method to close the underlying data source.
 */

trait CloseableIterator[+T] extends Iterator[T] with Closeable {

  override def close(): Unit
}
