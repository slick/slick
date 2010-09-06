package org.scalaquery.session


/**
 * An iterator on top of a data source which does not offer a hasNext()
 * method without doing a next()
 */

trait ReadAheadIterator[+T] extends Iterator[T] {

  private[this] var cached: Option[T] = null

  protected def fetchNext(): Option[T]

  def peek(): Option[T] = {
    if(cached eq null) cached = fetchNext()
    cached
  }

  def hasNext = peek().isDefined

  def next() = peek() match {
    case None => throw new NoSuchElementException("next on empty iterator");
    case Some(x) => { cached = null; x }
  }
}
