package com.novocode.squery

import com.novocode.squery.session.{Session, CloseableIterator}

/**
 * A parameterless invoker with some convenience methods.
 */
trait NoArgsInvoker[+R] extends Invoker[Unit, R] {

  final def apply()(implicit session: Session): R = apply(())

  final def firstOption(implicit session: Session): Option[R] = firstOption(())

  final def first()(implicit session: Session): R = first(())

  final def list()(implicit session: Session): List[R] = list(())

  final def foreach(f: R => Unit)(implicit session: Session): Unit = foreach((), f)

  final def foreach(f: R => Unit, maxRows: Int)(implicit session: Session): Unit = foreach((), f, maxRows)

  final def elements()(implicit session: Session): CloseableIterator[R] = elements(())
}
