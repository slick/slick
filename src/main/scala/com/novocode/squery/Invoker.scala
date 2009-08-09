package com.novocode.squery

import scala.collection.mutable.ListBuffer
import com.novocode.squery.session.{Session, CloseableIterator}

/**
 * Base trait for all statement invokers, using parameter type P and result type R.
 */
trait Invoker[-P, +R] { self =>

  def foreach(param: P, f: R => Unit, maxRows: Int)(implicit session: Session): Unit

  def elements(param: P)(implicit session: Session): CloseableIterator[R]

  final def apply(param: P)(implicit session: Session): R = first(param)

  final def firstOption(param: P)(implicit session: Session): Option[R] = {
    var res: Option[R] = None
    foreach(param, { x => res = Some(x) }, 1)
    res
  }

  final def first(param: P)(implicit session: Session): R = firstOption(param) match {
    case None => throw new Predef.NoSuchElementException("Invoker.first")
    case Some(res) => res
  }

  final def list(param: P)(implicit session: Session): List[R] = {
    val b = new ListBuffer[R]
    foreach(param, { x => b += x }, 0)
    b.toList
  }

  final def foreach(param: P, f: R => Unit)(implicit session: Session): Unit = foreach(param, f, 0)

  final def foldLeft[B](param: P, z: B)(op: (B, R) => B)(implicit session: Session): B = {
    var _z = z
    foreach(param, { e => _z = op(_z, e) })(session)
    _z
  }

  final def withParameter(fixedParam: P): NoArgsInvoker[R] = new NoArgsInvoker[R] {
    def foreach(param: Unit, f: R => Unit, maxRows: Int)(implicit session: Session): Unit =
      self.foreach(fixedParam, f, maxRows)
    def elements(param: Unit)(implicit session: Session): CloseableIterator[R] =
      self.elements(fixedParam)
  }
}

object Invoker {

  implicit def optionInvokerOperations[P,R](inv: Invoker[P,Option[R]]) = new {
    def firstFlatten(param: P)(implicit session: Session): Option[R] = inv.firstOption(param) match {
      case None => None
      case Some(None) => None
      case Some(x) => x
    }
  }

  implicit def optionNoArgsInvokerOperations[R](inv: NoArgsInvoker[Option[R]]) = new {
    def firstFlatten(implicit session: Session): Option[R] = inv.firstOption(session) match {
      case None => None
      case Some(None) => None
      case Some(x) => x
    }
  }
}
