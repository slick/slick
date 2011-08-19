package org.scalaquery.util

import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedParameters, PositionedResult}

/**
 * Converts between unpacked (e.g. in query results) and linearized (a
 * sequence of columns) form of values.
 */

trait ValueLinearizer[T] {
  def getResult(profile: BasicProfile, rs: PositionedResult): T
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T): Unit
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit
  def getLinearizedNodes: IndexedSeq[Node]
}

class ProductLinearizer(sub: IndexedSeq[ValueLinearizer[_]]) extends ValueLinearizer[Product] {

  def getLinearizedNodes: IndexedSeq[Node] =
    (0 until sub.length).flatMap(i => sub(i).asInstanceOf[ValueLinearizer[Any]].getLinearizedNodes)(collection.breakOut)

  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[Product]) =
    for(i <- 0 until sub.length)
      sub(i).asInstanceOf[ValueLinearizer[Any]].setParameter(profile, ps, value.map(_.productElement(i)))

  def updateResult(profile: BasicProfile, rs: PositionedResult, value: Product) =
    for(i <- 0 until sub.length)
      sub(i).asInstanceOf[ValueLinearizer[Any]].updateResult(profile, rs, value.productElement(i))

  def getResult(profile: BasicProfile, rs: PositionedResult): Product = {
    var i = -1
    def f = { i += 1; sub(i).getResult(profile, rs) }
    sub.length match {
      case 2 => (f,f)
      case 3 => (f,f,f)
      case 4 => (f,f,f,f)
      case 5 => (f,f,f,f,f)
      case 6 => (f,f,f,f,f,f)
      case 7 => (f,f,f,f,f,f,f)
      case 8 => (f,f,f,f,f,f,f,f)
      case 9 => (f,f,f,f,f,f,f,f,f)
      case 10 => (f,f,f,f,f,f,f,f,f,f)
      case 11 => (f,f,f,f,f,f,f,f,f,f,f)
      case 12 => (f,f,f,f,f,f,f,f,f,f,f,f)
      case 13 => (f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 14 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 15 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 16 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 17 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 18 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 19 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 20 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 21 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
      case 22 => (f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f)
    }
  }
}
