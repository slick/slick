package com.novocode.squery.combinator.extended

import scala.math.{min, max}
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic._
import com.novocode.squery.session.Session

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_ <: ExtendedProfile]
}

trait ExtendedImplicitConversions[DriverType <: ExtendedProfile] extends BasicImplicitConversions[DriverType] {
  implicit def queryToExtendedQueryOps[E](q: Query[E]) = new ExtendedQueryOps(q)
  implicit def extendedQueryToDeleteInvoker[T](q: Query[ExtendedTable[T]]): BasicDeleteInvoker[T] = new BasicDeleteInvoker(q, squeryDriver)
}

class ExtendedQueryOps[E](q: Query[E]) {
  import ExtendedQueryOps._

  def take(num: Int) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(Some(t), d)) => TakeDrop(Some(min(t, num)), d)
    case Some(TakeDrop(None, d)) => TakeDrop(Some(num), d)
    case _ => TakeDrop(Some(num), None)
  }
  def drop(num: Int) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(Some(t), None)) => TakeDrop(Some(max(0, t-num)), Some(num))
    case Some(TakeDrop(None, Some(d))) => TakeDrop(None, Some(d+num))
    case Some(TakeDrop(Some(t), Some(d))) => TakeDrop(Some(max(0, t-num)), Some(d+num))
    case _ => TakeDrop(None, Some(num))
  }
}

object ExtendedQueryOps {
  final case class TakeDrop(take: Option[Int], drop: Option[Int]) extends QueryModifier with NullaryNode
}

class ExtendedColumnOptions extends BasicColumnOptions {
  val AutoInc = ExtendedColumnOption.AutoInc
}

object ExtendedColumnOptions extends ExtendedColumnOptions

object ExtendedColumnOption {
  case object AutoInc extends ColumnOption[Nothing, ExtendedProfile]
}

abstract class AbstractExtendedTable[T](_tableName: String) extends AbstractBasicTable[T](_tableName) {

  type ProfileType <: ExtendedProfile
  override val O: ExtendedColumnOptions = ExtendedColumnOptions
}

abstract class ExtendedTable[T](_tableName: String) extends AbstractExtendedTable[T](_tableName) {
  type ProfileType = ExtendedProfile
}
