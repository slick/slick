package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.ast.{Drop, Take, Node, NullaryNode}

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_ <: ExtendedProfile]
}

trait ExtendedImplicitConversions[DriverType <: ExtendedProfile] extends BasicImplicitConversions[DriverType] {
  implicit def queryToExtendedQueryOps[E, U](q: Query[E, U]) = new ExtendedQueryOps(q)
  implicit def extendedQueryToDeleteInvoker[T](q: Query[ExtendedTable[T], T]): BasicDeleteInvoker[T] = new BasicDeleteInvoker(q, scalaQueryDriver)
}

class ExtendedQueryOps[E, U](q: Query[E, U]) {
  def take(num: Int): Query[E, U] = new WrappingQuery[E, U](Take(Node(q), num), q.unpackable)
  def drop(num: Int): Query[E, U] = new WrappingQuery[E, U](Drop(Node(q), num), q.unpackable)
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

abstract class AbstractExtendedTable[T](_schemaName: Option[String], _tableName: String) extends AbstractBasicTable[T](_schemaName, _tableName) {
  type ProfileType <: ExtendedProfile
  override val O: ExtendedColumnOptions = ExtendedColumnOptions
}

abstract class ExtendedTable[T](_schemaName: Option[String], _tableName: String) extends AbstractExtendedTable[T](_schemaName, _tableName) {
  def this(_tableName: String) = this(None, _tableName)
  type ProfileType = ExtendedProfile
}
