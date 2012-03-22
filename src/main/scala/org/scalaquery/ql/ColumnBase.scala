package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.ast._
import org.scalaquery.util._

/**
 * Common base trait for columns, tables and projections (but not unions and joins).
 */
trait ColumnBase[T] extends NodeGenerator with ValueLinearizer[T] with WithOp

/**
 * Base classs for columns.
 */
abstract class Column[T : TypeMapper] extends ColumnBase[T] {
  final val typeMapper = implicitly[TypeMapper[T]]
  def getLinearizedNodes = Vector(Node(this))
  def getAllColumnTypeMappers = Vector(typeMapper)
  def getResult(profile: BasicProfile, rs: PositionedResult): T = {
    val tmd = typeMapper(profile)
    tmd.nextValueOrElse(tmd.zero, rs)
  }
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = typeMapper(profile).updateValue(value, rs)
  final def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit = typeMapper(profile).setOption(value, ps)
  def orElse(n: =>T): Column[T] = new WrappedColumn[T](this) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): T = typeMapper(profile).nextValueOrElse(n, rs)
  }
  final def orFail = orElse { throw new SQueryException("Read NULL value for column "+this) }
  def ? : Column[Option[T]] = new WrappedColumn(this)(typeMapper.createOptionTypeMapper)

  def getOr[U](n: => U)(implicit ev: Option[U] =:= T): Column[U] = new WrappedColumn[U](this)(typeMapper.getBaseTypeMapper) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): U = typeMapper(profile).nextValueOrElse(n, rs)
  }
  def get[U](implicit ev: Option[U] =:= T): Column[U] = getOr[U] { throw new SQueryException("Read NULL value for column "+this) }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  // Functions which don't need an OptionMapper
  def in(e: Query[Column[_], _]) = ColumnOps.In(Node(this), Node(e))
  def notIn(e: Query[Column[_], _]) = ColumnOps.Not(Node(ColumnOps.In(Node(this), Node(e))))
  def count = StdFunction[Int]("count", Node(this))
  def isNull = ColumnOps.Is(Node(this), ConstColumn.NULL)
  def isNotNull = ColumnOps.Not(Node(ColumnOps.Is(Node(this), ConstColumn.NULL)))
  def countDistinct = ColumnOps.CountDistinct(Node(this))
  def asColumnOf[U : TypeMapper]: Column[U] = ColumnOps.AsColumnOf[U](Node(this), None)
  def asColumnOfType[U : TypeMapper](typeName: String): Column[U] = ColumnOps.AsColumnOf[U](Node(this), Some(typeName))

  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
final case class ConstColumn[T : TypeMapper](value: T) extends Column[T] with NullaryNode {
  override def toString = "ConstColumn["+SimpleTypeName.forVal(value)+"] "+value
  def bind = new BindColumn(value)
}

object ConstColumn {
  val NULL = new ConstColumn[Null](null)(TypeMapper.NullTypeMapper)
  val FALSE = new ConstColumn[Boolean](false)(TypeMapper.BooleanTypeMapper)
  val UNIT = new ConstColumn[Unit](())(TypeMapper.UnitTypeMapper)
}

/**
 * A column with a constant value which gets turned into a bind variable.
 */
final case class BindColumn[T : TypeMapper](value: T) extends Column[T] with NullaryNode {
  override def toString = "BindColumn["+SimpleTypeName.forVal(value)+"] "+value
}

/**
 * A parameter from a QueryTemplate which gets turned into a bind variable.
 */
final case class ParameterColumn[T : TypeMapper](idx: Int) extends Column[T] with NullaryNode {
  override def toString = "ParameterColumn "+idx
}

/**
 * A column which gets created as the result of applying an operator.
 */
abstract class OperatorColumn[T : TypeMapper] extends Column[T] {
  protected[this] val leftOperand: Node = Node(this)
}

/**
 * A WrappedColumn can be used to change a column's nullValue.
 */
sealed class WrappedColumn[T : TypeMapper](parent: ColumnBase[_]) extends Column[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
  protected[this] def nodeChildGenerators = Seq(nodeDelegate)
}

/**
 * A column which is part of a Table.
 */
final case class NamedColumn[T : TypeMapper](val table: Node, val name: String, val options: Seq[ColumnOption[T, _]])
  extends Column[T] with UnaryNode { //TODO should not be a Node
  def raw = RawNamedColumn(name, options, implicitly[TypeMapper[T]])
  override def nodeDelegate = new Wrapped(table, raw)
  val child = table
  override def toString = "NamedColumn " + name
  protected[this] override def nodeChildNames = Seq("table")
  protected[this] def nodeRebuild(child: Node): Node = copy[T](table = child)
}

final case class RawNamedColumn(name: String, options: Seq[ColumnOption[_, _]], typeMapper: TypeMapper[_]) extends NullaryNode {
  override def toString = "RawNamedColumn " + name
  def symbol = FieldSymbol(name)(Some(this))
}
