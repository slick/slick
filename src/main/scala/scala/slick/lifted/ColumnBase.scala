package scala.slick.lifted

import scala.language.existentials
import scala.slick.SlickException
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.{PositionedParameters, PositionedResult}
import scala.slick.ast._
import scala.slick.util._

/** Common base trait for all lifted values.
  */
trait Rep[T] extends NodeGenerator with WithOp { self: ValueLinearizer[T] => }

/** Common base trait for record values
  * (anything that is isomorphic to a tuple of scalar values).
  */
trait ColumnBase[T] extends Rep[T] with RecordLinearizer[T] with Typed

/** Base classs for columns.
  */
abstract class Column[T : TypedType] extends ColumnBase[T] with Typed {
  final val tpe = implicitly[TypedType[T]]
  def getLinearizedNodes = Vector(Node(this))
  def getAllColumnTypedTypes = Vector(tpe)
  def getResult(driver: JdbcDriver, rs: PositionedResult): T = {
    val tmd = driver.typeInfoFor(tpe)
    tmd.nextValueOrElse(
      if(tmd.nullable) tmd.zero else throw new SlickException("Read NULL value for column "+this),
      rs).asInstanceOf[T]
  }
  def updateResult(driver: JdbcDriver, rs: PositionedResult, value: T) = driver.typeInfoFor(tpe).updateValue(value, rs)
  final def setParameter(driver: JdbcDriver, ps: PositionedParameters, value: Option[T]): Unit = driver.typeInfoFor(tpe).setOption(value, ps)
  def orElse(n: =>T): Column[T] = new WrappedColumn[T](this) {
    override def getResult(driver: JdbcDriver, rs: PositionedResult): T =
      driver.typeInfoFor(tpe).nextValueOrElse(n, rs).asInstanceOf[T]
  }
  def orZero: Column[T] = new WrappedColumn[T](this) {
    override def getResult(driver: JdbcDriver, rs: PositionedResult): T = {
      val tmd = driver.typeInfoFor(tpe)
      tmd.nextValueOrElse(tmd.zero, rs).asInstanceOf[T]
    }
  }
  final def orFail = orElse { throw new SlickException("Read NULL value for column "+this) }
  def ? : Column[Option[T]] = Column.forNode(Node(this))(tpe.optionType)

  def getOr[U](n: => U)(implicit ev: Option[U] =:= T): Column[U] = new WrappedColumn[U](this)(tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[U]]) {
    override def getResult(driver: JdbcDriver, rs: PositionedResult): U = driver.typeInfoFor(tpe).nextValueOrElse(n, rs).asInstanceOf[U]
  }
  def get[U](implicit ev: Option[U] =:= T): Column[U] = getOr[U] { throw new SlickException("Read NULL value for column "+this) }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))
}

object Column {
  def forNode[T : TypedType](n: Node): Column[T] = new Column[T] {
    def nodeDelegate = if(op eq null) n else op.nodeDelegate
  }
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
final case class ConstColumn[T : TypedType](value: T) extends Column[T] with LiteralNode {
  override def toString = "ConstColumn["+SimpleTypeName.forVal(value)+"] "+value
  def bind = new BindColumn(value)
  def nodeRebuild = copy()
}

/**
 * A column with a constant value which gets turned into a bind variable.
 */
final case class BindColumn[T : TypedType](value: T) extends Column[T] with NullaryNode with LiteralNode {
  override def toString = "BindColumn["+SimpleTypeName.forVal(value)+"] "+value
  def nodeRebuild = copy()
}

/**
 * A parameter from a QueryTemplate which gets turned into a bind variable.
 */
final case class ParameterColumn[T : TypedType](extractor: (_ => T)) extends Column[T] with NullaryNode with TypedNode {
  def nodeRebuild = copy()
}

/**
 * A WrappedColumn can be used to change a column's nullValue.
 */
sealed abstract class WrappedColumn[T : TypedType](parent: Column[_]) extends Column[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
}
