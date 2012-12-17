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
abstract class Column[T : TypedType] extends ColumnBase[T] with Typed { self =>
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
  def ? : Column[Option[T]] = Column.forNode(OptionApply(Node(this)))(tpe.optionType)

  def getOrElse[U](default: => U)(implicit ev: Option[U] =:= T): Column[U] = {
    implicit val uType = tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[U]]
    new Column[U] {
      override def nodeDelegate = if(op eq null) GetOrElse(Node(self), () => default) else op.nodeDelegate
      override def getResult(driver: JdbcDriver, rs: PositionedResult): U =
        driver.typeInfoFor(tpe).nextValueOrElse(default, rs).asInstanceOf[U]
    }
  }
  def get[U](implicit ev: Option[U] =:= T): Column[U] = getOrElse[U] { throw new SlickException("Read NULL value for column "+this) }

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
