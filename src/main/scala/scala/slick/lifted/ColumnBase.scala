package scala.slick.lifted

import scala.language.existentials
import scala.slick.SlickException
import scala.slick.driver.BasicProfile
import scala.slick.session.{PositionedResult, PositionedParameters}
import scala.slick.ast._
import scala.slick.util._

/** Common base trait for all lifted values.
  */
trait Rep[T] extends NodeGenerator with WithOp { self: ValueLinearizer[T] => }

/** Common base trait for record values
  * (anything that is isomorphic to a tuple of scalar values).
  */
trait ColumnBase[T] extends Rep[T] with RecordLinearizer[T]

/** Base classs for columns.
  */
abstract class Column[T : TypeMapper] extends ColumnBase[T] with Typed {
  final val typeMapper = implicitly[TypeMapper[T]]
  final def tpe = typeMapper
  def getLinearizedNodes = Vector(Node(this))
  def getAllColumnTypeMappers = Vector(typeMapper)
  def getResult(profile: BasicProfile, rs: PositionedResult): T = {
    val tmd = typeMapper(profile)
    tmd.nextValueOrElse(
      if(tmd.nullable) tmd.zero else throw new SlickException("Read NULL value for column "+this),
      rs)
  }
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = typeMapper(profile).updateValue(value, rs)
  final def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit = typeMapper(profile).setOption(value, ps)
  def orElse(n: =>T): Column[T] = new WrappedColumn[T](this) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): T = typeMapper(profile).nextValueOrElse(n, rs)
  }
  def orZero: Column[T] = new WrappedColumn[T](this) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): T = {
      val tmd = typeMapper(profile)
      tmd.nextValueOrElse(tmd.zero, rs)
    }
  }
  final def orFail = orElse { throw new SlickException("Read NULL value for column "+this) }
  def ? : Column[Option[T]] = new WrappedColumn(this)(typeMapper.createOptionTypeMapper)

  def getOr[U](n: => U)(implicit ev: Option[U] =:= T): Column[U] = new WrappedColumn[U](this)(typeMapper.getBaseTypeMapper) {
    override def getResult(profile: BasicProfile, rs: PositionedResult): U = typeMapper(profile).nextValueOrElse(n, rs)
  }
  def get[U](implicit ev: Option[U] =:= T): Column[U] = getOr[U] { throw new SlickException("Read NULL value for column "+this) }
  final def ~[U](b: Column[U]) = new Projection2[T, U](this, b)

  def asc = ColumnOrdered[T](this, Ordering())
  def desc = ColumnOrdered[T](this, Ordering(direction = Ordering.Desc))
}

/**
 * A column with a constant value which is inserted into an SQL statement as a literal.
 */
final case class ConstColumn[T : TypeMapper](value: T) extends Column[T] with LiteralNode {
  override def toString = "ConstColumn["+SimpleTypeName.forVal(value)+"] "+value
  def bind = new BindColumn(value)
}

object ConstColumn {
  val NULL = new ConstColumn[Null](null)(TypeMapper.NullTypeMapper)
  val TRUE = new ConstColumn[Boolean](true)(TypeMapper.BooleanTypeMapper)
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
final case class ParameterColumn[T : TypeMapper](linearIdx: Int, extractor: (_ => T)) extends Column[T] with NullaryNode

/**
 * A WrappedColumn can be used to change a column's nullValue.
 */
sealed class WrappedColumn[T : TypeMapper](parent: Column[_]) extends Column[T] {
  override def nodeDelegate = if(op eq null) Node(parent) else op.nodeDelegate
  val nodeChildren = Seq(nodeDelegate)
}

abstract class ColumnOption[+T]

object ColumnOption {
  case object NotNull extends ColumnOption[Nothing]
  case object Nullable extends ColumnOption[Nothing]
  case object PrimaryKey extends ColumnOption[Nothing]
  case class Default[T](val defaultValue: T) extends ColumnOption[T]
  case class DBType(val dbType: String) extends ColumnOption[Nothing]
  case object AutoInc extends ColumnOption[Nothing]
}
