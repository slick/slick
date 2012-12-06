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
  final def ~[U1,U2](p: Projection2[U1,U2]) =
    new Projection3(this,p._1,p._2)
  final def ~[U1,U2,U3](p: Projection3[U1,U2,U3]) =
    new Projection4(this,p._1,p._2,p._3)
  final def ~[U1,U2,U3,U4](p: Projection4[U1,U2,U3,U4]) =
    new Projection5(this,p._1,p._2,p._3,p._4)
  final def ~[U1,U2,U3,U4,U5](p: Projection5[U1,U2,U3,U4,U5]) =
    new Projection6(this,p._1,p._2,p._3,p._4,p._5)
  final def ~[U1,U2,U3,U4,U5,U6](p: Projection6[U1,U2,U3,U4,U5,U6]) =
    new Projection7(this,p._1,p._2,p._3,p._4,p._5,p._6)
  final def ~[U1,U2,U3,U4,U5,U6,U7](p: Projection7[U1,U2,U3,U4,U5,U6,U7]) =
    new Projection8(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8](p: Projection8[U1,U2,U3,U4,U5,U6,U7,U8]) =
    new Projection9(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9](p: Projection9[U1,U2,U3,U4,U5,U6,U7,U8,U9]) =
    new Projection10(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10](p: Projection10[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10]) =
    new Projection11(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11](p: Projection11[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11]) =
    new Projection12(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12](p: Projection12[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12]) =
    new Projection13(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13](p: Projection13[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13]) =
    new Projection14(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14](p: Projection14[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14]) =
    new Projection15(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15](p: Projection15[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15]) =
    new Projection16(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16](p: Projection16[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16]) =
    new Projection17(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17](p: Projection17[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17]) =
    new Projection18(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18](p: Projection18[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18]) =
    new Projection19(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19](p: Projection19[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19]) =
    new Projection20(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20](p: Projection20[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20]) =
    new Projection21(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19,p._20)
  final def ~[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21](p: Projection21[U1,U2,U3,U4,U5,U6,U7,U8,U9,U10,U11,U12,U13,U14,U15,U16,U17,U18,U19,U20,U21]) =
    new Projection22(this,p._1,p._2,p._3,p._4,p._5,p._6,p._7,p._8,p._9,p._10,p._11,p._12,p._13,p._14,p._15,p._16,p._17,p._18,p._19,p._20,p._21)

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
