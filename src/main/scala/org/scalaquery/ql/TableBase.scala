package org.scalaquery.ql

import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.{BasicProfile, BasicQueryTemplate, BasicDriver}
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.util.{Node, UnaryNode, BinaryNode, WithOp}

sealed trait TableBase[T] extends Node with WithOp {
  override def isNamedTable = true
}

abstract class AbstractTable[T](val schemaName: Option[String], val tableName: String) extends TableBase[T] with ColumnBase[T] {

  final type TableType = T
  def nodeChildren = Nil
  override def toString = "Table " + tableName

  def * : ColumnBase[T]

  def create_* : Iterable[NamedColumn[_]] = {
    def f(n:Node): Iterable[NamedColumn[_]] = n match {
      case p:Projection[_] =>
        0 until p.productArity map (n => Node(p.productElement(n)) match {
          case c: NamedColumn[_] => c
          case c => throw new SQueryException("Cannot use column "+c+" in "+tableName+".* for CREATE TABLE statement")
        })
      case n:NamedColumn[_] => Iterable(n)
      case _ => throw new SQueryException("Cannot use "+tableName+".* for CREATE TABLE statement")
    }
    f(Node(*))
  }

  def foreignKey[P, PU, TT <: AbstractTable[_], U]
      (name: String, sourceColumns: P, targetTable: TT)
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
        onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Unpack[TT, U], unpackp: Unpack[P, PU]): ForeignKeyQuery[TT, U] = {
    val mappedTTU = Unpackable(targetTable.mapOp(tt => AbstractTable.Alias(Node(tt))), unpack)
    new ForeignKeyQuery(List(new ForeignKey(name, this, mappedTTU, targetTable, unpackp,
      sourceColumns, targetColumns, onUpdate, onDelete)), mappedTTU)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit unpack: Unpack[T, _]): PrimaryKey = PrimaryKey(name, unpack.linearizer(sourceColumns).getLinearizedNodes)

  def tableConstraints: Iterable[Constraint] = for {
      m <- getClass().getMethods.view
      if m.getParameterTypes.length == 0 &&
        (m.getReturnType == classOf[ForeignKeyQuery[_ <: AbstractTable[_], _]]
         || m.getReturnType == classOf[PrimaryKey])
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey[_ <: AbstractTable[_], _]] =
    tableConstraints collect { case q: ForeignKeyQuery[_, _] => q.fks } flatten

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints collect { case k: PrimaryKey => k }

  def index[T](name: String, on: T, unique: Boolean = false)(implicit unpack: Unpack[T, _]) = new Index(name, this, unpack.linearizer(on).getLinearizedNodes, unique)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index])

  def getLinearizedNodes = *.getLinearizedNodes
  def getResult(profile: BasicProfile, rs: PositionedResult) = *.getResult(profile, rs)
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = *.updateResult(profile, rs, value)
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]) = *.setParameter(profile, ps, value)
}

object AbstractTable {
  def unapply[T](t: AbstractTable[T]) = Some(t.tableName)

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "AbstractTable.Alias"
    override def isNamedTable = true
  }
}

final class JoinBase[+T1 <: AbstractTable[_], +T2 <: TableBase[_]](_left: T1, _right: T2, joinType: Join.JoinType) {
  def nodeChildren = Node(_left) :: Node(_right) :: Nil
  override def toString = "JoinBase(" + Node(_left) + "," + Node(_right) + ")"
  def on[T <: Column[_] : CanBeQueryCondition](pred: (T1, T2) => T) = new Join(_left, _right, joinType, Node(pred(_left, _right)))
}

final class Join[+T1 <: AbstractTable[_], +T2 <: TableBase[_]](_left: T1, _right: T2,
  val joinType: Join.JoinType, val on: Node) extends TableBase[Nothing] {

  def left = _left.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def right = _right.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def leftNode = Node(_left)
  def rightNode = Node(_right)
  def nodeChildren = leftNode :: rightNode :: Nil
  override def toString = "Join(" + Node(_left) + "," + Node(_right) + ")"
}

object Join {
  def unapply[T1 <: AbstractTable[_], T2 <: TableBase[_]](j: Join[T1, T2]) = Some((j.left, j.right))

  final case class JoinPart(left: Node, right: Node) extends BinaryNode {
    override def toString = "JoinPart"
    override def nodeNamedChildren = (left, "table") :: (right, "from") :: Nil
  }

  abstract class JoinType(val sqlName: String)
  case object Inner extends JoinType("inner")
  case object Left extends JoinType("left outer")
  case object Right extends JoinType("right outer")
  case object Outer extends JoinType("full outer")
}
