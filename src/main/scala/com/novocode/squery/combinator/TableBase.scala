package com.novocode.squery.combinator

import com.novocode.squery.SQueryException
import com.novocode.squery.combinator.basic.{BasicProfile, BasicQueryTemplate, BasicDriver}
import com.novocode.squery.session.{PositionedResult, PositionedParameters}

sealed trait TableBase[T] extends Node with WithOp {
  override def isNamedTable = true
}

object TableBase {
  type T_ = TableBase[_]
}

abstract class AbstractTable[T](val tableName: String) extends TableBase[T] with ColumnBase[T] {

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

  def foreignKey[P, TT <: AbstractTable.T_]
      (name: String, sourceColumns: ColumnBase[P], targetTable: TT)
      (targetColumns: TT => ColumnBase[P], onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
        onDelete: ForeignKeyAction = ForeignKeyAction.NoAction): ForeignKeyQuery[TT] =
    ForeignKeyQuery(ForeignKey(name, this, targetTable.mapOp(tt => AbstractTable.Alias(Node(tt))), targetTable,
      sourceColumns, targetColumns, onUpdate, onDelete))

  def foreignKeys: Iterable[ForeignKey[_ <: AbstractTable.T_]] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[ForeignKeyQuery[_ <: AbstractTable.T_]] && m.getParameterTypes.length == 0
      q = m.invoke(this).asInstanceOf[ForeignKeyQuery[_ <: AbstractTable.T_]]
    } yield q.fk)

  def getResult(profile: BasicProfile, rs: PositionedResult) = *.getResult(profile, rs)
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = *.updateResult(profile, rs, value)
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]) = *.setParameter(profile, ps, value)
}

object AbstractTable {
  type T_ = AbstractTable[_]

  def unapply[T](t: AbstractTable[T]) = Some(t.tableName)

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "AbstractTable.Alias"
    override def isNamedTable = true
  }
}

final class JoinBase[+T1 <: AbstractTable.T_, +T2 <: TableBase.T_](_left: T1, _right: T2, joinType: Join.JoinType) {
  def nodeChildren = Node(_left) :: Node(_right) :: Nil
  override def toString = "JoinBase(" + Node(_left) + "," + Node(_right) + ")"
  def on[T <: Column[_] : CanBeQueryCondition](pred: (T1, T2) => T) = new Join(_left, _right, joinType, Node(pred(_left, _right)))
}

final class Join[+T1 <: AbstractTable.T_, +T2 <: TableBase.T_](_left: T1, _right: T2,
  val joinType: Join.JoinType, val on: Node) extends TableBase[Nothing] {

  def left = _left.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def right = _right.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def leftNode = Node(_left)
  def rightNode = Node(_right)
  def nodeChildren = leftNode :: rightNode :: Nil
  override def toString = "Join(" + Node(_left) + "," + Node(_right) + ")"
}

object Join {
  def unapply[T1 <: AbstractTable.T_, T2 <: TableBase.T_](j: Join[T1, T2]) = Some((j.left, j.right))

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
