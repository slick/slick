package org.scalaquery.ql

import scala.collection.mutable.{ArrayBuffer, HashSet}
import org.scalaquery.SQueryException
import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedResult, PositionedParameters}
import org.scalaquery.ast._

sealed trait TableBase[T] extends Node with WithOp {
  override def isNamedTable = true
}

abstract class AbstractTable[T](val schemaName: Option[String], val tableName: String) extends TableBase[T] with ColumnBase[T] with NullaryNode {

  final type TableType = T
  override def toString = "Table " + tableName

  def * : ColumnBase[T]

  def create_* : Iterable[RawNamedColumn] = {
    val seq = new ArrayBuffer[RawNamedColumn]
    val seen = new HashSet[RawNamedColumn]
    def add(c: RawNamedColumn) {
      if(!seen.contains(c)) {
        seen += c
        seq += c
      }
    }
    def scan(n:Node): Unit = n match {
      case Wrapped(in, c: RawNamedColumn) if in == this => add(c)
      case n => n.nodeChildren.foreach(scan)
    }
    scan(Node(*))
    seq
  }

  def foreignKey[P, PU, TT <: AbstractTable[_], U]
      (name: String, sourceColumns: P, targetTable: TT)
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
        onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Unpack[TT, U], unpackp: Unpack[P, PU]): ForeignKeyQuery[TT, U] = {
    val mappedTTU = Unpackable(targetTable.mapOp(tt => AbstractTable.Alias(Node(tt))), unpack)
    val fks = IndexedSeq(ForeignKey(name, this, mappedTTU, targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete))
    ForeignKeyQuery(mappedTTU.reifiedNode, fks.map(Node.apply _))(fks, mappedTTU)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit unpack: Unpack[T, _]): PrimaryKey = PrimaryKey(name, unpack.linearizer(sourceColumns).getLinearizedNodes)

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 &&
        (m.getReturnType == classOf[ForeignKeyQuery[_ <: AbstractTable[_], _]]
         || m.getReturnType == classOf[PrimaryKey])
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey[_ <: AbstractTable[_], _]] =
    tableConstraints.collect{ case q: ForeignKeyQuery[_, _] => q.fks }.flatten.toIndexedSeq

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints collect { case k: PrimaryKey => k } toIndexedSeq

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
  def unapply(t: AbstractTable[_]) = Some(t.tableName)

  final case class Alias(child: Node) extends UnaryNode {
    override def toString = "AbstractTable.Alias"
    override def isNamedTable = true
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
    override def hashCode() = System.identityHashCode(this)
    override def equals(o: Any) = this eq o.asInstanceOf[AnyRef]
  }
}

final class JoinBase[+T1 <: AbstractTable[_], +T2 <: TableBase[_]](_left: T1, _right: T2, joinType: Join.JoinType) {
  protected[this] def nodeChildGenerators = Seq(_left, _right)
  def on[T <: Column[_] : CanBeQueryCondition](pred: (T1, T2) => T) = new Join(_left, _right, joinType, Node(pred(_left, _right)))
}

//TODO Remove the joins on tables, to be replaced by joins on queries
final class Join[+T1 <: AbstractTable[_], +T2 <: TableBase[_]](_left: T1, _right: T2,
  val joinType: Join.JoinType, val on: Node) extends TableBase[Nothing] {

  def left = _left.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def right = _right.mapOp(n => Join.JoinPart(Node(n), Node(this)))
  def leftNode = Node(_left)
  def rightNode = Node(_right)
  protected[this] def nodeChildGenerators = Seq(leftNode, rightNode)
  def nodeMapChildren(f: Node => Node): Node = this //-- incorrect

  override def hashCode() = toString.hashCode() + nodeChildren.hashCode()
  override def equals(o: Any) = o match {
    case j: Join[_,_] => nodeChildren == j.nodeChildren
    case _ => false
  }
}

object Join {
  def unapply[T1 <: AbstractTable[_], T2 <: TableBase[_]](j: Join[T1, T2]) = Some((j.left, j.right))

  final case class JoinPart(left: Node, right: Node) extends BinaryNode {
    override def toString = "JoinPart"
    protected[this] override def nodeChildNames = Seq("table", "from")
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }

  abstract class JoinType(val sqlName: String)
  case object Inner extends JoinType("inner")
  case object Left extends JoinType("left outer")
  case object Right extends JoinType("right outer")
  case object Outer extends JoinType("full outer")
}
