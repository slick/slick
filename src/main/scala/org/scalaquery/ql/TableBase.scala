package org.scalaquery.ql

import scala.collection.mutable.{ArrayBuffer, HashSet}
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
    //Query(targetTable).filter[Column[Boolean]](tt => ColumnOps.Is(Node(targetColumns(tt)), Node(sourceColumns)))
    val q = Query[TT, U, TT](targetTable)
    val generator = new AnonSymbol
    val aliased = InRef.forUnpackable(generator, q.unpackable)
    val fv = ColumnOps.Is(Node(targetColumns(aliased.value)), Node(sourceColumns))
    val fk = ForeignKey(name, this, q.unpackable.asInstanceOf[Unpackable[TT, _]],
      targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete)
    new ForeignKeyQuery[TT, U](Filter(generator, Node(q), Node(fv)), q.unpackable, IndexedSeq(fk), q, generator, aliased.value)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit unpack: Unpack[T, _]): PrimaryKey = PrimaryKey(name, unpack.linearizer(sourceColumns).getLinearizedNodes)

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && classOf[Constraint].isAssignableFrom(m.getReturnType)
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
}

abstract class JoinType(val sqlName: String)

object JoinType {
  case object Inner extends JoinType("inner")
  case object Left extends JoinType("left outer")
  case object Right extends JoinType("right outer")
  case object Outer extends JoinType("full outer")
}

object Join {
  @deprecated("Use a tuple extractor instead", "0.10.0-M2")
  def unapply[T1, T2](t: (T1, T2)) = Some(t)

  @deprecated("Use JoinType.Inner instead", "0.10.0-M2")
  val Inner = JoinType.Inner

  @deprecated("Use JoinType.Left instead", "0.10.0-M2")
  val Left = JoinType.Left

  @deprecated("Use JoinType.Right instead", "0.10.0-M2")
  val Right = JoinType.Right

  @deprecated("Use JoinType.Outer instead", "0.10.0-M2")
  val Outer = JoinType.Outer
}