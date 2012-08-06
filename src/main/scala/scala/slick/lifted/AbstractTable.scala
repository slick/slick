package scala.slick.lifted

import scala.slick.driver.BasicProfile
import scala.slick.session.{PositionedResult, PositionedParameters}
import scala.slick.ast._
import scala.slick.ast.Util.nodeToNodeOps

abstract class AbstractTable[T](val schemaName: Option[String], val tableName: String) extends TableNode with ColumnBase[T] with NullaryNode with WithOp {

  def * : ColumnBase[T]
  def nodeShaped_* : ShapedValue[_, _] = ShapedValue(*, implicitly[Shape[ColumnBase[T], _, _]])

  def create_* : Iterable[FieldSymbol] = {
    Node(*).collect {
      case Select(Ref(IntrinsicSymbol(in)), f: FieldSymbol) if in == this => f
    }.toSeq.distinct
  }

  def foreignKey[P, PU, TT <: TableNode, U]
      (name: String, sourceColumns: P, targetTable: TT)
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
       onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Shape[TT, U, _], unpackp: Shape[P, PU, _]): ForeignKeyQuery[TT, U] = {
    val q = Query[TT, U, TT](targetTable)(Shape.tableShape.asInstanceOf[Shape[TT, U, TT]])
    val generator = new AnonSymbol
    val aliased = q.unpackable.encodeRef(generator)
    val fv = Library.==(Node(targetColumns(aliased.value)), Node(sourceColumns))
    val fk = ForeignKey(name, this, q.unpackable.asInstanceOf[ShapedValue[TT, _]],
      targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete)
    new ForeignKeyQuery[TT, U](Filter(generator, Node(q), fv), q.unpackable, IndexedSeq(fk), q, generator, aliased.value)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit unpack: Shape[T, _, _]): PrimaryKey = PrimaryKey(name, unpack.linearizer(sourceColumns).narrowedLinearizer.getLinearizedNodes)

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && classOf[Constraint].isAssignableFrom(m.getReturnType)
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey[_ <: TableNode, _]] =
    tableConstraints.collect{ case q: ForeignKeyQuery[_, _] => q.fks }.flatten.toIndexedSeq

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect{ case k: PrimaryKey => k }.toIndexedSeq

  def index[T](name: String, on: T, unique: Boolean = false)(implicit shape: Shape[T, _, _]) = new Index(name, this, shape.linearizer(on).narrowedLinearizer.getLinearizedNodes, unique)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index])

  def getLinearizedNodes = *.getLinearizedNodes
  def getResult(profile: BasicProfile, rs: PositionedResult) = *.getResult(profile, rs)
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T) = *.updateResult(profile, rs, value)
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]) = *.setParameter(profile, ps, value)
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

trait NothingContainer {
  /**
   * Uninhabited type for queries of raw tables that can be used instead of
   * Nothing (because the type inferencer will not infer Nothing where needed)
   */
  type TableNothing
}
