package scala.slick.lifted

import scala.slick.ast._
import scala.slick.ast.Util.nodeToNodeOps

/** A Tag marks a specific row represented by an AbstractTable instance. */
sealed trait Tag {
  /** Return a new instance of the AbstractTable carrying this Tag, with a new path */
  def taggedAs(path: List[Symbol]): AbstractTable[_]
}

/** A Tag for table instances that represent a path */
abstract class RefTag(val path: List[Symbol]) extends Tag

/** A Tag marking the base table instance itself */
trait BaseTag extends Tag

abstract class AbstractTable[T](val tableTag: Tag, val schemaName: Option[String], val tableName: String) extends ColumnBase[T] {
  type TableElementType

  def tableIdentitySymbol: TableIdentitySymbol

  lazy val tableNode = TableNode(schemaName, tableName, tableIdentitySymbol, this)

  def encodeRef(path: List[Symbol]) = tableTag.taggedAs(path).asInstanceOf[AbstractTable[T]]

  def * : ProvenShape[T]

  override def toNode = tableTag match {
    case _: BaseTag =>
      val sym = new AnonSymbol
      TableExpansion(sym, tableNode, ProductNode(Vector(tableTag.taggedAs(List(sym)).*.toNode)).flatten)
    case t: RefTag => Path(t.path)
  }

  def create_* : Iterable[FieldSymbol] = collectFieldSymbols(*.toNode)

  protected[this] def collectFieldSymbols(n: Node): Iterable[FieldSymbol] =
    n.collect {
      case Select(Ref(IntrinsicSymbol(in)), f: FieldSymbol) if in == tableNode => f
    }.toSeq.distinct

  def foreignKey[P, PU, TT <: AbstractTable[_], U]
      (name: String, sourceColumns: P, targetTableQuery: TableQuery[TT, _])
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
       onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Shape[TT, U, _], unpackp: Shape[P, PU, _]): ForeignKeyQuery[TT, U] = {
    val targetTable: TT = targetTableQuery.unpackable.value
    val q = Query[TT, U, TT](targetTable)(Shape.repShape.asInstanceOf[Shape[TT, U, TT]])
    val generator = new AnonSymbol
    val aliased = q.unpackable.encodeRef(generator :: Nil)
    val fv = Library.==.typed[Boolean](unpackp.toNode(targetColumns(aliased.value)), unpackp.toNode(sourceColumns))
    val fk = ForeignKey(name, toNode, q.unpackable.asInstanceOf[ShapedValue[TT, _]],
      targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete)
    new ForeignKeyQuery[TT, U](Filter.ifRefutable(generator, q.toNode, fv), q.unpackable, IndexedSeq(fk), q, generator, aliased.value)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit shape: Shape[T, _, _]): PrimaryKey = PrimaryKey(name, ExtraUtil.linearizeFieldRefs(shape.toNode(sourceColumns)))

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && classOf[Constraint].isAssignableFrom(m.getReturnType)
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey] =
    tableConstraints.collect{ case q: ForeignKeyQuery[_, _] => q.fks }.flatten.toIndexedSeq

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect{ case k: PrimaryKey => k }.toIndexedSeq

  def index[T](name: String, on: T, unique: Boolean = false)(implicit shape: Shape[T, _, _]) = new Index(name, this, ExtraUtil.linearizeFieldRefs(shape.toNode(on)), unique)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index])
}
