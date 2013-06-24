package scala.slick.lifted

import scala.slick.ast._
import scala.slick.ast.Util.nodeToNodeOps

abstract class AbstractTable[T](val schemaName: Option[String], val tableName: String) extends ColumnBase[T] with Cloneable with EncodeRef {

  def tableIdentitySymbol: TableIdentitySymbol

  private[AbstractTable] var _tableNode: TableNode = _

  def tableNode: TableNode = {
    if(_tableNode eq null) _tableNode = TableNode(schemaName, tableName, tableIdentitySymbol, { tableRef: Node =>
      val base = if(tableRef eq this) this else mapOp((_, _) => tableRef)
      Node(base.*)
    })
    _tableNode
  }

  private[this] def mapOp(f: (Node, List[Int]) => Node, positions: List[Int] = Nil): this.type = {
    val tv = Node(this)
    val fv = f(tv, positions)
    if(fv eq tv) this
    else {
      val t = clone.asInstanceOf[this.type]
      t._op = fv
      t
    }
  }

  private[AbstractTable] var _op: Node = _

  def tableIsRaw = _op eq null

  def encodeRef(sym: Symbol, positions: List[Int] = Nil): this.type = {
    def f(n: Node, positions: List[Int]): Node = Path(positions.map(ElementSymbol) :+ sym)
    mapOp(f, positions)
  }

  def * : ProvenShape[T]

  override def nodeDelegate = if(_op ne null) _op else tableNode

  def create_* : Iterable[FieldSymbol] = collectFieldSymbols(Node(*))

  protected[this] def collectFieldSymbols(n: Node): Iterable[FieldSymbol] =
    n.collect {
      case Select(Ref(IntrinsicSymbol(in)), f: FieldSymbol) if in == tableNode => f
    }.toSeq.distinct

  def foreignKey[P, PU, TT <: AbstractTable[_], U]
      (name: String, sourceColumns: P, targetTable: TT)
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
       onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Shape[TT, U, _], unpackp: Shape[P, PU, _]): ForeignKeyQuery[TT, U] = {
    val q = Query[TT, U, TT](targetTable)(Shape.tableShape.asInstanceOf[Shape[TT, U, TT]])
    val generator = new AnonSymbol
    val aliased = q.unpackable.encodeRef(generator)
    val fv = Library.==.typed[Boolean](Node(targetColumns(aliased.value)), Node(sourceColumns))
    val fk = ForeignKey(name, Node(this), q.unpackable.asInstanceOf[ShapedValue[TT, _]],
      targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete)
    new ForeignKeyQuery[TT, U](Filter.ifRefutable(generator, Node(q), fv), q.unpackable, IndexedSeq(fk), q, generator, aliased.value)
  }

  def primaryKey[T](name: String, sourceColumns: T)(implicit shape: Shape[T, _, _]): PrimaryKey = PrimaryKey(name, ExtraUtil.linearizeFieldRefs(Node(shape.pack(sourceColumns))))

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && classOf[Constraint].isAssignableFrom(m.getReturnType)
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey] =
    tableConstraints.collect{ case q: ForeignKeyQuery[_, _] => q.fks }.flatten.toIndexedSeq

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect{ case k: PrimaryKey => k }.toIndexedSeq

  def index[T](name: String, on: T, unique: Boolean = false)(implicit shape: Shape[T, _, _]) = new Index(name, this, ExtraUtil.linearizeFieldRefs(Node(shape.pack(on))), unique)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index])
}

trait NothingContainer {
  /**
   * Uninhabited type for queries of raw tables that can be used instead of
   * Nothing (because the type inferencer will not infer Nothing where needed)
   */
  type TableNothing
}
