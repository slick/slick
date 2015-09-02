package slick.lifted

import slick.ast._
import slick.ast.Util.nodeToNodeOps
import slick.model.ForeignKeyAction

/** A Tag marks a specific row represented by an AbstractTable instance. */
sealed trait Tag {
  /** Return a new instance of the AbstractTable carrying this Tag, with a new path */
  def taggedAs(path: Node): AbstractTable[_]
}

/** A Tag for table instances that represent a Node */
abstract class RefTag(val path: Node) extends Tag

/** A Tag marking the base table instance itself */
trait BaseTag extends Tag

/** The driver-independent superclass of all table row objects.
  * @tparam T Row type for this table. Make sure it matches the type of your `*` projection. */
abstract class AbstractTable[T](val tableTag: Tag, val schemaName: Option[String], val tableName: String) extends Rep[T] {
  /** The client-side type of the table as defined by its * projection */
  type TableElementType

  def tableIdentitySymbol: TableIdentitySymbol

  lazy val tableNode = TableNode(schemaName, tableName, tableIdentitySymbol, tableIdentitySymbol)(this)

  def encodeRef(path: Node) = tableTag.taggedAs(path).asInstanceOf[AbstractTable[T]]

  /** The * projection of the table used as default for queries and inserts.
    * Should include all columns as a tuple, HList or custom shape and optionally
    * map them to a custom entity type using the <> operator.
    * The `ProvenShape` return type ensures that
    * there is a `Shape` available for translating between the `Column`-based
    * type in * and the client-side type without `Column` in the table's type
    * parameter. */
  def * : ProvenShape[T]

  override def toNode = tableTag match {
    case _: BaseTag =>
      val sym = new AnonSymbol
      TableExpansion(sym, tableNode, tableTag.taggedAs(Ref(sym)).*.toNode)
    case t: RefTag => t.path
  }

  def create_* : Iterable[FieldSymbol] = collectFieldSymbols(*.toNode)

  protected[this] def collectFieldSymbols(n: Node): Iterable[FieldSymbol] =
    n.collect {
      case Select(in, f: FieldSymbol) if in == tableNode => f
    }.toSeq.distinct

  /** Define a foreign key relationship.
    *
    * @param name The name of the foreign key in the database (only used when
    *             you define the database schema with Slick).
    * @param sourceColumns A column or a projection of multiple columns from
    *                      this table defining the source of the foreign key.
    * @param targetTableQuery The `TableQuery` for the target table.
    * @param targetColumns A function that maps from the target table to the
    *                      column (or columns) to which the foreign key points.
    * @param onUpdate A `ForeignKeyAction`, default being `NoAction`.
    * @param onDelete A `ForeignKeyAction`, default being `NoAction`.
    */
  def foreignKey[P, PU, TT <: AbstractTable[_], U]
      (name: String, sourceColumns: P, targetTableQuery: TableQuery[TT])
      (targetColumns: TT => P, onUpdate: ForeignKeyAction = ForeignKeyAction.NoAction,
       onDelete: ForeignKeyAction = ForeignKeyAction.NoAction)(implicit unpack: Shape[_ <: FlatShapeLevel, TT, U, _], unpackp: Shape[_ <: FlatShapeLevel, P, PU, _]): ForeignKeyQuery[TT, U] = {
    val targetTable: TT = targetTableQuery.shaped.value
    val q = targetTableQuery.asInstanceOf[Query[TT, U, Seq]]
    val generator = new AnonSymbol
    val aliased = q.shaped.encodeRef(Ref(generator))
    val fv = Library.==.typed[Boolean](unpackp.toNode(targetColumns(aliased.value)), unpackp.toNode(sourceColumns))
    val fk = ForeignKey(name, toNode, q.shaped.asInstanceOf[ShapedValue[TT, _]],
      targetTable, unpackp, sourceColumns, targetColumns, onUpdate, onDelete)
    new ForeignKeyQuery[TT, U](Filter.ifRefutable(generator, q.toNode, fv), q.shaped, IndexedSeq(fk), q, generator, aliased.value)
  }

  /** Define the primary key for this table.
    * It is usually simpler to use the `O.PrimaryKey` option on the primary
    * key column but this method allows you to define compound primary keys
    * or give them user-defined names (when defining the database schema
    * with Slick). */
  def primaryKey[T](name: String, sourceColumns: T)(implicit shape: Shape[_ <: FlatShapeLevel, T, _, _]): PrimaryKey = PrimaryKey(name, ForeignKey.linearizeFieldRefs(shape.toNode(sourceColumns)))

  def tableConstraints: Iterator[Constraint] = for {
      m <- getClass().getMethods.iterator
      if m.getParameterTypes.length == 0 && classOf[Constraint].isAssignableFrom(m.getReturnType)
      q = m.invoke(this).asInstanceOf[Constraint]
    } yield q

  final def foreignKeys: Iterable[ForeignKey] =
    tableConstraints.collect{ case q: ForeignKeyQuery[_, _] => q.fks }.flatten.toIndexedSeq.sortBy(_.name)

  final def primaryKeys: Iterable[PrimaryKey] =
    tableConstraints.collect{ case k: PrimaryKey => k }.toIndexedSeq.sortBy(_.name)

  /** Define an index or a unique constraint. */
  def index[T](name: String, on: T, unique: Boolean = false)(implicit shape: Shape[_ <: FlatShapeLevel, T, _, _]) = new Index(name, this, ForeignKey.linearizeFieldRefs(shape.toNode(on)), unique)

  def indexes: Iterable[Index] = (for {
      m <- getClass().getMethods.view
      if m.getReturnType == classOf[Index] && m.getParameterTypes.length == 0
    } yield m.invoke(this).asInstanceOf[Index]).sortBy(_.name)
}
