package slick.memory

import scala.collection.compat.*
import scala.collection.mutable
import scala.language.existentials
import scala.reflect.ClassTag

import slick.ast.*
import slick.ast.TypeUtil.*
import slick.basic.{FixedBasicAction, FixedBasicStreamingAction}
import slick.compiler.*
import slick.dbio.*
import slick.relational.{CompiledMapping, RelationalProfile, ResultConverter, ResultConverterCompiler}
import slick.util.{??, DumpInfo}

/** A profile for interpreted queries on top of the in-memory database. */
trait MemoryProfile extends RelationalProfile with MemoryQueryingProfile { self: MemoryProfile =>

  type SchemaDescription = SchemaDescriptionDef
  type InsertInvoker[T] = InsertInvokerDef[T]
  type Backend = HeapBackend
  val backend: Backend = HeapBackend
  val api: MemoryAPI = new MemoryAPI {}

  lazy val queryCompiler = compiler + new MemoryCodeGen
  lazy val updateCompiler = compiler
  lazy val deleteCompiler = compiler
  lazy val insertCompiler =
    QueryCompiler(
      Phase.assignUniqueSymbols,
      Phase.inferTypes,
      new InsertCompiler(InsertCompiler.NonAutoInc),
      new MemoryInsertCodeGen
    )

  override protected def computeCapabilities = super.computeCapabilities ++ MemoryCapabilities.all

  def createInsertInvoker[T](tree: Node): InsertInvoker[T] = new InsertInvokerDef[T](tree)
  def buildSequenceSchemaDescription(seq: Sequence[?]): SchemaDescription = ??
  def buildTableSchemaDescription(table: Table[?]): SchemaDescription = new DDL(Vector(table))

  type QueryActionExtensionMethods[R, S <: NoStream] = MemoryQueryActionExtensionMethodsImpl[R, S]
  type StreamingQueryActionExtensionMethods[R, T] = MemoryStreamingQueryActionExtensionMethodsImpl[R, T]
  type SchemaActionExtensionMethods = MemorySchemaActionExtensionMethodsImpl
  type InsertActionExtensionMethods[T] = MemoryInsertActionExtensionMethodsImpl[T]

  def createQueryActionExtensionMethods[R, S <: NoStream](tree: Node, param: Any): QueryActionExtensionMethods[R, S] =
    new QueryActionExtensionMethods[R, S](tree, param)
  def createStreamingQueryActionExtensionMethods[R, T](tree: Node,
                                                       param: Any): StreamingQueryActionExtensionMethods[R, T] =
    new StreamingQueryActionExtensionMethods[R, T](tree, param)
  def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    new MemorySchemaActionExtensionMethodsImpl(schema)
  def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new MemoryInsertActionExtensionMethodsImpl[T](compiled)

  override lazy val MappedColumnType: MemoryMappedColumnTypeFactory = new MemoryMappedColumnTypeFactory

  class MemoryMappedColumnTypeFactory extends MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] = {
      assertNonNullType(implicitly[BaseColumnType[U]])
      new MappedColumnType(implicitly[BaseColumnType[U]], tmap, tcomap)
    }
  }

  class MappedColumnType[T, U](val baseType: ColumnType[U], toBase: T => U, toMapped: U => T)
                              (implicit val classTag: ClassTag[T]) extends ScalaType[T] with BaseTypedType[T] {
    def nullable: Boolean = baseType.nullable
    def ordered: Boolean = baseType.ordered
    def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T] = new scala.math.Ordering[T] {
      val uOrdering = baseType.scalaOrderingFor(ord)
      def compare(x: T, y: T): Int = uOrdering.compare(toBase(x), toBase(y))
    }
  }

  trait MemoryAPI extends RelationalAPI with MemoryQueryingAPI {
    type SimpleDBIO[+R] = SimpleMemoryAction[R]
    val SimpleDBIO = SimpleMemoryAction
  }

  protected def createInterpreter(db: Backend#Database, param: Any): QueryInterpreter =
    new QueryInterpreter(db, param) {
      override def run(n: Node) = n match {
        case ResultSetMapping(_, from, CompiledMapping(converter, _)) :@ CollectionType(cons, el) =>
          val fromV = run(from).asInstanceOf[IterableOnce[Any]]
          val b = cons.createBuilder(el.classTag).asInstanceOf[mutable.Builder[Any, Any]]
          b ++= fromV.iterator.map { v =>
            converter.asInstanceOf[ResultConverter[MemoryResultConverterDomain, ?]]
              .read(v.asInstanceOf[QueryInterpreter.ProductValue])
          }
          b.result()
        case n                                                                                    => super.run(n)
      }
    }

  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: Backend#Session): R =
    createInterpreter(session.database, param).run(tree).asInstanceOf[R]

  class InsertInvokerDef[T](tree: Node) {
    protected[this] val ResultSetMapping(_, Insert(_, table: TableNode, _, _), CompiledMapping(converter, _)) = tree

    type SingleInsertResult = Unit
    type MultiInsertResult = Unit

    def += (value: T)(implicit session: Backend#Session): Unit = {
      val heapTable = session.database.getTable(table.tableName)
      val buf = heapTable.createInsertRow
      converter.asInstanceOf[ResultConverter[MemoryResultConverterDomain, Any]].set(value, buf, 0)
      heapTable.append(buf.toIndexedSeq)
    }

    def ++= (values: Iterable[T])(implicit session: Backend#Session): Unit =
      values.foreach(this += _)
  }

  class DDL(val tables: Vector[Table[?]]) extends SchemaDescriptionDef {
    def ++(other: SchemaDescription): SchemaDescription =
      new DDL(tables ++ other.asInstanceOf[DDL].tables)
  }

  type ProfileAction[+R, +S <: NoStream, -E <: Effect] = FixedBasicAction[R, S, E]
  type StreamingProfileAction[+R, +T, -E <: Effect] = FixedBasicStreamingAction[R, T, E]

  protected[this] def dbAction[R, S <: NoStream, E <: Effect](f: Backend#Session => R): ProfileAction[R, S, E] =
    new ProfileAction[R, S, E] with SynchronousDatabaseAction[R, S, Backend#This, E] {
      def run(ctx: Backend#Context): R = f(ctx.session)
      def getDumpInfo = DumpInfo("MemoryProfile.ProfileAction")
    }

  class StreamingQueryAction[R, T](tree: Node, param: Any)
    extends StreamingProfileAction[R, T, Effect.Read]
      with SynchronousDatabaseAction[R, Streaming[T], Backend#This, Effect.Read] {
    type StreamState = Iterator[T]
    protected[this] def getIterator(ctx: Backend#Context): Iterator[T] = {
      val inter = createInterpreter(ctx.session.database, param)
      val ResultSetMapping(_, from, CompiledMapping(converter, _)) = tree
      val productValueIterator = inter.run(from).asInstanceOf[IterableOnce[QueryInterpreter.ProductValue]].iterator
      productValueIterator.map(converter.asInstanceOf[ResultConverter[MemoryResultConverterDomain, T]].read)
    }
    def run(ctx: Backend#Context): R =
      createInterpreter(ctx.session.database, param).run(tree).asInstanceOf[R]
    override def emitStream(ctx: Backend#StreamingContext, limit: Long, state: StreamState): StreamState = {
      val it = if(state ne null) state else getIterator(ctx)
      var count = 0L
      while(count < limit && it.hasNext) {
        count += 1
        ctx.emit(it.next())
      }
      if(it.hasNext) it else null
    }
    def head: ProfileAction[T, NoStream, Effect.Read] =
      new ProfileAction[T, NoStream, Effect.Read]
        with SynchronousDatabaseAction[T, NoStream, Backend#This, Effect.Read] {
        def run(ctx: Backend#Context): T = getIterator(ctx).next()
        def getDumpInfo = DumpInfo("MemoryProfile.StreamingQueryAction.first")
      }
    def headOption: ProfileAction[Option[T], NoStream, Effect.Read] =
      new ProfileAction[Option[T], NoStream, Effect.Read]
        with SynchronousDatabaseAction[Option[T], NoStream, Backend#This, Effect.Read] {

        def run(ctx: Backend#Context): Option[T] = {
          val it = getIterator(ctx)
          if (it.hasNext) Some(it.next()) else None
        }
        def getDumpInfo = DumpInfo("MemoryProfile.StreamingQueryAction.firstOption")
      }
    def getDumpInfo = DumpInfo("MemoryProfile.StreamingQueryAction")
  }

  class MemoryQueryActionExtensionMethodsImpl[R, S <: NoStream](tree: Node, param: Any)
    extends BasicQueryActionExtensionMethodsImpl[R, S] {

    def result: ProfileAction[R, S, Effect.Read] =
      new StreamingQueryAction[R, Nothing](tree, param).asInstanceOf[ProfileAction[R, S, Effect.Read]]
  }

  class MemoryStreamingQueryActionExtensionMethodsImpl[R, T](tree: Node, param: Any)
    extends MemoryQueryActionExtensionMethodsImpl[R, Streaming[T]](tree, param)
      with BasicStreamingQueryActionExtensionMethodsImpl[R, T] {
    override def result: StreamingProfileAction[R, T, Effect.Read] =
      super.result.asInstanceOf[StreamingProfileAction[R, T, Effect.Read]]
  }

  class MemorySchemaActionExtensionMethodsImpl(schema: SchemaDescription)
    extends RelationalSchemaActionExtensionMethodsImpl {
    protected[this] val tables = schema.asInstanceOf[DDL].tables
    override def create: FixedBasicAction[Unit, Nothing, Effect.Schema] = dbAction { session =>
      tables.foreach(t =>
        session.database.createTable(t.tableName,
          t.create_*.map { fs => new HeapBackend.Column(fs, typeInfoFor(fs.tpe)) }.toIndexedSeq,
          t.indexes.toIndexedSeq, t.tableConstraints.toIndexedSeq)
      )
    }

    override def createIfNotExists: FixedBasicAction[Unit, Nothing, Effect.Schema] = dbAction { session =>
      tables.foreach(t =>
        session.database.createTableIfNotExists(t.tableName,
          t.create_*.map { fs => new HeapBackend.Column(fs, typeInfoFor(fs.tpe)) }.toIndexedSeq,
          t.indexes.toIndexedSeq, t.tableConstraints.toIndexedSeq)
      )
    }

    override def drop: FixedBasicAction[Unit, Nothing, Effect.Schema] = dbAction { session =>
      tables.foreach(t => session.database.dropTable(t.tableName))
    }

    override def dropIfExists: FixedBasicAction[Unit, Nothing, Effect.Schema] = dbAction { session =>
      tables.foreach(t => session.database.dropTableIfExists(t.tableName))
    }

    override def truncate: FixedBasicAction[Unit, Nothing, Effect.Schema] = dbAction{ session =>
      tables.foreach(t => session.database.truncateTable(t.tableName) )
    }
  }

  class MemoryInsertActionExtensionMethodsImpl[T](compiled: CompiledInsert)
    extends InsertActionExtensionMethodsImpl[T] {

    protected[this] val inv = createInsertInvoker[T](compiled)
    type SingleInsertResult = Unit
    type MultiInsertResult = Unit
    override def +=(value: T): FixedBasicAction[Unit, Nothing, Effect.Write] = dbAction(inv.+=(value)(_))
    override def ++=(values: Iterable[T]): FixedBasicAction[Unit, Nothing, Effect.Write] = dbAction(inv.++=(values)(_))
  }

  override def computeQueryCompiler = super.computeQueryCompiler ++ QueryCompiler.interpreterPhases

  class InsertMappingCompiler(insert: Insert) extends ResultConverterCompiler[MemoryResultConverterDomain] {
    val Insert(_, table: TableNode, ProductNode(cols), _) = insert
    val tableColumnIndexes = table.profileTable.asInstanceOf[Table[?]].create_*.zipWithIndex.toMap

    def createColumnConverter(n: Node,
                              idx: Int,
                              column: Option[FieldSymbol]): ResultConverter[MemoryResultConverterDomain, ?] =
      new InsertResultConverter(tableColumnIndexes(column.get))

    class InsertResultConverter(tableIndex: Int) extends ResultConverter[MemoryResultConverterDomain, Any] {
      override def read(pr: MemoryResultConverterDomain#Reader): Nothing = ??
      override def update(value: Any, pr: MemoryResultConverterDomain#Updater): Nothing = ??
      def set(value: Any, pp: MemoryResultConverterDomain#Writer, offset: Int) = pp(tableIndex + offset) = value
      override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"tableIndex=$tableIndex")
      def width = 1
    }
  }

  class MemoryInsertCodeGen extends CodeGen {
    override def compileServerSideAndMapping(serverSide: Node,
                                             mapping: Option[Node],
                                             state: CompilerState): (Node, Option[CompiledMapping]) =
      (serverSide, mapping.map(new InsertMappingCompiler(serverSide.asInstanceOf[Insert]).compileMapping))
  }
}

object MemoryProfile extends MemoryProfile

/** A non-streaming Action that wraps a synchronous MemoryProfile API call. */
case class SimpleMemoryAction[+R](f: HeapBackend#Context => R)
  extends SynchronousDatabaseAction[R, NoStream, HeapBackend, Effect.All] {

  def run(context: HeapBackend#Context): R = f(context)
  def getDumpInfo = DumpInfo(name = "SimpleMemoryAction")
}
