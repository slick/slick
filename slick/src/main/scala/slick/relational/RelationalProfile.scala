package slick.relational

import slick.ast._
import slick.basic.{BasicActionComponent, BasicProfile}
import slick.compiler.{EmulateOuterJoins, Phase, QueryCompiler}
import slick.dbio._
import slick.lifted.FunctionSymbolExtensionMethods._
import slick.lifted._

import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag

/** A profile for relational databases that does not assume the existence
  * of SQL (or any other text-based language for executing statements).
  * It requires a relational table structure as its basic model of data. */
trait RelationalProfile extends BasicProfile with RelationalTableComponent
  with RelationalSequenceComponent with RelationalTypesComponent
  with RelationalActionComponent { self: RelationalProfile =>

  @deprecated("Use the Profile object directly instead of calling `.profile` on it", "3.2")
  override val profile: RelationalProfile = this

  type Backend <: RelationalBackend

  override protected def computeCapabilities = super.computeCapabilities ++ RelationalCapabilities.all

  trait API extends super.API with ImplicitColumnTypes {
    type FastPath[T] = SimpleFastPathResultConverter[ResultConverterDomain, T]
    type Table[T] = self.Table[T]
    type Sequence[T] = self.Sequence[T]
    val Sequence = self.Sequence
    type ColumnType[T] = self.ColumnType[T]
    type BaseColumnType[T] = self.BaseColumnType[T]
    val MappedColumnType = self.MappedColumnType

    @deprecated("Use an explicit conversion to an Option column with `.?`", "3.0")
    implicit def columnToOptionColumn[T : BaseTypedType](c: Rep[T]): Rep[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypedType](v: T): LiteralColumn[T] = new LiteralColumn[T](v)
    implicit def columnToOrdered[T : TypedType](c: Rep[T]): ColumnOrdered[T] = ColumnOrdered[T](c, Ordering())
    implicit def tableQueryToTableQueryExtensionMethods[T <: RelationalProfile#Table[_], U](q: Query[T, U, Seq] with TableQuery[T]): TableQueryExtensionMethods[T, U] =
      new TableQueryExtensionMethods[T, U](q)

    implicit def streamableCompiledInsertActionExtensionMethods[EU](c: StreamableCompiled[_, _, EU]): InsertActionExtensionMethods[EU] = createInsertActionExtensionMethods[EU](c.compiledInsert.asInstanceOf[CompiledInsert])
    implicit def queryInsertActionExtensionMethods[U, C[_]](q: Query[_, U, C]): InsertActionExtensionMethods[U] = createInsertActionExtensionMethods[U](compileInsert(q.toNode))

    implicit def schemaActionExtensionMethods(sd: SchemaDescription): SchemaActionExtensionMethods = createSchemaActionExtensionMethods(sd)

    implicit def fastPathExtensionMethods[T, P](mp: MappedProjection[T, P]): FastPathExtensionMethods[ResultConverterDomain, T, P] = new FastPathExtensionMethods[ResultConverterDomain, T, P](mp)
  }

  val api: API

  final lazy val compiler = computeQueryCompiler

  protected def computeQueryCompiler: QueryCompiler = {
    val base = QueryCompiler.standard
    val canJoinLeft = capabilities contains RelationalCapabilities.joinLeft
    val canJoinRight = capabilities contains RelationalCapabilities.joinRight
    val canJoinFull = capabilities contains RelationalCapabilities.joinFull
    if(canJoinLeft && canJoinRight && canJoinFull) base
    else base.addBefore(new EmulateOuterJoins(canJoinLeft, canJoinRight), Phase.expandRecords)
  }

  class TableQueryExtensionMethods[T <: RelationalProfile#Table[_], U](val q: Query[T, U, Seq] with TableQuery[T]) {
    /** Get the schema description (DDL) for this table. */
    def schema: SchemaDescription = buildTableSchemaDescription(q.shaped.value.asInstanceOf[Table[_]])

    /** Create a `Compiled` query which selects all rows where the specified
      * key matches the parameter value. */
    def findBy[P](f: (T => Rep[P]))(implicit ashape: Shape[ColumnsShapeLevel, Rep[P], P, Rep[P]], pshape: Shape[ColumnsShapeLevel, P, P, _]): CompiledFunction[Rep[P] => Query[T, U, Seq], Rep[P], P, Query[T, U, Seq], Seq[U]] = {
      import self.api._
      Compiled { (p: Rep[P]) => (q: Query[T, U, Seq]).filter(table => Library.==.column[Boolean](f(table).toNode, p.toNode)) }
    }
  }

  /** Run a query synchronously on the provided session. This is used by DistributedProfile until we
    * can make it fully asynchronous. */
  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: Backend#Session): R

  class FastPathExtensionMethods[M <: ResultConverterDomain, T, P](val mp: MappedProjection[T, P]) {
    def fastPath(fpf: (TypeMappingResultConverter[M, T, _] => SimpleFastPathResultConverter[M, T])): MappedProjection[T, P] = mp.genericFastPath {
      case tm @ TypeMappingResultConverter(_: ProductResultConverter[_, _], _, _) =>
        fpf(tm.asInstanceOf[TypeMappingResultConverter[M, T, _]])
      case tm => tm
    }
  }
}

object RelationalProfile {
  /** Extra column options for RelationalProfile */
  object ColumnOption {
    /** Default value for the column. Needs to wrap an Option for nullable Columns. */
    case class Default[T](defaultValue: T) extends ColumnOption[T]

    /** Number of unicode characters for string-like types. Unlike DBType this is portable
      * between different DBMS. Note that for DDL Slick currently picks type CHAR when
      * varying=false and VARCHAR when varying=true. Slick uses VARCHAR or VARCHAR(254) in DDL for
      * String columns if neither ColumnOption DBType nor Length are given.
      *
      * @param varying indicates wether this is just the maximum length of a varying */
    case class Length(length: Int, varying: Boolean = true) extends ColumnOption[Nothing]
  }
}

trait RelationalTableComponent { self: RelationalProfile =>

  def buildTableSchemaDescription(table: Table[_]): SchemaDescription

  trait ColumnOptions {
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = RelationalProfile.ColumnOption.Default[T](defaultValue)
    val AutoInc = ColumnOption.AutoInc
    val Unique = ColumnOption.Unique
    val Length = RelationalProfile.ColumnOption.Length
  }

  val columnOptions: ColumnOptions = new ColumnOptions {}

  abstract class Table[T](_tableTag: Tag, _schemaName: Option[String], _tableName: String) extends AbstractTable[T](_tableTag, _schemaName, _tableName) { table =>
    final type TableElementType = T

    def this(_tableTag: Tag, _tableName: String) = this(_tableTag, None, _tableName)

    def tableProvider: RelationalProfile = self

    def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(self, schemaName.getOrElse("_"), tableName)

    val O: self.columnOptions.type = columnOptions

    /**
      * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String
      * columns if neither ColumnOption DBType nor Length are given.
      */
    def column[C](n: String, options: ColumnOption[C]*)(implicit tt: TypedType[C]): Rep[C] = {
      if(tt == null) throw new NullPointerException(
        "implicit TypedType[C] for column[C] is null. "+
        "This may be an initialization order problem. "+
        "When using a MappedColumnType, you may want to change it from a val to a lazy val or def.")
      new Rep.TypedRep[C] {
        override def toNode =
          Select((tableTag match {
            case r: RefTag => r.path
            case _ => tableNode
          }), FieldSymbol(n)(options, tt)) :@ tt
        override def toString = (tableTag match {
          case r: RefTag => "(" + _tableName + " " + r.path + ")"
          case _ => _tableName
        }) + "." + n
      }
    }
  }
}

trait RelationalSequenceComponent { self: RelationalProfile =>

  def buildSequenceSchemaDescription(seq: Sequence[_]): SchemaDescription

  class Sequence[T] private[Sequence] (val name: String,
                                       val _minValue: Option[T],
                                       val _maxValue: Option[T],
                                       val _increment: Option[T],
                                       val _start: Option[T],
                                       val _cycle: Boolean)(implicit val tpe: TypedType[T], val integral: Integral[T])
    { seq =>

    def min(v: T) = new Sequence[T](name, Some(v), _maxValue, _increment, _start, _cycle)
    def max(v: T) = new Sequence[T](name, _minValue, Some(v), _increment, _start, _cycle)
    def inc(v: T) = new Sequence[T](name, _minValue, _maxValue, Some(v), _start, _cycle)
    def start(v: T) = new Sequence[T](name, _minValue, _maxValue, _increment, Some(v), _cycle)
    def cycle = new Sequence[T](name, _minValue, _maxValue, _increment, _start, true)

    final def next = Library.NextValue.column[T](toNode)
    final def curr = Library.CurrentValue.column[T](toNode)

    def toNode = SequenceNode(name)(_increment.map(integral.toLong).getOrElse(1))

    def schema: SchemaDescription = buildSequenceSchemaDescription(this)
  }

  object Sequence {
    def apply[T : TypedType : Integral](name: String) = new Sequence[T](name, None, None, None, None, false)
  }
}

trait RelationalTypesComponent { self: RelationalProfile =>
  type ColumnType[T] <: TypedType[T]
  type BaseColumnType[T] <: ColumnType[T] with BaseTypedType[T]

  val MappedColumnType: MappedColumnTypeFactory


  trait MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T]

    protected[this] def assertNonNullType(t: BaseColumnType[_]): Unit =
      if(t == null)
        throw new NullPointerException("implicit BaseColumnType[U] for MappedColumnType.base[T, U] is null. This may be an initialization order problem.")
  }

  trait ImplicitColumnTypes {
    implicit def isomorphicType[A, B](implicit iso: Isomorphism[A, B], ct: ClassTag[A], jt: BaseColumnType[B]): BaseColumnType[A] =
      MappedColumnType.base[A, B](iso.map, iso.comap)
    implicit def booleanColumnType: BaseColumnType[Boolean]
    implicit def bigDecimalColumnType: BaseColumnType[BigDecimal] with NumericTypedType
    implicit def byteColumnType: BaseColumnType[Byte] with NumericTypedType
    implicit def charColumnType: BaseColumnType[Char]
    implicit def doubleColumnType: BaseColumnType[Double] with NumericTypedType
    implicit def floatColumnType: BaseColumnType[Float] with NumericTypedType
    implicit def intColumnType: BaseColumnType[Int] with NumericTypedType
    implicit def longColumnType: BaseColumnType[Long] with NumericTypedType
    implicit def shortColumnType: BaseColumnType[Short] with NumericTypedType
    implicit def stringColumnType: BaseColumnType[String]
  }
}

trait RelationalActionComponent extends BasicActionComponent { self: RelationalProfile =>

  //////////////////////////////////////////////////////////// Insert Actions

  type InsertActionExtensionMethods[T] <: InsertActionExtensionMethodsImpl[T]

  def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T]

  trait InsertActionExtensionMethodsImpl[T] {
    /** The result type when inserting a single value. */
    type SingleInsertResult

    /** The result type when inserting a collection of values. */
    type MultiInsertResult

    /** An Action that inserts a single value. */
    def += (value: T): ProfileAction[SingleInsertResult, NoStream, Effect.Write]

    /** An Action that inserts a collection of values. */
    def ++= (values: Iterable[T]): ProfileAction[MultiInsertResult, NoStream, Effect.Write]
  }

  //////////////////////////////////////////////////////////// Schema Actions

  type SchemaActionExtensionMethods <: SchemaActionExtensionMethodsImpl

  def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods

  trait SchemaActionExtensionMethodsImpl {
    /** Create an Action that creates the entities described by this schema description. */
    def create: ProfileAction[Unit, NoStream, Effect.Schema]

    /** Create an Action that creates the entities described by this schema description if the entities do not exist. */
    def createIfNotExists: ProfileAction[Unit, NoStream, Effect.Schema]

    /** Create an Action that drops the entities described by this schema description. */
    def drop: ProfileAction[Unit, NoStream, Effect.Schema]

    /** Create an Action that drops the entities described by this schema description only if the entities exist. */
    def dropIfExists: ProfileAction[Unit, NoStream, Effect.Schema]

    /** Create an Action that truncates entries described by this schema description */
    def truncate: ProfileAction[Unit, NoStream, Effect.Schema]
  }
}
