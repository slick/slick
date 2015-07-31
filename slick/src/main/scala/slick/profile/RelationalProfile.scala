package slick.profile

import scala.language.{implicitConversions, higherKinds, existentials}
import slick.dbio._
import slick.ast._
import slick.backend.RelationalBackend
import slick.lifted._
import slick.relational._
import slick.util.{TupleMethods, TupleSupport}
import FunctionSymbolExtensionMethods._
import slick.SlickException
import scala.reflect.ClassTag
import slick.compiler.{Phase, EmulateOuterJoins, QueryCompiler}

/** A profile for relational databases that does not assume the existence
  * of SQL (or any other text-based language for executing statements).
  * It requires a relational table structure as its basic model of data. */
trait RelationalProfile extends BasicProfile with RelationalTableComponent
  with RelationalSequenceComponent with RelationalTypesComponent
  with RelationalActionComponent { driver: RelationalDriver =>

  type Backend <: RelationalBackend

  override protected def computeCapabilities = super.computeCapabilities ++ RelationalProfile.capabilities.all

  trait API extends super.API with ImplicitColumnTypes {
    type Table[T] = driver.Table[T]
    type Sequence[T] = driver.Sequence[T]
    val Sequence = driver.Sequence
    type ColumnType[T] = driver.ColumnType[T]
    type BaseColumnType[T] = driver.BaseColumnType[T]
    val MappedColumnType = driver.MappedColumnType

    @deprecated("Use an explicit conversion to an Option column with `.?`", "3.0")
    implicit def columnToOptionColumn[T : BaseTypedType](c: Rep[T]): Rep[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypedType](v: T) = new LiteralColumn[T](v)
    implicit def columnToOrdered[T : TypedType](c: Rep[T]): ColumnOrdered[T] = ColumnOrdered[T](c, Ordering())
    implicit def tableQueryToTableQueryExtensionMethods[T <: Table[_], U](q: Query[T, U, Seq] with TableQuery[T]) =
      new TableQueryExtensionMethods[T, U](q)

    implicit def streamableCompiledInsertActionExtensionMethods[EU](c: StreamableCompiled[_, _, EU]): InsertActionExtensionMethods[EU] = createInsertActionExtensionMethods[EU](c.compiledInsert.asInstanceOf[CompiledInsert])
    implicit def queryInsertActionExtensionMethods[U, C[_]](q: Query[_, U, C]) = createInsertActionExtensionMethods[U](compileInsert(q.toNode))

    implicit def schemaActionExtensionMethods(sd: SchemaDescription): SchemaActionExtensionMethods = createSchemaActionExtensionMethods(sd)
  }

  val api: API

  final lazy val compiler = computeQueryCompiler

  protected def computeQueryCompiler: QueryCompiler = {
    val base = QueryCompiler.standard
    val canJoinLeft = capabilities contains RelationalProfile.capabilities.joinLeft
    val canJoinRight = capabilities contains RelationalProfile.capabilities.joinRight
    val canJoinFull = capabilities contains RelationalProfile.capabilities.joinFull
    if(canJoinLeft && canJoinRight && canJoinFull) base
    else base.addBefore(new EmulateOuterJoins(canJoinLeft, canJoinRight), Phase.expandConditionals)
  }

  class TableQueryExtensionMethods[T <: Table[_], U](val q: Query[T, U, Seq] with TableQuery[T]) {
    /** Get the schema description (DDL) for this table. */
    def schema: SchemaDescription = buildTableSchemaDescription(q.shaped.value)

    /** Create a `Compiled` query which selects all rows where the specified
      * key matches the parameter value. */
    def findBy[P](f: (T => Rep[P]))(implicit ashape: Shape[ColumnsShapeLevel, Rep[P], P, Rep[P]], pshape: Shape[ColumnsShapeLevel, P, P, _]): CompiledFunction[Rep[P] => Query[T, U, Seq], Rep[P], P, Query[T, U, Seq], Seq[U]] = {
      import driver.api._
      Compiled { (p: Rep[P]) => (q: Query[T, U, Seq]).filter(table => Library.==.column[Boolean](f(table).toNode, p.toNode)) }
    }
  }

  /** Run a query synchronously on the provided session. This is used by DistributedDriver until we
    * can make it fully asynchronous. */
  def runSynchronousQuery[R](tree: Node, param: Any)(implicit session: Backend#Session): R
}

object RelationalProfile {
  object capabilities {
    /** Supports default values in column definitions */
    val columnDefaults = Capability("relational.columnDefaults")
    /** Supports foreignKeyActions */
    val foreignKeyActions = Capability("relational.foreignKeyActions")
    /** Supports the ''database'' function to get the current database name.
      * A driver without this capability will return an empty string. */
    val functionDatabase = Capability("relational.functionDatabase")
    /** Supports the ''user'' function to get the current database user.
      * A driver without this capability will return an empty string. */
    val functionUser = Capability("relational.functionUser")
    /** Supports indexOf method on string columns */
    val indexOf = Capability("relational.indexOf")
    /** Supports repeat method on string columns */
    val repeat = Capability("relational.repeat") 
    /** Supports full outer joins */
    val joinFull = Capability("relational.joinFull")
    /** Supports left outer joins */
    val joinLeft = Capability("relational.joinLeft")
    /** Supports right outer joins */
    val joinRight = Capability("relational.joinRight")
    /** Supports escape characters in "like" */
    val likeEscape = Capability("relational.likeEscape")
    /** Supports .drop on queries */
    val pagingDrop = Capability("relational.pagingDrop")
    /** Supports properly compositional paging in sub-queries */
    val pagingNested = Capability("relational.pagingNested")
    /** Returns only the requested number of rows even if some rows are not
      * unique. Without this capability, non-unique rows may be counted as
      * only one row each. */
    val pagingPreciseTake = Capability("relational.pagingPreciseTake")
    /** Supports replace method on string columns */
    val replace = Capability("relational.replace")
    /** Supports reverse method on string columns */
    val reverse = Capability("relational.reverse")
    /** Can set an Option[ Array[Byte] ] column to None */
    val setByteArrayNull = Capability("relational.setByteArrayNull")
    /** Supports the BigDecimal data type */
    val typeBigDecimal = Capability("relational.typeBigDecimal")
    /** Supports the Blob data type */
    val typeBlob = Capability("relational.typeBlob")
    /** Supports the Long data type */
    val typeLong = Capability("relational.typeLong")
    /** Supports zip, zipWith and zipWithIndex */
    val zip = Capability("relational.zip")

    /** Supports all RelationalProfile features which do not have separate capability values */
    val other = Capability("relational.other")

    /** All relational capabilities */
    val all = Set(other, columnDefaults, foreignKeyActions, functionDatabase,
      functionUser, joinFull, joinLeft, joinRight, likeEscape, pagingDrop, pagingNested,
      pagingPreciseTake, setByteArrayNull, typeBigDecimal, typeBlob, typeLong,
      zip, replace, reverse, indexOf, repeat)
  }

  /** Extra column options for RelationalProfile */
  object ColumnOption {
    /** Default value for the column. Needs to wrap an Option for nullable Columns. */
    case class Default[T](val defaultValue: T) extends ColumnOption[T]

    /** Number of unicode characters for string-like types. Unlike DBType this is portable
      * between different DBMS. Note that for DDL Slick currently picks type CHAR when
      * varying=false and VARCHAR when varying=true. Slick uses VARCHAR or VARCHAR(254) in DDL for
      * String columns if neither ColumnOption DBType nor Length are given.
      *
      * @param varying indicates wether this is just the maximum length of a varying */
    case class Length(length: Int, varying: Boolean = true) extends ColumnOption[Nothing]
  }
}

trait RelationalDriver extends BasicDriver with RelationalProfile {
  override val profile: RelationalProfile = this
}

trait RelationalTableComponent { driver: RelationalDriver =>

  def buildTableSchemaDescription(table: Table[_]): SchemaDescription

  trait ColumnOptions {
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = RelationalProfile.ColumnOption.Default[T](defaultValue)
    val AutoInc = ColumnOption.AutoInc
    val Length = RelationalProfile.ColumnOption.Length
  }

  val columnOptions: ColumnOptions = new ColumnOptions {}

  abstract class Table[T](_tableTag: Tag, _schemaName: Option[String], _tableName: String) extends AbstractTable[T](_tableTag, _schemaName, _tableName) { table =>
    final type TableElementType = T

    def this(_tableTag: Tag, _tableName: String) = this(_tableTag, None, _tableName)

    def tableProvider: RelationalDriver = driver

    def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(driver, schemaName.getOrElse("_"), tableName)

    val O: driver.columnOptions.type = columnOptions

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

trait RelationalSequenceComponent { driver: RelationalDriver =>

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

trait RelationalTypesComponent { driver: RelationalDriver =>
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

trait RelationalActionComponent extends BasicActionComponent { driver: RelationalDriver =>

  //////////////////////////////////////////////////////////// Insert Actions

  type InsertActionExtensionMethods[T] <: InsertActionExtensionMethodsImpl[T]

  def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T]

  trait InsertActionExtensionMethodsImpl[T] {
    /** The result type when inserting a single value. */
    type SingleInsertResult

    /** The result type when inserting a collection of values. */
    type MultiInsertResult

    /** An Action that inserts a single value. */
    def += (value: T): DriverAction[SingleInsertResult, NoStream, Effect.Write]

    /** An Action that inserts a collection of values. */
    def ++= (values: Iterable[T]): DriverAction[MultiInsertResult, NoStream, Effect.Write]
  }

  //////////////////////////////////////////////////////////// Schema Actions

  type SchemaActionExtensionMethods <: SchemaActionExtensionMethodsImpl

  def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods

  trait SchemaActionExtensionMethodsImpl {
    /** Create an Action that creates the entities described by this schema description. */
    def create: DriverAction[Unit, NoStream, Effect.Schema]

    /** Create an Action that drops the entities described by this schema description. */
    def drop: DriverAction[Unit, NoStream, Effect.Schema]
  }
}
