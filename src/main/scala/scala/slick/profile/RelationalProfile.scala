package scala.slick.profile

import scala.language.{implicitConversions, higherKinds}
import scala.slick.ast._
import scala.slick.lifted._
import scala.slick.util.{TupleMethods, TupleSupport}
import FunctionSymbolExtensionMethods._
import scala.slick.SlickException

/**
 * A profile for relational databases that does not assume the existence
 * of SQL (or any other text-based language for executing statements).
 */
trait RelationalProfile extends BasicProfile with RelationalTableComponent
  with RelationalSequenceComponent with RelationalTypesComponent { driver: RelationalDriver =>

  override protected def computeCapabilities = super.computeCapabilities ++ RelationalProfile.capabilities.all

  val Implicit: Implicits
  val simple: SimpleQL

  trait Implicits extends super.Implicits with ImplicitColumnTypes {
    implicit def columnToOptionColumn[T : BaseTypedType](c: Column[T]): Column[Option[T]] = c.?
    implicit def valueToConstColumn[T : TypedType](v: T) = new LiteralColumn[T](v)
    implicit def columnToOrdered[T](c: Column[T]): ColumnOrdered[T] = c.asc
    implicit def tableQueryToTableQueryExtensionMethods[T <: Table[_], U](q: TableQuery[T, U]) =
      new TableQueryExtensionMethods[T, U](q)
  }

  trait SimpleQL extends super.SimpleQL with Implicits {
    type Table[T] = driver.Table[T]
    type Sequence[T] = driver.Sequence[T]
    val Sequence = driver.Sequence
    type ColumnType[T] = driver.ColumnType[T]
    type BaseColumnType[T] = driver.BaseColumnType[T]
  }

  class TableQueryExtensionMethods[T <: Table[_], U](val q: TableQuery[T, U]) {
    def ddl: SchemaDescription = buildTableSchemaDescription(q.unpackable.value)

    /** Create a ParameterizedQuery which selects all rows where the specified
      * key matches the parameter value. */
    def findBy[P](f: (T => Column[P]))(implicit tm: TypedType[P]): ParameterizedQuery[P, U] = {
      import driver.Implicit._
      Parameters[P].flatMap { p => q.filter(table => Library.==.column[Boolean](f(table).toNode, p.toNode)) }
    }
  }
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
    /** Supports full outer joins */
    val joinFull = Capability("relational.joinFull")
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
      functionUser, joinFull, joinRight, likeEscape, pagingDrop, pagingNested,
      pagingPreciseTake, setByteArrayNull, typeBigDecimal, typeBlob, typeLong,
      zip)
  }
}

trait RelationalDriver extends BasicDriver with RelationalProfile {
  override val profile: RelationalProfile = this
}

trait RelationalTableComponent { driver: RelationalDriver =>

  def buildTableSchemaDescription(table: Table[_]): SchemaDescription

  trait ColumnOptions {
    val PrimaryKey = ColumnOption.PrimaryKey
    def Default[T](defaultValue: T) = ColumnOption.Default[T](defaultValue)
    val AutoInc = ColumnOption.AutoInc
  }

  val columnOptions: ColumnOptions = new AnyRef with ColumnOptions

  abstract class Table[T](_tableTag: Tag, _schemaName: Option[String], _tableName: String) extends AbstractTable[T](_tableTag, _schemaName, _tableName) { table =>
    final type TableElementType = T

    def this(_tableTag: Tag, _tableName: String) = this(_tableTag, None, _tableName)

    def tableProvider: RelationalDriver = driver

    def tableIdentitySymbol: TableIdentitySymbol = SimpleTableIdentitySymbol(driver, schemaName.getOrElse("_"), tableName)

    val O: driver.columnOptions.type = columnOptions

    def column[C](n: String, options: ColumnOption[C]*)(implicit tm: TypedType[C]): Column[C] = new Column[C] {
      override def toNode =
        Path(FieldSymbol(n)(options, tm) :: (tableTag match {
          case r: RefTag => r.path
          case _ => List(tableNode.nodeIntrinsicSymbol)
        })).nodeTyped(tm)
      override def toString = (tableTag match {
        case r: RefTag => "(" + _tableName + " " + Path.toString(r.path) + ")"
        case _ => _tableName
      }) + "." + n
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
    extends Typed { seq =>

    def min(v: T) = new Sequence[T](name, Some(v), _maxValue, _increment, _start, _cycle)
    def max(v: T) = new Sequence[T](name, _minValue, Some(v), _increment, _start, _cycle)
    def inc(v: T) = new Sequence[T](name, _minValue, _maxValue, Some(v), _start, _cycle)
    def start(v: T) = new Sequence[T](name, _minValue, _maxValue, _increment, Some(v), _cycle)
    def cycle = new Sequence[T](name, _minValue, _maxValue, _increment, _start, true)

    final def next = Library.NextValue.column[T](toNode)
    final def curr = Library.CurrentValue.column[T](toNode)

    def toNode = SequenceNode(name)(_increment.map(integral.toLong).getOrElse(1))

    def ddl: SchemaDescription = buildSequenceSchemaDescription(this)
  }

  object Sequence {
    def apply[T : TypedType : Integral](name: String) = new Sequence[T](name, None, None, None, None, false)
  }
}

trait RelationalTypesComponent { driver: BasicDriver =>
  type ColumnType[T] <: TypedType[T]
  type BaseColumnType[T] <: ColumnType[T] with BaseTypedType[T]

  trait ImplicitColumnTypes {
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
    implicit def unitColumnType: BaseColumnType[Unit]
  }
}

/** This optional driver component provides a way to compose client-side
  * accessors for parameters and result sets. */
trait RelationalMappingCompilerComponent {
  /* TODO: PositionedResult isn't the right interface -- it assumes that
   * all columns will be read and updated in order. We should not limit it in
   * this way. */
  type RowReader
  type RowWriter
  type RowUpdater

  /** Create a CompiledMapping for parameters and result sets. Subclasses have
    * to provide profile-specific createColumnConverter implementations. */
  trait MappingCompiler {

    def compileMapping(n: Node): ResultConverter = n match {
      case p @ Path(_) => createColumnConverter(n, p, false)
      case OptionApply(p @ Path(_)) => createColumnConverter(n, p, true)
      case ProductNode(ch) =>
        new ProductResultConverter(ch.map(n => compileMapping(n))(collection.breakOut))
      case GetOrElse(ch, default) =>
        new GetOrElseResultConverter(compileMapping(ch), default)
      case TypeMapping(ch, toBase, toMapped) =>
        new TypeMappingResultConverter(compileMapping(ch), toBase, toMapped)
      case n =>
        throw new SlickException("Unexpected node in ResultSetMapping: "+n)
    }

    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter
  }

  /** A node that wraps a ResultConverter */
  final case class CompiledMapping(converter: ResultConverter, tpe: Type) extends NullaryNode with TypedNode {
    type Self = CompiledMapping
    def nodeRebuild = copy()
    override def toString = "CompiledMapping"
  }

  trait ResultConverter {
    def read(pr: RowReader): Any
    def update(value: Any, pr: RowUpdater): Unit
    def set(value: Any, pp: RowWriter): Unit
  }

  final class ProductResultConverter(children: IndexedSeq[ResultConverter]) extends ResultConverter {
    def read(pr: RowReader) = TupleSupport.buildTuple(children.map(_.read(pr)))
    def update(value: Any, pr: RowUpdater) =
      children.iterator.zip(value.asInstanceOf[Product].productIterator).foreach { case (ch, v) =>
        ch.update(v, pr)
      }
    def set(value: Any, pp: RowWriter) =
      children.iterator.zip(value.asInstanceOf[Product].productIterator).foreach { case (ch, v) =>
        ch.set(v, pp)
      }
  }

  final class GetOrElseResultConverter(child: ResultConverter, default: () => Any) extends ResultConverter {
    def read(pr: RowReader) = child.read(pr).asInstanceOf[Option[Any]].getOrElse(default())
    def update(value: Any, pr: RowUpdater) = child.update(Some(value), pr)
    def set(value: Any, pp: RowWriter) = child.set(Some(value), pp)
  }

  final class TypeMappingResultConverter(child: ResultConverter, toBase: Any => Any, toMapped: Any => Any) extends ResultConverter {
    def read(pr: RowReader) = toMapped(child.read(pr))
    def update(value: Any, pr: RowUpdater) = child.update(toBase(value), pr)
    def set(value: Any, pp: RowWriter) = child.set(toBase(value), pp)
  }
}
