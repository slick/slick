package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.SlickException
import scala.slick.session.{PositionedParameters, PositionedResult, ResultSetType}
import java.util.UUID
import java.sql.{Blob, Clob, Date, Time, Timestamp, SQLException}
import scala.slick.compiler.{QueryCompiler, CompilationState, Phase}

/**
 * Slick driver for Microsoft Access via JdbcOdbcDriver.
 *
 * This driver implements the [[scala.slick.driver.ExtendedProfile]]
 * ''without'' the following capabilities:
 *
 * <ul>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.columnDefaults]]:
 *     Access does not allow the definition of default values through ODBC but
 *     only via OLEDB/ADO. Trying to generate DDL SQL code which uses this
 *     feature throws a SlickException.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.foreignKeyActions]]:
 *     All foreign key actions are ignored. Access supports CASCADE and SET
 *     NULL but not through ODBC, only via OLEDB/ADO.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.functionDatabase]],
 *     [[scala.slick.driver.BasicProfile.capabilities.functionUser]]:
 *     <code>Functions.user</code> and <code>Functions.database</code> are
 *     not available in Access. Slick will return empty strings for both.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.likeEscape]]:
 *     Access does not allow you to specify a custom escape character for
 *     <code>like</code>.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.pagingDrop]]:
 *     <code>Drop(n)</code> modifiers are not supported. Trying to generate
 *     SQL code which uses this feature throws a SlickException.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.pagingPreciseTake]]:
 *     <code>Take(n)</code> modifiers are mapped to <code>SELECT TOP n</code>
 *     which may return more rows than requested if they are not unique.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.sequence]]:
 *     Sequences are not supported by Access</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.returnInsertKey]],
 *     [[scala.slick.driver.BasicProfile.capabilities.returnInsertOther]]:
 *     Returning columns from an INSERT operation is not supported. Trying to
 *     execute such an insert statement throws a SlickException.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.typeBlob]]:
 *     Trying to use <code>java.sql.Blob</code> objects causes a NPE in the
 *     JdbcOdbcDriver. Binary data in the form of <code>Array[Byte]</code> is
 *     supported.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.typeLong]]:
 *     Access does not have a long integer type.</li>
 *   <li>[[scala.slick.driver.BasicProfile.capabilities.zip]]:
 *     Row numbers (required by <code>zip</code> and
 *     <code>zipWithIndex</code>) are not supported. Trying to generate SQL
 *     code which uses this feature throws a SlickException.</li>
 * </ul>
 *
 * @author szeiger
 */
trait AccessDriver extends ExtendedDriver { driver =>

  override val capabilities: Set[Capability] = (BasicProfile.capabilities.all
    - BasicProfile.capabilities.columnDefaults
    - BasicProfile.capabilities.foreignKeyActions
    - BasicProfile.capabilities.functionDatabase
    - BasicProfile.capabilities.functionUser
    - BasicProfile.capabilities.likeEscape
    - BasicProfile.capabilities.pagingDrop
    - BasicProfile.capabilities.pagingPreciseTake
    - BasicProfile.capabilities.sequence
    - BasicProfile.capabilities.returnInsertKey
    - BasicProfile.capabilities.returnInsertOther
    - BasicProfile.capabilities.typeBlob
    - BasicProfile.capabilities.typeLong
    - BasicProfile.capabilities.zip
    )

  override val Implicit: Implicits = new Implicits
  override val simple: SimpleQL = new Implicits with SimpleQL
  override val compiler =
    QueryCompiler.relational.addBefore(new ExistsToCount, QueryCompiler.relationalPhases.head)

  class Implicits extends super.Implicits {
    override implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[T, U] = new QueryInvoker(q)
  }

  val retryCount = 10
  override val typeMapperDelegates = new TypeMapperDelegates(retryCount)

  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  override def createInsertBuilder(node: Node): InsertBuilder = new InsertBuilder(node)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "YESNO"
    case java.sql.Types.BLOB => "LONGBINARY"
    case java.sql.Types.SMALLINT => "INTEGER"
    case java.sql.Types.BIGINT => "LONG"
    case java.sql.Types.TINYINT => "BYTE"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("&")
    override protected val hasPiFunction = false
    override protected val hasRadDegConversion = false

    override protected def buildComprehension(c: Comprehension) =
      if(c.offset.isDefined) throw new SlickException("Access does not support drop(...) calls")
      else super.buildComprehension(c)

    override protected def buildSelectModifiers(c: Comprehension) {
      if(!c.fetch.isEmpty) b"top ${c.fetch.get} "
    }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case c: Case.CaseNode => {
        b"switch("
        var first = true
        c.clauses.reverseIterator.foreach { case Case.WhenNode(l, r) =>
          if(first) first = false
          else b","
          b"$l,$r"
        }
        c.elseClause match {
          case ConstColumn(null) =>
          case n =>
            if(!first) b += ","
            b"1=1,$n"
        }
        b")"
      }
      case Library.IfNull(l, r) => b"iif(isnull($l),$r,$l)"
      case a @ Library.Cast(ch @ _*) =>
        (if(ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
          else a.asInstanceOf[Typed].tpe.asInstanceOf[TypeMapper[_]].apply(driver).sqlTypeName
        ).toLowerCase match {
          case "integer" => b"cint(${ch(0)})"
          case "long" => b"clng(${ch(0)})"
          case tn =>
            throw new SlickException(s"""Cannot represent cast to type "$tn" in Access SQL""")
        }
      case RowNumber(_) => throw new SlickException("Access does not support row numbers")
      case _ => super.expr(c, skipParens)
    }

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc) {
        b"(1-isnull($n)),"
      } else if(o.nulls.first && o.direction.desc) {
        b"(1-isnull($n)) desc,"
      }
      expr(n)
      if(o.direction.desc) b" desc"
    }

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = ()
  }

  class InsertBuilder(node: Node) extends super.InsertBuilder(node) {
    override def buildReturnColumns(node: Node, table: String): IndexedSeq[FieldSymbol] =
      throw new SlickException("Returning columns from INSERT statements is not supported by Access")
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def addForeignKey(fk: ForeignKey[_ <: TableNode, _], sb: StringBuilder) {
      sb append "CONSTRAINT " append quoteIdentifier(fk.name) append " FOREIGN KEY("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
      sb append ") REFERENCES " append quoteIdentifier(fk.targetTable.tableName) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      sb append ")"
      // Foreign key actions are not supported by Access so we ignore them
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
      if(autoIncrement) {
        sb append "AUTOINCREMENT"
        autoIncrement = false
      }
      else sb append sqlType
      appendOptions(sb)
    }

    override protected def appendOptions(sb: StringBuilder) {
      if(notNull) sb append " NOT NULL"
      if(defaultLiteral ne null) throw new SlickException("Default values are not supported by AccessDriver")
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  class TypeMapperDelegates(retryCount: Int) extends super.TypeMapperDelegates {
    /* Retry all parameter and result operations because ODBC can randomly throw
     * S1090 (Invalid string or buffer length) exceptions. Retrying the call can
     * sometimes work around the bug. */
    trait Retry[T] extends TypeMapperDelegate[T] {
      abstract override def nextValue(r: PositionedResult) = {
        def f(c: Int): T =
          try super.nextValue(r) catch {
            case e: SQLException if c > 0 && e.getSQLState == "S1090" => f(c-1)
          }
        f(retryCount)
      }
      abstract override def setValue(v: T, p: PositionedParameters) = {
        def f(c: Int): Unit =
          try super.setValue(v, p) catch {
            case e: SQLException if c > 0 && e.getSQLState == "S1090" => f(c-1)
          }
        f(retryCount)
      }
      abstract override def setOption(v: Option[T], p: PositionedParameters) = {
        def f(c: Int): Unit =
          try super.setOption(v, p) catch {
            case e: SQLException if c > 0 && e.getSQLState == "S1090" => f(c-1)
          }
        f(retryCount)
      }
      abstract override def updateValue(v: T, r: PositionedResult) = {
        def f(c: Int): Unit =
          try super.updateValue(v, r) catch {
            case e: SQLException if c > 0 && e.getSQLState == "S1090" => f(c-1)
          }
        f(retryCount)
      }
    }

    // This is a nightmare... but it seems to work
    class UUIDTypeMapperDelegate extends super.UUIDTypeMapperDelegate {
      override def sqlType = java.sql.Types.BLOB
      override def setOption(v: Option[UUID], p: PositionedParameters) =
        if(v == None) p.setString(null) else p.setBytes(toBytes(v.get))
      override def nextValueOrElse(d: =>UUID, r: PositionedResult) = { val v = nextValue(r); if(v.eq(null) || r.rs.wasNull) d else v }
      override def nextOption(r: PositionedResult): Option[UUID] = { val v = nextValue(r); if(v.eq(null) || r.rs.wasNull) None else Some(v) }
    }

    /* Access does not have a TINYINT (8-bit signed type), so we use 16-bit signed. */
    class ByteTypeMapperDelegate extends super.ByteTypeMapperDelegate {
      override def setValue(v: Byte, p: PositionedParameters) = p.setShort(v)
      override def setOption(v: Option[Byte], p: PositionedParameters) = p.setIntOption(v.map(_.toInt))
      override def nextValue(r: PositionedResult) = r.nextInt.toByte
      override def updateValue(v: Byte, r: PositionedResult) = r.updateInt(v)
    }

    class LongTypeMapperDelegate extends super.LongTypeMapperDelegate {
      override def setValue(v: Long, p: PositionedParameters) = p.setString(v.toString)
      override def setOption(v: Option[Long], p: PositionedParameters) = p.setStringOption(v.map(_.toString))
    }

    override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate with Retry[Boolean]
    override val blobTypeMapperDelegate = new BlobTypeMapperDelegate with Retry[Blob]
    override val bigDecimalTypeMapperDelegate = new BigDecimalTypeMapperDelegate with Retry[BigDecimal]
    override val byteTypeMapperDelegate = new ByteTypeMapperDelegate with Retry[Byte]
    override val byteArrayTypeMapperDelegate = new ByteArrayTypeMapperDelegate with Retry[Array[Byte]]
    override val clobTypeMapperDelegate = new ClobTypeMapperDelegate with Retry[Clob]
    override val dateTypeMapperDelegate = new DateTypeMapperDelegate with Retry[Date]
    override val doubleTypeMapperDelegate = new DoubleTypeMapperDelegate with Retry[Double]
    override val floatTypeMapperDelegate = new FloatTypeMapperDelegate with Retry[Float]
    override val intTypeMapperDelegate = new IntTypeMapperDelegate with Retry[Int]
    override val longTypeMapperDelegate = new LongTypeMapperDelegate with Retry[Long]
    override val shortTypeMapperDelegate = new ShortTypeMapperDelegate with Retry[Short]
    override val stringTypeMapperDelegate = new StringTypeMapperDelegate with Retry[String]
    override val timeTypeMapperDelegate = new TimeTypeMapperDelegate with Retry[Time]
    override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate with Retry[Timestamp]
    override val nullTypeMapperDelegate = new NullTypeMapperDelegate with Retry[Null]
    override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate with Retry[UUID]
  }

  class QueryInvoker[Q, R](q: Query[Q, _ <: R]) extends super.QueryInvoker[Q, R](q) {
    /* Using Auto or ForwardOnly causes a NPE in the JdbcOdbcDriver */
    override protected val mutateType: ResultSetType = ResultSetType.ScrollInsensitive
    /* Access goes forward instead of backward after deleting the current row in a mutable result set */
    override protected val previousAfterDelete = true
  }

  /** Query compiler phase that rewrites Exists calls in projections to
    * equivalent CountAll > 0 calls which can then be fused into aggregation
    * sub-queries in the fuseComprehensions phase. */
  class ExistsToCount extends Phase {
    val name = "access:existsToCount"

    def apply(n: Node, state: CompilationState) = tr(n, false)

    protected def tr(n: Node, inSelect: Boolean): Node = n match {
      case b @ Bind(_, _, sel) => b.nodeMapChildren { n => tr(n, n eq sel) }
      case f: FilteredQuery => f.nodeMapChildren(tr(_, false))
      case Library.Exists(ch) if inSelect =>
        Library.>.typed[Boolean](Library.CountAll(tr(ch, true)), ConstColumn(0))
      case n => n.nodeMapChildren(ch => tr(ch, inSelect))
    }
  }
}

object AccessDriver extends AccessDriver
