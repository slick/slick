package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.session.{PositionedParameters, PositionedResult, ResultSetType}
import java.util.UUID
import java.sql.{Blob, Clob, Date, Time, Timestamp, SQLException}

/**
 * SLICK driver for Microsoft Access via JdbcOdbcDriver.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 * <ul>
 *   <li>Sequences are not supported because Access does not have them.</li>
 *   <li><code>O.Default</code> is not supported because Access does not allow
 *     the definition of default values through ODBC but only via OLEDB/ADO.
 *     Trying to generate DDL SQL code which uses this feature throws a
 *     SlickException.</li>
 *   <li>All foreign key actions are ignored. Access supports CASCADE and
 *     SET NULL but not through ODBC, only via OLEDB/ADO.</li>
 *   <li><code>Take(n)</code> modifiers are mapped to <code>SELECT TOP n</code>
 *     which may return more rows than requested if they are not unique.</li>
 *   <li><code>Drop(n)</code> modifiers are not supported. Trying to generate
 *     SQL code which uses this feature throws a SlickException.</li>
 *   <li>Row numbers (required by <code>zip</code> and <code>zipWithIndex</code>)
 *     are not supported. Trying to generate SQL code which uses this feature
 *     throws a SlickException.</li>
 *   <li><code>Functions.user</code> and <code>Functions.database</code> are
 *     not available in Access. SLICK will return empty strings for
 *     both.</li>
 *   <li>Trying to use <code>java.sql.Blob</code> objects causes a NPE in the
 *     JdbcOdbcDriver. Binary data in the form of <code>Array[Byte]</code> is
 *     supported.</li>
 * </ul>
 *
 * @author szeiger
 */
trait AccessDriver extends ExtendedDriver { driver =>

  override val Implicit: Implicits = new Implicits {
    override implicit def queryToQueryInvoker[T, U](q: Query[T, _ <: U]): QueryInvoker[T, U] = new QueryInvoker(q)
  }

  val retryCount = 10
  override val typeMapperDelegates = new TypeMapperDelegates(retryCount)

  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "YESNO"
    case java.sql.Types.BLOB => "LONGBINARY"
    case _ => super.mapTypeName(tmd)
  }

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("&")

    val pi = "3.1415926535897932384626433832795"

    override protected def buildComprehension(c: Comprehension) =
      if(c.offset.isDefined) throw new SlickException("Access does not support drop(...) calls")
      else super.buildComprehension(c)

    override protected def buildSelectModifiers(c: Comprehension) {
      if(!c.fetch.isEmpty) b += "top " += c.fetch.get += " "
    }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case c: Case.CaseNode => {
        b += "switch("
        var first = true
        c.clauses.foldRight(()) { (w,_) =>
          if(first) first = false
          else b += ","
          expr(w.asInstanceOf[Case.WhenNode].left)
          b += ","
          expr(w.asInstanceOf[Case.WhenNode].right)
        }
        c.elseClause match {
          case ConstColumn(null) =>
          case n =>
            if(!first) b += ","
            b += "1=1,"
            expr(n)
        }
        b += ")"
      }
      case Library.Degrees(ch) =>
        if(!skipParens) b += '('
        b += "180/"+pi+"*"
        expr(ch)
        if(!skipParens) b += ')'
      case Library.Radians(ch) =>
        if(!skipParens) b += '('
        b += pi+"/180*"
        expr(ch)
        if(!skipParens) b += ')'
      case Library.IfNull(l, r) => b += "iif(isnull("; expr(l); b += "),"; expr(r); b += ','; expr(l); b += ')'
      case a @ Library.Cast(ch @ _*) =>
        (if(ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
          else mapTypeName(a.asInstanceOf[Typed].tpe.asInstanceOf[TypeMapper[_]].apply(driver))
        ).toLowerCase match {
          case "integer" => b += "cint("; expr(ch(0)); b += ')'
          case "long" => b += "clng("; expr(ch(0)); b += ')'
          case tn =>
            throw new SlickException("Cannot represent cast to type \"" + tn + "\" in Access SQL")
        }
      case Library.User() => b += "''"
      case Library.Database() => b += "''"
      case Library.Pi() => b += pi
      case RowNumber(_) => throw new SlickException("Access does not support row numbers")
      case _ => super.expr(c, skipParens)
    }

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc) {
        b += "(1-isnull("
        expr(n)
        b += ")),"
      } else if(o.nulls.first && o.direction.desc) {
        b += "(1-isnull("
        expr(n)
        b += ")) desc,"
      }
      expr(n)
      if(o.direction.desc) b += " desc"
    }

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = ()
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
      override def sqlTypeName = "LONGBINARY"
      override def setOption(v: Option[UUID], p: PositionedParameters) =
        if(v == None) p.setString(null) else p.setBytes(toBytes(v.get))
      override def nextValueOrElse(d: =>UUID, r: PositionedResult) = { val v = nextValue(r); if(v.eq(null) || r.rs.wasNull) d else v }
      override def nextOption(r: PositionedResult): Option[UUID] = { val v = nextValue(r); if(v.eq(null) || r.rs.wasNull) None else Some(v) }
    }

    /* Access does not have a TINYINT (8-bit signed type), so we use 16-bit signed. */
    class ByteTypeMapperDelegate extends super.ByteTypeMapperDelegate {
      override def sqlTypeName = "BYTE"
      override def setValue(v: Byte, p: PositionedParameters) = p.setShort(v)
      override def setOption(v: Option[Byte], p: PositionedParameters) = p.setIntOption(v.map(_.toInt))
      override def nextValue(r: PositionedResult) = r.nextInt.toByte
      override def updateValue(v: Byte, r: PositionedResult) = r.updateInt(v)
    }

    class ShortTypeMapperDelegate extends super.ShortTypeMapperDelegate {
      override def sqlTypeName = "INTEGER"
    }

    class LongTypeMapperDelegate extends super.LongTypeMapperDelegate {
      override def sqlTypeName = "LONG"
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
}

object AccessDriver extends AccessDriver
