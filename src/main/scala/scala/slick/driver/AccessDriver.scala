package scala.slick.driver

import scala.language.implicitConversions
import scala.slick.SlickException
import scala.slick.ast._
import scala.slick.compiler.{QueryCompiler, CompilerState, Phase}
import scala.slick.jdbc.{ResultSetType, JdbcType}
import scala.slick.lifted._
import scala.slick.profile.{RelationalProfile, SqlProfile, Capability}
import scala.slick.util.MacroSupport.macroSupportInterpolation
import java.util.UUID
import java.sql.{Blob, Clob, Date, Time, Timestamp, SQLException, PreparedStatement, ResultSet}

/** Slick driver for Microsoft Access via JdbcOdbcDriver.
  *
  * This driver implements [[scala.slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.columnDefaults]]:
  *     Access does not allow the definition of default values through ODBC but
  *     only via OLEDB/ADO. Trying to generate DDL SQL code which uses this
  *     feature throws a SlickException.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.foreignKeyActions]]:
  *     All foreign key actions are ignored. Access supports CASCADE and SET
  *     NULL but not through ODBC, only via OLEDB/ADO.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.functionDatabase]],
  *     [[scala.slick.profile.RelationalProfile.capabilities.functionUser]]:
  *     <code>Functions.user</code> and <code>Functions.database</code> are
  *     not available in Access. Slick will return empty strings for both.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.likeEscape]]:
  *     Access does not allow you to specify a custom escape character for
  *     <code>like</code>.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.pagingDrop]]:
  *     <code>Drop(n)</code> modifiers are not supported. Trying to generate
  *     SQL code which uses this feature throws a SlickException.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.pagingPreciseTake]]:
  *     <code>Take(n)</code> modifiers are mapped to <code>SELECT TOP n</code>
  *     which may return more rows than requested if they are not unique.</li>
  *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequence]]:
  *     Sequences are not supported by Access</li>
  *   <li>[[scala.slick.driver.JdbcProfile.capabilities.returnInsertKey]],
  *     [[scala.slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
  *     Returning columns from an INSERT operation is not supported. Trying to
  *     execute such an insert statement throws a SlickException.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.typeBlob]]:
  *     Trying to use <code>java.sql.Blob</code> objects causes a NPE in the
  *     JdbcOdbcDriver. Binary data in the form of <code>Array[Byte]</code> is
  *     supported.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.setByteArrayNull]]:
  *     Setting an Option[ Array[Byte] ] column to None causes an Exception
  *     in the JdbcOdbcDriver.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.typeBigDecimal]],
  *     [[scala.slick.profile.RelationalProfile.capabilities.typeLong]]:
  *     Access does not support decimal or long integer types.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.zip]]:
  *     Row numbers (required by <code>zip</code> and
  *     <code>zipWithIndex</code>) are not supported. Trying to generate SQL
  *     code which uses this feature throws a SlickException.</li>
  *   <li>[[scala.slick.profile.RelationalProfile.capabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[scala.slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side because there
  *     is no native support for them.</li>
  * </ul>
  */
@deprecated("AccessDriver will be removed when we drop support for Java versions < 8", "2.1")
trait AccessDriver extends JdbcDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalProfile.capabilities.columnDefaults
    - RelationalProfile.capabilities.foreignKeyActions
    - RelationalProfile.capabilities.functionDatabase
    - RelationalProfile.capabilities.functionUser
    - RelationalProfile.capabilities.likeEscape
    - RelationalProfile.capabilities.pagingDrop
    - RelationalProfile.capabilities.pagingPreciseTake
    - SqlProfile.capabilities.sequence
    - JdbcProfile.capabilities.returnInsertKey
    - JdbcProfile.capabilities.returnInsertOther
    - RelationalProfile.capabilities.setByteArrayNull
    - RelationalProfile.capabilities.typeBigDecimal
    - RelationalProfile.capabilities.typeBlob
    - RelationalProfile.capabilities.typeLong
    - RelationalProfile.capabilities.zip
    - JdbcProfile.capabilities.createModel
    - RelationalProfile.capabilities.joinFull
    - JdbcProfile.capabilities.insertOrUpdate
  )

  def integralTypes = Set(
    java.sql.Types.INTEGER,
    java.sql.Types.BIGINT,
    java.sql.Types.SMALLINT,
    java.sql.Types.TINYINT
  )

  override protected def computeQueryCompiler =
    super.computeQueryCompiler.addBefore(new ExistsToCount, QueryCompiler.relationalPhases.head)

  val retryCount = 10
  override val columnTypes = new JdbcTypes(retryCount)

  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "YESNO"
    case java.sql.Types.BLOB => "LONGBINARY"
    case java.sql.Types.SMALLINT => "INTEGER"
    case java.sql.Types.BIGINT => "LONG"
    case java.sql.Types.TINYINT => "BYTE"
    case _ => super.defaultSqlTypeName(tmd)
  }

  /* Using Auto or ForwardOnly causes a NPE in the JdbcOdbcDriver */
  override val invokerMutateType: ResultSetType = ResultSetType.ScrollInsensitive
  /* Access goes forward instead of backward after deleting the current row in a mutable result set */
  override val invokerPreviousAfterDelete = true

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("&")
    override protected val hasPiFunction = false
    override protected val hasRadDegConversion = false

    protected final case class StarAnd(child: Node) extends UnaryNode with SimplyTypedNode {
      type Self = StarAnd
      protected[this] def nodeRebuild(child: Node) = StarAnd(child)
      protected def buildType = UnassignedType
    }

    protected def extendWithDummyColumn(c: Comprehension, rn: AnonSymbol): Comprehension = c.select match {
      case Some(Pure(StructNode(ch), _)) =>
        c.copy(select = Some(Pure(StructNode(ch :+ (rn -> LiteralNode(1))))), fetch = None, offset = None)
      case Some(Pure(ProductNode(ch), _)) =>
        c.copy(select = Some(Pure(StructNode(ch.toIndexedSeq.map(n => newSym -> n) :+ (rn -> LiteralNode(1))))), fetch = None, offset = None)
      case Some(Pure(n, _)) =>
        c.copy(select = Some(Pure(StructNode(IndexedSeq(newSym -> n, rn -> LiteralNode(1))))), fetch = None, offset = None)
      case None =>
        // should not happen at the outermost layer, so copying an extra row does not matter
        c.copy(select = Some(Pure(StructNode(IndexedSeq(rn -> StarAnd(LiteralNode(1)))))), fetch = None, offset = None)
    }

    override protected def buildComprehension(c: Comprehension) =
      if(c.offset.isDefined) throw new SlickException("Access does not support drop(...) calls")
      else super.buildComprehension(c)

    override protected def buildSelectModifiers(c: Comprehension) {
      if(!c.fetch.isEmpty) b"top ${c.fetch.get} "
    }

    override protected def buildFrom(n: Node, alias: Option[Symbol], skipParens: Boolean = false): Unit = building(FromPart) {
      n match {
        case j @ Join(leftGen, rightGen, left: Comprehension, right: Comprehension, jt, LiteralNode(true)) =>
          val sym = new AnonSymbol
          buildFrom(extendWithDummyColumn(left, sym), Some(leftGen))
          b" ${jt.sqlName} join "
          buildFrom(extendWithDummyColumn(right, sym), Some(rightGen))
          val on = Apply(Library.==, Seq(Select(Ref(leftGen), sym), Select(Ref(rightGen), sym)))(ScalaBaseType.booleanType)
          b" on !$on"
        case n => super.buildFrom(n, alias, skipParens)
      }
    }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case StarAnd(ch) => b"*, !$ch"
      case c: IfThenElse => {
        b"switch("
        var first = true
        c.ifThenClauses.foreach { case (i, t) =>
          if(first) first = false
          else b","
          b"$i,$t"
        }
        c.elseClause match {
          case LiteralNode(null) =>
          case n =>
            if(!first) b += ","
            b"1=1,$n"
        }
        b")"
      }
      case Library.IfNull(l, r) => b"iif(isnull($l),$r,$l)"
      case Library.Cast(ch @ _*) =>
        (if(ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
          else jdbcTypeFor(c.nodeType).sqlTypeName
        ).toLowerCase match {
          case "boolean" => b"cbool(${ch(0)})"
          case "double" => b"cdbl(${ch(0)})"
          case "integer" => b"cint(${ch(0)})"
          case "long" => b"clng(${ch(0)})"
          case t if t.startsWith("varchar") && integralTypes.contains(jdbcTypeFor(ch(0).nodeType).sqlType) =>
            b"format(${ch(0)}, '#############################0')"
          case tn =>
            throw new SlickException(s"""Cannot represent cast to type "$tn" in Access SQL""")
        }
      case Library.Reverse(n) => b"StrReverse($n)"
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

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = ()
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder) {
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
      if(autoIncrement && !customSqlType) sb append "AUTOINCREMENT"
      else appendType(sb)
      autoIncrement = false
      appendOptions(sb)
    }

    override protected def appendOptions(sb: StringBuilder) {
      if(notNull) sb append " NOT NULL"
      if(defaultLiteral ne null) throw new SlickException("Default values are not supported by AccessDriver")
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  class JdbcTypes(retryCount: Int) extends super.JdbcTypes {
    /* Retry all parameter and result operations because ODBC can randomly throw
     * S1090 (Invalid string or buffer length) exceptions. Retrying the call can
     * sometimes work around the bug. */
    trait Retry[T] extends JdbcType[T] {
      protected[this] def retry[T](g: => T) = {
        def f(c: Int): T = try g catch { case e: SQLException if c > 0 && e.getSQLState == "S1090" => f(c-1) }
        f(retryCount)
      }
      abstract override def getValue(r: ResultSet, idx: Int) = retry(super.getValue(r, idx))
      abstract override def wasNull(r: ResultSet, idx: Int) = retry(super.wasNull(r, idx))
      abstract override def setValue(v: T, p: PreparedStatement, idx: Int) = retry(super.setValue(v, p, idx))
      abstract override def setNull(p: PreparedStatement, idx: Int) = retry(super.setNull(p, idx))
      abstract override def updateValue(v: T, r: ResultSet, idx: Int) = retry(super.updateValue(v, r, idx))
      abstract override def updateNull(r: ResultSet, idx: Int) = retry(super.updateNull(r, idx))
    }

    // This is a nightmare... but it seems to work
    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlType = java.sql.Types.BLOB
      override def setNull(p: PreparedStatement, idx: Int) = p.setString(idx, null)
    }

    /* Access does not have a TINYINT (8-bit signed type), so we use 16-bit signed. */
    class ByteJdbcType extends super.ByteJdbcType {
      override def setValue(v: Byte, p: PreparedStatement, idx: Int) = p.setShort(idx, v)
      override def getValue(r: ResultSet, idx: Int) = r.getInt(idx).toByte
      override def updateValue(v: Byte, r: ResultSet, idx: Int) = r.updateInt(idx, v)
    }

    class LongJdbcType extends super.LongJdbcType {
      override def setValue(v: Long, p: PreparedStatement, idx: Int) = p.setString(idx, v.toString)
      override def setNull(p: PreparedStatement, idx: Int) = p.setString(idx, null)
    }

    override val booleanJdbcType = new BooleanJdbcType with Retry[Boolean]
    override val blobJdbcType = new BlobJdbcType with Retry[Blob]
    override val bigDecimalJdbcType = new BigDecimalJdbcType with Retry[BigDecimal]
    override val byteJdbcType = new ByteJdbcType with Retry[Byte]
    override val byteArrayJdbcType = new ByteArrayJdbcType with Retry[Array[Byte]]
    override val clobJdbcType = new ClobJdbcType with Retry[Clob]
    override val dateJdbcType = new DateJdbcType with Retry[Date]
    override val doubleJdbcType = new DoubleJdbcType with Retry[Double]
    override val floatJdbcType = new FloatJdbcType with Retry[Float]
    override val intJdbcType = new IntJdbcType with Retry[Int]
    override val longJdbcType = new LongJdbcType with Retry[Long]
    override val shortJdbcType = new ShortJdbcType with Retry[Short]
    override val stringJdbcType = new StringJdbcType with Retry[String]
    override val timeJdbcType = new TimeJdbcType with Retry[Time]
    override val timestampJdbcType = new TimestampJdbcType with Retry[Timestamp]
    override val nullJdbcType = new NullJdbcType with Retry[Null]
    override val uuidJdbcType = new UUIDJdbcType with Retry[UUID]
  }

  /** Query compiler phase that rewrites Exists calls in projections to
    * equivalent CountAll > 0 calls which can then be fused into aggregation
    * sub-queries in the fuseComprehensions phase. */
  class ExistsToCount extends Phase {
    val name = "access:existsToCount"

    def apply(state: CompilerState) = state.map(n => tr(n, false))

    protected def tr(n: Node, inSelect: Boolean): Node = n match {
      case b @ Bind(_, _, sel) => b.nodeMapChildren { n => tr(n, n eq sel) }
      case f: FilteredQuery => f.nodeMapChildren(tr(_, false))
      case a @ Library.Exists(ch) if inSelect =>
        Library.>.typed[Boolean](Library.CountAll.typed[Int](tr(ch, true)), LiteralNode(0))
      case n => n.nodeMapChildren(ch => tr(ch, inSelect))
    }
  }
}

@deprecated("AccessDriver will be removed when we drop support for Java versions < 8", "2.1")
object AccessDriver extends AccessDriver
