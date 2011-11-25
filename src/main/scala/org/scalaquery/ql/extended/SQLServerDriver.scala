package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._
import org.scalaquery.SQueryException
import java.sql.{Timestamp, Time, Date}
import org.scalaquery.session.{PositionedParameters, PositionedResult, ResultSetType}

/**
 * ScalaQuery driver for Microsoft SQL Server.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 *
 * <ul>
 *   <li>Sequences are not supported because SQLServer does not have this
 *     feature.</li>
 *   <li>There is limited support for take() and drop() modifiers on
 *     subqueries. Due to the way these modifiers have to be encoded for SQL
 *     Server, they only work on top-level queries or sub-queries of simple
 *     COUNT(*) top-level queries.</li>
 * </ul>
 *
 * @author szeiger
 */
class SQLServerDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[SQLServerDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[SQLServerDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new SQLServerTypeMapperDelegates
  override val sqlUtils = new SQLServerSQLUtils

  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new SQLServerDDLBuilder(table, this).buildDDL
  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new SQLServerQueryBuilder(query, nc, None, this)
}

object SQLServerDriver extends SQLServerDriver

class SQLServerTypeMapperDelegates extends BasicTypeMapperDelegates {
  import SQLServerTypeMapperDelegates._
  override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
  override val byteTypeMapperDelegate = new ByteTypeMapperDelegate
  override val dateTypeMapperDelegate = new DateTypeMapperDelegate
  override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
  override val uuidTypeMapperDelegate = new BasicTypeMapperDelegates.UUIDTypeMapperDelegate {
    override def sqlTypeName = "UNIQUEIDENTIFIER"
  }
}

object SQLServerTypeMapperDelegates {
  /* SQL Server does not have a proper BOOLEAN type. The suggested workaround is
   * BIT with constants 1 and 0 for TRUE and FALSE. */
  class BooleanTypeMapperDelegate extends BasicTypeMapperDelegates.BooleanTypeMapperDelegate {
    override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
  }
  /* Selecting a straight Date or Timestamp literal fails with a NPE (probably
   * because the type information gets lost along the way), so we cast all Date
   * and Timestamp values to the proper type. This work-around does not seem to
   * be required for Time values. */
  class DateTypeMapperDelegate extends BasicTypeMapperDelegates.DateTypeMapperDelegate {
    override def valueToSQLLiteral(value: Date) = "{fn convert({d '" + value + "'}, DATE)}"
  }
  class TimestampTypeMapperDelegate extends BasicTypeMapperDelegates.TimestampTypeMapperDelegate {
    /* TIMESTAMP in SQL Server is a data type for sequence numbers. What we
     * want here is DATETIME. */
    override def sqlTypeName = "DATETIME"
    override def valueToSQLLiteral(value: Timestamp) = "{fn convert({ts '" + value + "'}, DATETIME)}"
  }
  /* SQL Server's TINYINT is unsigned, so we use SMALLINT instead to store a signed byte value.
   * The JDBC driver also does not treat signed values correctly when reading bytes from result
   * sets, so we read as Short and then convert to Byte. */
  class ByteTypeMapperDelegate extends BasicTypeMapperDelegates.ByteTypeMapperDelegate {
    override def sqlTypeName = "SMALLINT"
    //def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
    //def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
    override def nextValue(r: PositionedResult) = r.nextShort.toByte
    //def updateValue(v: Byte, r: PositionedResult) = r.updateByte(v)
  }
}

class SQLServerQueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: SQLServerDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import profile.sqlUtils._
  import ExtendedQueryOps._

  override type Self = SQLServerQueryBuilder
  override protected val supportsTuples = false
  override protected val concatOperator = Some("+")

  val hasTakeDrop = !query.typedModifiers[TakeDrop].isEmpty
  val hasDropOnly = query.typedModifiers[TakeDrop] match {
    case TakeDrop(None, Some(_)) :: _ => true
    case _ => false
  }
  val isCountAll = query.reified match {
    case ColumnOps.CountAll(_) => true
    case _ => false
  }

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new SQLServerQueryBuilder(query, nc, Some(this), profile)

  override def buildSelect(b: SQLBuilder): Unit = {
    /* Rename at top level if we need to wrap with TakeDrop code */
    innerBuildSelect(b, hasTakeDrop)
    insertAllFromClauses()
  }

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(t), Some(d)) :: _ =>
        b += "WITH T AS (SELECT TOP " += (t+d) += ' '
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") SELECT "
        addCopyColumns(b)
        b += " FROM T WHERE \"c0r\" BETWEEN " += (d+1) += " AND " += (t+d)
        if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
      case TakeDrop(Some(t), None) :: _ =>
        b += "WITH T AS (SELECT TOP " += t += ' '
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") SELECT "
        addCopyColumns(b)
        b += " FROM T WHERE \"c0r\" BETWEEN 1 AND " += t
        if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
      case TakeDrop(None, Some(d)) :: _ =>
        b += "WITH T AS (SELECT "
        expr(query.reified, b, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") SELECT "
        addCopyColumns(b)
        b += " FROM T WHERE \"c0r\" > " += d
        if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
      case _ =>
        super.innerBuildSelectNoRewrite(b, rename)
    }
  }

  def addCopyColumns(b: SQLBuilder) {
    if(isCountAll) b += "count(*)"
    else if(maxColumnPos == 0) b += "*"
    else b.sep(1 to maxColumnPos, ",")(i => b += "\"c" += i += "\"")
  }

  override protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean): Unit = {
    c match {
      /* Convert proper BOOLEANs which should be returned from a SELECT
       * statement into pseudo-boolean BIT values 1 and 0 */
      case c: Column[_] if topLevel && !rename && b == selectSlot && c.typeMapper(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
        b += "case when "
        innerExpr(c, b)
        b += " then 1 else 0 end"
      case _ => super.expr(c, b, rename, topLevel)
    }
    if(topLevel && hasTakeDrop) {
      b += ",ROW_NUMBER() OVER ("
      appendOrderClause(b)
      if(query.typedModifiers[Ordering].isEmpty) b += "ORDER BY (SELECT NULL)"
      b += ") AS \"c0r\""
    }
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    /* Create TRUE and FALSE values because SQL Server lacks boolean literals */
    case c @ ConstColumn(true) => b += "(1=1)"
    case c @ ConstColumn(false) => b += "(1=0)"

    /* Convert pseudo-booleans from tables and subqueries to real booleans */
    case n: NamedColumn[_] if n.typeMapper(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
      b += "("; super.innerExpr(c, b); b += " != 0)"
    case c @ SubqueryColumn(pos, sq, tm) if tm(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
      b += "("; super.innerExpr(c, b); b += " != 0)"

    case ColumnOps.CountAll(q) if(hasTakeDrop) => b += "*"; localTableName(q)
    case _ => super.innerExpr(c, b)
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    appendConditions(b)
    appendGroupClause(b)
    appendHavingConditions(b)
    if(!hasDropOnly) appendOrderClause(b)
  }

  override protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if(o.nullOrdering == Ordering.NullsLast && !desc) {
      b += "case when ("
      expr(o.by, b)
      b += ") is null then 1 else 0 end,"
    } else if(o.nullOrdering == Ordering.NullsFirst && desc) {
      b += "case when ("
      expr(o.by, b)
      b += ") is null then 0 else 1 end,"
    }
    expr(o.by, b)
    if(desc) b += " desc"
  }

  /* Move COUNT(*) into subqueries even if they have TakeDrop modifiers.
   * It will be treated specially there to make it work. */
  override protected def rewriteCountStarQuery(q: Query[_, _]) = true
}

class SQLServerDDLBuilder(table: AbstractBasicTable[_], profile: SQLServerDriver) extends BasicDDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class SQLServerColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " IDENTITY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new SQLServerColumnDDLBuilder(c)
}

class SQLServerSQLUtils extends BasicSQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "BIT"
    case java.sql.Types.BLOB => "IMAGE"
    case java.sql.Types.CLOB => "TEXT"
    case _ => super.mapTypeName(tmd)
  }
}
