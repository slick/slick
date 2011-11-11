package org.scalaquery.ql.extended

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._
import org.scalaquery.util.SQLBuilder._
import java.sql.{Timestamp, Time, Date}
import java.util.UUID

/**
 * ScalaQuery driver for SQLite.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 * <ul>
 *   <li>Sequences are not supported because SQLite does not have them.</li>
 *   <li>Blobs are not supported by the SQLite JDBC driver (but binary data in
 *     the form of <code>Array[Byte]</code> is).</li>
 *   <li>SQLite does not allow mutation of result sets. All cursors are
 *     read-only.</li>
 *   <li><code>Functions.user</code> and <code>Functions.database</code> are
 *     not available in SQLite. ScalaQuery will return empty strings for
 *     both.</li>
 * </ul>
 */
class SQLiteDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[SQLiteDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[SQLiteDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new SQLiteTypeMapperDelegates

  override def createQueryBuilder(query: Query[_, _], nc: NamingContext) = new SQLiteQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new SQLiteDDLBuilder(table, this).buildDDL
}

object SQLiteDriver extends SQLiteDriver

class SQLiteTypeMapperDelegates extends BasicTypeMapperDelegates {
  import SQLiteTypeMapperDelegates._
  override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
  override val dateTypeMapperDelegate = new DateTypeMapperDelegate
  override val timeTypeMapperDelegate = new TimeTypeMapperDelegate
  override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
  override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate
}

object SQLiteTypeMapperDelegates {
  /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
   * INTEGER with constants 1 and 0 for TRUE and FALSE. */
  class BooleanTypeMapperDelegate extends BasicTypeMapperDelegates.BooleanTypeMapperDelegate {
    override def sqlTypeName = "INTEGER"
    override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
  }
  /* The SQLite JDBC driver does not support the JDBC escape syntax for
   * date/time/timestamp literals. SQLite expects these values as milliseconds
   * since epoch. */
  class DateTypeMapperDelegate extends BasicTypeMapperDelegates.DateTypeMapperDelegate {
    override def valueToSQLLiteral(value: Date) = value.getTime.toString
  }
  class TimeTypeMapperDelegate extends BasicTypeMapperDelegates.TimeTypeMapperDelegate {
    override def valueToSQLLiteral(value: Time) = value.getTime.toString
  }
  class TimestampTypeMapperDelegate extends BasicTypeMapperDelegates.TimestampTypeMapperDelegate {
    override def valueToSQLLiteral(value: Timestamp) = value.getTime.toString
  }
  class UUIDTypeMapperDelegate extends BasicTypeMapperDelegates.UUIDTypeMapperDelegate {
    override def sqlType = java.sql.Types.BLOB
  }
}

class SQLiteDDLBuilder(table: AbstractBasicTable[_], profile: SQLiteDriver) extends BasicDDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class SQLiteColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if(notNull) sb append " NOT NULL"
      else if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new SQLiteColumnDDLBuilder(c)

  override def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for(n <- table.create_*) {
      if(first) first = false
      else b append ","
      createColumnDDLBuilder(n).appendColumn(b)
    }
    var prevPK: String = null
    for(pk <- table.primaryKeys) {
      if(prevPK eq null) prevPK = pk.name
      else throw new SQueryException("Table "+table.tableName+" defines multiple primary keys "+prevPK+" and "+pk.name)
      b append ","
      addPrimaryKey(pk, b)
    }
    for(fk <- table.foreignKeys) {
      b append ","
      addForeignKey(fk, b)
    }
    b append ")"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Iterable()
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP TABLE " + table.tableName)
    }
  }
}

class SQLiteQueryBuilder(_query: Query[_, _], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: SQLiteDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._
  import profile.sqlUtils._

  override type Self = SQLiteQueryBuilder
  override protected val supportsTuples = false
  override protected val concatOperator = Some("||")

  protected def createSubQueryBuilder(query: Query[_, _], nc: NamingContext) =
    new SQLiteQueryBuilder(query, nc, Some(this), profile)

  override protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case j: Join[_,_] => createJoin(j, b)
    case _ => super.table(t, name, b)
  }

  override protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if(o.nullOrdering == Ordering.NullsLast && !desc) {
      b += "("
      expr(o.by, b)
      b += ") is null,"
    } else if(o.nullOrdering == Ordering.NullsFirst && desc) {
      b += "("
      expr(o.by, b)
      b += ") is null desc,"
    }
    expr(o.by, b)
    if(desc) b += " desc"
  }

  override protected def appendLimitClause(b: SQLBuilder) = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d)) => b += " LIMIT " += d += "," += t
    case TakeDrop(Some(t), None) => b += " LIMIT " += t
    case TakeDrop(None, Some(d)) => b += " LIMIT " += d += ",-1"
    case _ =>
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case StdFunction("exists", q: Query[_, _]) =>
      // SQLite doesn't like double parens around the sub-expression
      b += "exists"; expr(q, b)
    case EscFunction("ucase", ch) => b += "upper("; expr(ch, b); b += ')'
    case EscFunction("lcase", ch) => b += "lower("; expr(ch, b); b += ')'
    case EscFunction("mod", l, r) => b += '('; expr(l, b); b += '%'; expr(r, b); b += ')'
    case EscFunction("ceiling", ch) => b += "round("; expr(ch, b); b += "+0.5)"
    case EscFunction("floor", ch) => b += "round("; expr(ch, b); b += "-0.5)"
    case EscFunction("user") => b += "''"
    case EscFunction("database") => b += "''"
    case s: SimpleFunction if s.scalar && s.name != "concat" =>
      /* The SQLite JDBC driver does not support ODBC {fn ...} escapes, so we try
       * unescaped function calls by default */
      b += s.name += '('
      b.sep(s.nodeChildren, ",")(expr(_, b))
      b += ")"
    case _ => super.innerExpr(c, b)
  }
}
