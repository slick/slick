package scala.slick.driver

import scala.slick.SLICKException
import scala.slick.ql._
import scala.slick.ast._
import java.sql.{Timestamp, Time, Date}
import scala.slick.util.ValueLinearizer

/**
 * SLICK driver for SQLite.
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
 *     not available in SQLite. SLICK will return empty strings for
 *     both.</li>
 * </ul>
 */
class SQLiteDriver extends ExtendedDriver { driver =>

  override val typeMapperDelegates = new TypeMapperDelegates
  override def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new DDLBuilder(table).buildDDL

  class QueryBuilder(ast: Node, linearizer: ValueLinearizer[_]) extends super.QueryBuilder(ast, linearizer) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("||")

    /*
    override protected def table(t: Node, name: String): Unit = t match {
      case j: Join[_,_] => createJoin(j)
      case _ => super.table(t, name)
    }
    */

    override protected def appendOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc) {
        b += "("
        expr(n)
        b += ") is null,"
      } else if(o.nulls.first && o.direction.desc) {
        b += "("
        expr(n)
        b += ") is null desc,"
      }
      expr(n)
      if(o.direction.desc) b += " desc"
    }

    override protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = (take, drop) match {
      case (Some(t), Some(d)) => b += " LIMIT " += d += "," += t
      case (Some(t), None) => b += " LIMIT " += t
      case (None, Some(d)) => b += " LIMIT " += d += ",-1"
      case _ =>
    }

    override protected def innerExpr(c: Node): Unit = c match {
      case StdFunction("exists", q: Query[_, _]) =>
        // SQLite doesn't like double parens around the sub-expression
        b += "exists"; expr(q)
      case EscFunction("ucase", ch) => b += "upper("; expr(ch); b += ')'
      case EscFunction("lcase", ch) => b += "lower("; expr(ch); b += ')'
      case EscFunction("mod", l, r) => b += '('; expr(l); b += '%'; expr(r); b += ')'
      case EscFunction("ceiling", ch) => b += "round("; expr(ch); b += "+0.5)"
      case EscFunction("floor", ch) => b += "round("; expr(ch); b += "-0.5)"
      case EscFunction("user") => b += "''"
      case EscFunction("database") => b += "''"
      case s: SimpleFunction if s.scalar && s.name != "concat" =>
        /* The SQLite JDBC driver does not support ODBC {fn ...} escapes, so we try
         * unescaped function calls by default */
        b += s.name += '('
        b.sep(s.nodeChildren, ",")(expr)
        b += ")"
      case _ => super.innerExpr(c)
    }
  }

  class DDLBuilder(table: AbstractBasicTable[_]) extends super.DDLBuilder(table) {
    override protected def createColumnDDLBuilder(c: RawNamedColumn) = new ColumnDDLBuilder(c)

    protected class ColumnDDLBuilder(column: RawNamedColumn) extends super.ColumnDDLBuilder(column) {
      override protected def appendOptions(sb: StringBuilder) {
        if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
        if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
        else if(notNull) sb append " NOT NULL"
        else if(primaryKey) sb append " PRIMARY KEY"
      }
    }

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
        else throw new SLICKException("Table "+table.tableName+" defines multiple primary keys "+prevPK+" and "+pk.name)
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

  class TypeMapperDelegates extends super.TypeMapperDelegates {
    override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
    override val dateTypeMapperDelegate = new DateTypeMapperDelegate
    override val timeTypeMapperDelegate = new TimeTypeMapperDelegate
    override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
    override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate

    /* SQLite does not have a proper BOOLEAN type. The suggested workaround is
     * INTEGER with constants 1 and 0 for TRUE and FALSE. */
    class BooleanTypeMapperDelegate extends super.BooleanTypeMapperDelegate {
      override def sqlTypeName = "INTEGER"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }
    /* The SQLite JDBC driver does not support the JDBC escape syntax for
     * date/time/timestamp literals. SQLite expects these values as milliseconds
     * since epoch. */
    class DateTypeMapperDelegate extends super.DateTypeMapperDelegate {
      override def valueToSQLLiteral(value: Date) = value.getTime.toString
    }
    class TimeTypeMapperDelegate extends super.TimeTypeMapperDelegate {
      override def valueToSQLLiteral(value: Time) = value.getTime.toString
    }
    class TimestampTypeMapperDelegate extends super.TimestampTypeMapperDelegate {
      override def valueToSQLLiteral(value: Timestamp) = value.getTime.toString
    }
    class UUIDTypeMapperDelegate extends super.UUIDTypeMapperDelegate {
      override def sqlType = java.sql.Types.BLOB
    }
  }
}

object SQLiteDriver extends SQLiteDriver
