package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._
import org.scalaquery.SQueryException
import org.scalaquery.session.ResultSetType

/**
 * ScalaQuery driver for Microsoft Access via JdbcOdbcDriver.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 * <ul>
 *   <li>Sequences are not supported because Access does not have them.</li>
 *   <li><code>O.Default</code> is not supported because Access does not allow
 *     the definition of default values through ODBC but only via OLEDB/ADO.
 *     Trying to generate DDL SQL code which uses this feature throws an
 *     SQueryException.</li>
 *   <li>All foreign key actions are ignored. Access supports CASCADE and
 *     SET NULL but not through ODBC, only via OLEDB/ADO.</li>
 *   <li><code>Take(n)</code> modifiers are mapped to <code>SELECT TOP n</code>
 *     which may return more rows than requested if they are not unique.</li>
 *   <li><code>Drop(n)</code> modifiers are not supported. Trying to generate
 *     SQL code which uses this feature throws an SQueryException.</li>
 *   <li><code>Functions.user</code> and <code>Functions.database</code> are
 *     not implemented by Access. ScalaQuery will generate the usual ODBC
 *     scalar function calls but Access cannot run them.</li>
 *   <li>Trying to use <code>java.sql.Blob</code> objects causes a NPE in the
 *     JdbcOdbcDriver. Binary data in the form of <code>Array[Byte]</code> is
 *     supported.</li>
 * </ul>
 *
 * @author szeiger
 */
class AccessDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[AccessDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[AccessDriver] {
    implicit val scalaQueryDriver = self
    override implicit def queryToQueryInvoker[T](q: Query[ColumnBase[T]]): BasicQueryInvoker[T] = new AccessQueryInvoker(q, scalaQueryDriver)
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}
  override val sqlUtils = new AccessSQLUtils

  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new AccessDDLBuilder(table, this).buildDDL
  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new AccessQueryBuilder(query, nc, None, this)
}

object AccessDriver extends AccessDriver

class AccessQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: AccessDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import profile.sqlUtils._
  import ExtendedQueryOps._

  override type Self = AccessQueryBuilder

  val pi = "3.1415926535897932384626433832795"

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new AccessQueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(_ , Some(_)) :: _ =>
        throw new SQueryException("Access does not support drop(...) modifiers")
      case TakeDrop(Some(0), _) :: _ =>
        /* Access does not allow TOP 0, so we use this workaround
         * to force the query to return no results */
        b += "SELECT * FROM ("
        super.innerBuildSelectNoRewrite(b, rename)
        b += ") WHERE FALSE"
      case TakeDrop(Some(n), _) :: _ =>
        selectSlot = b.createSlot
        selectSlot += "SELECT TOP " += n += ' '
        expr(Node(query.value), selectSlot, rename, true)
        fromSlot = b.createSlot
        appendClauses(b)
      case _ =>
        super.innerBuildSelectNoRewrite(b, rename)
    }
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case c: Case.CaseColumn[_] => {
      b += "switch("
      var first = true
      c.clauses.foldRight(()) { (w,_) =>
        if(first) first = false
        else b += ","
        expr(w.left, b)
        b += ","
        expr(w.right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n =>
          if(!first) b += ","
          b += "1=1,"
          expr(n, b)
      }
      b += ")"
    }
    case fk: ForeignKey[_] =>
      /* Access does not support row value constructor syntax (tuple syntax),
       * so we need to untuple and compare the individual columns (which
       * may not not kosher in the presence of NULLs). */
      val cols = untupleColumn(fk.left) zip untupleColumn(fk.right)
      b += "("
      for((l,r) <- b.sep(cols, " and ")) {
        expr(l, b); b += "="; expr(r, b);
      }
      b += ")"
    case ColumnOps.Degrees(ch, _) => b += "(180/"+pi+"*"; expr(ch, b); b += ')'
    case ColumnOps.Radians(ch, _) => b += "("+pi+"/180*"; expr(ch, b); b += ')'
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "&"; expr(r, b); b += ')'
    case ColumnOps.IfNull(l, r) => b += "iif(isnull("; expr(l, b); b += "),"; expr(r, b); b += ','; expr(l, b); b += ')'
    case ColumnOps.Exists(q: Query[_]) =>
      // Access doesn't like double parens around the sub-expression
      b += "exists"; expr(q, b)
    case a @ ColumnOps.AsColumnOf(ch, name) => name match {
      case None if a.typeMapper eq TypeMapper.IntTypeMapper =>
        b += "cint("; expr(ch, b); b += ')'
      case None if a.typeMapper eq TypeMapper.LongTypeMapper =>
        b += "clng("; expr(ch, b); b += ')'
      case Some(n) if n.toLowerCase == "integer" =>
        b += "cint("; expr(ch, b); b += ')'
      case Some(n) if n.toLowerCase == "long" =>
        b += "clng("; expr(ch, b); b += ')'
      case _ =>
        val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
        throw new SQueryException("Cannot represent cast to type \"" + tn + "\" in Access SQL")
    }
    case s: SimpleScalarFunction if s.name == "pi" => b += pi
    case _ => super.innerExpr(c, b)
  }

  override protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if(o.nullOrdering == Ordering.NullsLast && !desc) {
      b += "(1-isnull("
      expr(o.by, b)
      b += ")),"
    } else if(o.nullOrdering == Ordering.NullsFirst && desc) {
      b += "(1-isnull("
      expr(o.by, b)
      b += ")) desc,"
    }
    expr(o.by, b)
    if(desc) b += " desc"
  }
}

class AccessDDLBuilder(table: AbstractBasicTable[_], profile: AccessDriver) extends BasicDDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class AccessColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {

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
      if(defaultLiteral ne null) throw new SQueryException("Default values are not supported by AccessDriver")
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new AccessColumnDDLBuilder(c)

  override protected def addForeignKey(fk: ForeignKey[_ <: AbstractTable[_]], sb: StringBuilder) {
    sb append "CONSTRAINT " append quoteIdentifier(fk.name) append " FOREIGN KEY("
    addForeignKeyColumnList(fk.sourceColumns, sb, table.tableName)
    sb append ") REFERENCES " append quoteIdentifier(fk.targetTable.tableName) append "("
    addForeignKeyColumnList(fk.targetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
    sb append ")"
    /*if(fk.onUpdate == ForeignKeyAction.Cascade || fk.onUpdate == ForeignKeyAction.SetNull)
      sb append " ON UPDATE " append fk.onUpdate.action
    if(fk.onDelete == ForeignKeyAction.Cascade || fk.onDelete == ForeignKeyAction.SetNull)
      sb append " ON DELETE " append fk.onDelete.action*/
  }
}

class AccessSQLUtils extends BasicSQLUtils {
  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "YESNO"
    case java.sql.Types.BLOB => "LONGBINARY"
    case _ => super.mapTypeName(tmd)
  }
}

class AccessQueryInvoker[R](q: Query[ColumnBase[R]], profile: BasicProfile) extends BasicQueryInvoker[R](q, profile) {
  /* Using Auto or ForwardOnly causes a NPE in the JdbcOdbcDriver */
  override protected val mutateType: ResultSetType = ResultSetType.ScrollInsensitive
  /* Access goes forward instead of backward after deleting the current row in a mutable result set */
  override protected val previousAfterDelete = true
}
