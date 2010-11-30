package org.scalaquery.ql.extended

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._

class MySQLDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[MySQLDriver]
  type TypeMapperDelegatesT = MySQLTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[MySQLDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new MySQLTypeMapperDelegates
  override val sqlUtils = new MySQLSQLUtils

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new MySQLQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new MySQLDDLBuilder(table, this).buildDDL
  override def buildSequenceDDL(seq: Sequence[_]): DDL = new MySQLSequenceDDLBuilder(seq, this).buildDDL
}

object MySQLDriver extends MySQLDriver

class MySQLTypeMapperDelegates extends BasicTypeMapperDelegates {
  override val stringTypeMapperDelegate = new BasicTypeMapperDelegates.StringTypeMapperDelegate {
    override def valueToSQLLiteral(value: String) = if(value eq null) "NULL" else {
      val sb = new StringBuilder
      sb append '\''
      for(c <- value) c match {
        case '\'' => sb append "\\'"
        case '"' => sb append "\\\""
        case 0 => sb append "\\0"
        case 26 => sb append "\\Z"
        case '\b' => sb append "\\b"
        case '\n' => sb append "\\n"
        case '\r' => sb append "\\r"
        case '\t' => sb append "\\t"
        case '\\' => sb append "\\\\"
        case _ => sb append c
      }
      sb append '\''
      sb.toString
    }
  }
}

class MySQLQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: MySQLDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._
  import profile.sqlUtils._

  override type Self = MySQLQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new MySQLQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += "concat("; expr(l, b); b += ','; expr(r, b); b += ')'
    case Sequence.Nextval(seq) => b += quoteIdentifier(seq.name + "_nextval") += "()"
    case Sequence.Currval(seq) => b += quoteIdentifier(seq.name + "_currval") += "()"
    case _ => super.innerExpr(c, b)
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    super.appendClauses(b)
    appendLimitClause(b)
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d)) => b += " LIMIT " += d += ',' += t
    case TakeDrop(Some(t), None) => b += " LIMIT " += t
    case TakeDrop(None, Some(d)) => b += " LIMIT " += d += ",18446744073709551615"
    case _ =>
  }

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }

  override protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if(o.nullOrdering == Ordering.NullsLast && !desc) {
      b += "isnull("
      expr(o.by, b)
      b += "),"
    } else if(o.nullOrdering == Ordering.NullsFirst && desc) {
      b += "isnull("
      expr(o.by, b)
      b += ") desc,"
    }
    expr(o.by, b)
    if(desc) b += " desc"
  }
}

class MySQLDDLBuilder(table: AbstractBasicTable[_], profile: MySQLDriver) extends BasicDDLBuilder(table, profile) {
  override protected def dropForeignKey(fk: ForeignKey[_ <: AbstractTable[_]]) = {
    "ALTER TABLE " + table.tableName + " DROP FOREIGN KEY " + fk.name
  }
}

class MySQLSequenceDDLBuilder[T](seq: Sequence[T], profile: MySQLDriver) extends BasicSequenceDDLBuilder(seq, profile) {
  import profile.sqlUtils._

  override def buildDDL: DDL = {
    import seq.integral._
    val sqlType = seq.typeMapper(profile).sqlTypeName
    val t = sqlType + " not null"
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val minValue = seq._minValue getOrElse (if(desc) fromInt(java.lang.Integer.MIN_VALUE) else one)
    val maxValue = seq._maxValue getOrElse (if(desc) fromInt(-1) else fromInt(java.lang.Integer.MAX_VALUE))
    val start = seq._start.getOrElse(if(desc) maxValue else minValue)
    val beforeStart = start - increment
    if(!seq._cycle && (seq._minValue.isDefined && desc || seq._maxValue.isDefined && !desc))
      throw new SQueryException("Sequences with limited size and without CYCLE are not supported by MySQLDriver's sequence emulation")
    val incExpr = if(seq._cycle) {
      if(desc) "if(id-"+(-increment)+"<"+minValue+","+maxValue+",id-"+(-increment)+")"
      else "if(id+"+increment+">"+maxValue+","+minValue+",id+"+increment+")"
    } else {
      "id+("+increment+")"
    }
    //TODO Implement currval function
    new DDL {
      val createPhase1 = Iterable(
        "create table " + quoteIdentifier(seq.name + "_seq") + " (id " + t + ")",
        "insert into " + quoteIdentifier(seq.name + "_seq") + " values (" + beforeStart + ")",
        "create function " + quoteIdentifier(seq.name + "_nextval") + "() returns " + sqlType + " begin update " +
          quoteIdentifier(seq.name + "_seq") + " set id=last_insert_id(" + incExpr + "); return last_insert_id(); end")
      val createPhase2 = Nil
      val dropPhase1 = Nil
      val dropPhase2 = Iterable(
        "drop function " + quoteIdentifier(seq.name + "_nextval"),
        "drop table " + quoteIdentifier(seq.name + "_seq"))
    }
  }
}

class MySQLSQLUtils extends BasicSQLUtils {
  override def quoteIdentifier(id: String) = '`' + id + '`'
}
