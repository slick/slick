package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.basic._

object MySQLDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[MySQLDriver.type]
  type TypeMapperDelegatesT = MySQLTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[MySQLDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new MySQLTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new MySQLQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new MySQLDDLBuilder(table).buildDDL
}

trait MySQLTypeMapperDelegates extends BasicTypeMapperDelegates {
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

class MySQLQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: MySQLDriver.type)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = MySQLQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new MySQLQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += "concat("; expr(l, b); b += ','; expr(r, b); b += ')'
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

class MySQLDDLBuilder(table: AbstractBasicTable[_]) extends BasicDDLBuilder(table, MySQLDriver) {
  override protected def dropForeignKey(fk: ForeignKey[_ <: AbstractTable[_]]) = {
    "ALTER TABLE " + table.tableName + " DROP FOREIGN KEY " + fk.name
  }
}
