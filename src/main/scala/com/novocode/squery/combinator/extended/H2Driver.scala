package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext, Node, SQLBuilder, ColumnOps, TypeMapperDelegate, DDL}
import com.novocode.squery.combinator.basic._

object H2Driver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[H2Driver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[H2Driver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new H2QueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new H2DDLBuilder(table).buildDDL
}

class H2QueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: H2Driver.type)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = H2QueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new H2QueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelect(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(0), _) :: _ =>
        /* H2 ignores LIMIT 0 and treats negative limits as positive, so
         * we use this workaround to force the query to return no results */
        b += "SELECT * FROM (SELECT "
        expr(Node(query.value), b, rename)
        fromSlot = b.createSlot
        appendClauses(b)
        b += ") WHERE FALSE"
      case _ =>
        b += "SELECT "
        expr(Node(query.value), b, rename)
        fromSlot = b.createSlot
        appendClauses(b)
    }
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "||"; expr(r, b); b += ')'
    case _ => super.innerExpr(c, b)
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    super.appendClauses(b)
    appendLimitClause(b)
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(0), _) => // handled above in innerBuildSelect
    case TakeDrop(Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
    case TakeDrop(Some(t), None) => b += " LIMIT " += t
    case TakeDrop(None, Some(d)) => b += " LIMIT 0 OFFSET " += d
    case _ =>
  }
}

class H2DDLBuilder(table: AbstractBasicTable[_]) extends BasicDDLBuilder(table, H2Driver) {
  override protected def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR => "VARCHAR"
    case _ => super.mapTypeName(tmd)
  }
}
