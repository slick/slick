package com.novocode.squery.combinator.sql

import scala.collection.mutable.{HashMap, HashSet}
import java.io.PrintWriter
import com.novocode.squery.combinator._
import com.novocode.squery.RefId

private class QueryBuilder (val query: Query[_], private[this] var nc: NamingContext, parent: Option[QueryBuilder]) {

  //TODO Pull tables out of subqueries where needed
  //TODO Support unions
  //TODO Rename columns where needed

  private[this] val localTables = new HashMap[String, Node]
  private[this] val declaredTables = new HashSet[String]
  private[this] val subQueryBuilders = new HashMap[RefId[Query[_]], QueryBuilder]
  private[this] var fromSlot: SQLBuilder = _

  private[this] def localTableName(n: Node) = n match {
    case Join.JoinPart(table, from) =>
      // Special case for Joins: A join combines multiple tables but does not alias them
      localTables(nc.nameFor(from)) = from
      nc.nameFor(table)
    case _ =>
      val name = nc.nameFor(n)
      localTables(name) = n
      name
  }

  private def isDeclaredTable(name: String): Boolean =
    if(declaredTables contains name) true
    else parent.map(_.isDeclaredTable(name)).getOrElse(false)
  
  private[this] def subQueryBuilderFor(q: Query[_]) =
    subQueryBuilders.getOrElseUpdate(RefId(q), new QueryBuilder(q, nc, Some(this)))

  private def buildSelect: (String, SQLBuilder.Setter) = {
    val b = new SQLBuilder
    buildSelect(Node(query.value), b)
    insertFromClauses()
    b.build
  }

  private def buildSelect(value: Node, b: SQLBuilder): Unit = value match {
    case Operator.Count(e) => { b += "SELECT count(*) from ("; buildSelect(e, b); b += ")" }
    case _ =>
      b += "SELECT "
      expr(value, b)
      fromSlot = b.createSlot
      appendConditions(b)
      appendGroupClause(b)
      appendOrderClause(b)
  }

  private def appendGroupClause(b: SQLBuilder): Unit = query.groupBy match {
    case x :: xs => {
      b += " GROUP BY "
      expr(x.by, b)
      for(x <- xs) {
        b += ","
        expr(x.by, b)
      }
    }
    case _ =>
  }

  private def appendOrderClause(b: SQLBuilder): Unit = query.orderBy match {
    case x :: xs => {
      b += " ORDER BY "
      appendOrdering(x, b)
      for(x <- xs) {
        b += ","
        appendOrdering(x, b)
      }
    }
    case _ =>
  }

  private def appendOrdering(o: OrderBy.Ordering, b: SQLBuilder) {
    expr(o.by, b)
    if(o.isInstanceOf[OrderBy.Desc]) b += " descending"
  }

  private def buildDelete = {
    val b = new SQLBuilder += "DELETE FROM "
    val (delTable, delTableName) = Node(query.value) match {
      case t @ Table.Alias(base:Table[_]) => (t, base.tableName)
      case t:Table[_] => (t, t.tableName)
      case n => throw new SQueryException("Cannot create a DELETE statement from an \""+n+
        "\" expression; An aliased or base table is required")
    }
    b += delTableName
    nc = nc.overrideName(delTable, delTableName) // Alias table to itself because DELETE does not support aliases
    appendConditions(b)
    if(localTables.size > 1)
      throw new SQueryException("Conditions of a DELETE statement must not reference other tables")
    for(qb <- subQueryBuilders.values)
      qb.insertFromClauses()
    b.build
  }

  private def buildUpdate: String = Node(query.value) match {
    case p:Projection[_] => ""
      val b = new SQLBuilder += "UPDATE "
      /*val (updateTable, updateTableName) = Node(query.value) match {
        case t @ Table.Alias(base:Table[_]) => (t, base.tableName)
        case t:Table[_] => (t, t.tableName)
        case n => throw new SQueryException("Cannot create a UPDATE statement from an \""+n+
          "\" expression; An aliased or base table is required")
      }
      b += updateTableName += " SET "
      nc = nc.overrideName(updateTable, updateTableName) // Alias table to itself because UPDATE does not support aliases
      appendConditions(b)
      if(localTables.size > 1)
        throw new SQueryException("Conditions of a DELETE statement must not reference other tables")
      for(qb <- subQueryBuilders.values)
        qb.insertFromClauses()*/
      b.toString
    case n => throw new SQueryException("Cannot create a UPDATE statement from an \""+n+
      "\" expression; Not a Projection; A projection of named columns from the same aliased or base table is required")
  }

  private[this] def expr(c: Node, b: SQLBuilder): Unit = c match {
    case NullNode => b += "null"
    case Operator.Not(Operator.Is(l, NullNode)) => { b += '('; expr(l, b); b += " is not null)" }
    case Operator.Not(e) => { b += "not "; expr(e, b) }
    case Operator.Is(l, NullNode) => { b += '('; expr(l, b); b += " is null)" }
    case Operator.Is(l, r) => { b += '('; expr(l, b); b += '='; expr(r, b); b += ')' }
    case s: SimpleFunction[_] => {
      b += s.name += '('
      var first = true
      for(ch <- s.nodeChildren) {
        if(first) first = false
        else b += ','
        expr(ch, b)
      }
      b += ')'
    }
    case s: SimpleBinaryOperator[_] => { b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')' }
    case query:Query[_] => { b += "("; subQueryBuilderFor(query).buildSelect(Node(query.value), b); b += ")" }
    //case Union.UnionPart(_) => "*"
    case p: Projection[_] => {
      var first = true
      p.nodeChildren.foreach { c =>
        if(first) first = false else b += ','
        expr(c, b)
      }
    }
    case c @ ConstColumn(v) => b += c.typeMapper.valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { p => c.typeMapper.setValue(v, p); p }
    case n: NamedColumn[_] => { b += localTableName(n.table) += '.' += n.name }
    case a @ Table.Alias(t: WithOp) => expr(t.mapOp(_ => a), b)
    case t: Table[_] => expr(t.*, b)
    case t: TableBase[_] => b += localTableName(t) += ".*"
    case _ => throw new SQueryException("Don't know what to do with node \""+c+"\" in an expression")
  }

  private[this] def appendConditions(b: SQLBuilder): Unit = query.cond match {
    case a :: l =>
      b += " WHERE "
      expr(Node(a), b)
      for(e <- l) { b += " AND "; expr(Node(e), b) }
    case Nil => ()
  }

  private def insertFromClauses() {
    var first = true
    for((name, t) <- localTables) {
      if(!parent.map(_.isDeclaredTable(name)).getOrElse(false)) {
        if(first) { fromSlot += " FROM "; first = false }
        else fromSlot += ','
        table(t, name, fromSlot)
        declaredTables += name
      }
    }
    for(qb <- subQueryBuilders.values)
      qb.insertFromClauses()
  }

  private[this] def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case Table.Alias(base: Table[_]) =>
      b += base.tableName += ' ' += name
    case base: Table[_] =>
      b += base.tableName += ' ' += name
    case Table.Alias(j: Join[_,_]) => {
      var first = true
      for(n <- j.nodeChildren) {
        if(first) first = false
        else b += " natural join "
        table(n, nc.nameFor(n), b)
      }
    }
  }
}

object QueryBuilder {
  def buildSelect(query: Query[_], nc: NamingContext) = new QueryBuilder(query, nc, None).buildSelect

  def buildDelete(query: Query[_], nc: NamingContext) = new QueryBuilder(query, nc, None).buildDelete

  def buildUpdate(query: Query[_], nc: NamingContext) = new QueryBuilder(query, nc, None).buildUpdate
}
