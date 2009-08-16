package com.novocode.squery.combinator.basic

import scala.collection.mutable.{HashMap, HashSet}
import com.novocode.squery.{RefId, SQueryException}
import com.novocode.squery.combinator._

class ConcreteBasicQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], _profile: BasicProfile)
extends BasicQueryBuilder(_query, _nc, parent, _profile) {
  type Self = BasicQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new ConcreteBasicQueryBuilder(query, nc, Some(this), profile)
}

abstract class BasicQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], _profile: BasicProfile) {

  //TODO Pull tables out of subqueries where needed
  //TODO Support unions

  type Self <: BasicQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext): Self

  protected val profile = _profile
  protected val query: Query[_] = _query
  protected var nc: NamingContext = _nc
  protected val localTables = new HashMap[String, Node]
  protected val declaredTables = new HashSet[String]
  protected val subQueryBuilders = new HashMap[RefId[Query[_]], Self]
  protected var fromSlot: SQLBuilder = _

  protected def localTableName(n: Node) = n match {
    case Join.JoinPart(table, from) =>
      // Special case for Joins: A join combines multiple tables but does not alias them
      localTables(nc.nameFor(from)) = from
      nc.nameFor(table)
    case _ =>
      val name = nc.nameFor(n)
      localTables(name) = n
      name
  }

  protected def isDeclaredTable(name: String): Boolean =
    if(declaredTables contains name) true
    else parent.map(_.isDeclaredTable(name)).getOrElse(false)
  
  protected def subQueryBuilderFor(q: Query[_]) =
    subQueryBuilders.getOrElseUpdate(RefId(q), createSubQueryBuilder(q, nc))

  def buildSelect: SQLBuilder.Result = {
    val b = new SQLBuilder
    buildSelect(Node(query.value), b, false)
    insertFromClauses()
    b.build
  }

  protected def buildSelect(value: Node, b: SQLBuilder, rename: Boolean): Unit = value match {
    case Operator.Count(e) =>
      b += "SELECT count(*) from ("; buildSelect(e, b, false); b += ")"
      if(rename) b += " as c1"
    case _ =>
      b += "SELECT "
      expr(value, b, rename)
      fromSlot = b.createSlot
      appendConditions(b)
      appendGroupClause(b)
      appendHavingConditions(b)
      appendOrderClause(b)
  }

  protected def appendGroupClause(b: SQLBuilder): Unit = query.groupings match {
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

  protected def appendOrderClause(b: SQLBuilder): Unit = query.orderings match {
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

  protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    expr(o.by, b)
    if(o.isInstanceOf[Ordering.Desc]) b += " desc"
  }

  def buildDelete = {
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
    if(query.condHaving ne Nil)
      throw new SQueryException("DELETE statement must contain a HAVING clause")
    for(qb <- subQueryBuilders.values)
      qb.insertFromClauses()
    b.build
  }

  def buildUpdate: String = Node(query.value) match {
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

  protected def expr(c: Node, b: SQLBuilder): Unit = expr(c, b, false)

  protected def expr(c: Node, b: SQLBuilder, rename: Boolean): Unit = {
    var pos = 0
    c match {
      case p: Projection[_] => {
        p.nodeChildren.foreach { c =>
          if(pos != 0) b += ','
          pos += 1
          expr(c, b)
          if(rename) b += " as c" += pos.toString
        }
      }
      case _ => innerExpr(c, b)
    }
    if(rename && pos == 0) b += " as c1"
  }

  protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ConstColumn(null) => b += "null"
    case Operator.Not(Operator.Is(l, ConstColumn(null))) => { b += '('; expr(l, b); b += " is not null)" }
    case Operator.Not(e) => { b += "(not "; expr(e, b); b+= ')' }
    case Operator.InSet(e, seq, tm, bind) => if(seq.isEmpty) b += "false" else {
      b += '('; expr(e, b); b += " in ("
      if(bind) {
        var first = true
        for(x <- seq) {
          if(first) first = false else b += ','
          b +?= { (p, param) => tm(profile).setValue(x, p) }
        }
      }
      else b += seq.map(tm(profile).valueToSQLLiteral).mkString(",")
      b += "))"
    }
    case Operator.Is(l, ConstColumn(null)) => { b += '('; expr(l, b); b += " is null)" }
    case Operator.Is(l, r) => { b += '('; expr(l, b); b += '='; expr(r, b); b += ')' }
    case s: SimpleFunction => {
      b += s.name += '('
      var first = true
      for(ch <- s.nodeChildren) {
        if(first) first = false
        else b += ','
        expr(ch, b)
      }
      b += ')'
    }
    case Operator.CountDistinct(e) => { b += "count(distinct "; expr(e, b); b += ')' }
    case s: SimpleBinaryOperator => { b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')' }
    case query:Query[_] => { b += "("; subQueryBuilderFor(query).buildSelect(Node(query.value), b, false); b += ")" }
    //case Union.UnionPart(_) => "*"
    case c @ ConstColumn(v) => b += c.typeMapper(profile).valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(profile).setValue(v, p) }
    case ParameterColumn(idx, tm) => b +?= { (p, param) =>
      val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
      tm(profile).setValue(v, p)
    }
    case c: Case.CaseColumn[_] => {
      b += "(case"
      c.clauses.foldRight(()) { (w,_) =>
        b += " when "
        expr(w.left, b)
        b += " then "
        expr(w.right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n =>
          b += " else "
          expr(n, b)
      }
      b += " end)"
    }
    case n: NamedColumn[_] => { b += localTableName(n.table) += '.' += n.name }
    case SubqueryColumn(pos, sq) => { b += localTableName(sq) += ".c" += pos.toString }
    case a @ Table.Alias(t: WithOp) => expr(t.mapOp(_ => a), b)
    case t: Table[_] => expr(t.*, b)
    case t: TableBase[_] => b += localTableName(t) += ".*"
    case _ => throw new SQueryException("Don't know what to do with node \""+c+"\" in an expression")
  }

  protected def appendConditions(b: SQLBuilder): Unit = query.cond match {
    case a :: l =>
      b += " WHERE "
      expr(Node(a), b)
      for(e <- l) { b += " AND "; expr(Node(e), b) }
    case Nil => ()
  }

  protected def appendHavingConditions(b: SQLBuilder): Unit = query.condHaving match {
    case a :: l =>
      b += " HAVING "
      expr(Node(a), b)
      for(e <- l) { b += " AND "; expr(Node(e), b) }
    case Nil => ()
  }

  protected def insertFromClauses() {
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

  protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case Table.Alias(base: Table[_]) =>
      b += base.tableName += ' ' += name
    case base: Table[_] =>
      b += base.tableName += ' ' += name
    case Subquery(sq: Query[_], rename) =>
      b += "("; subQueryBuilderFor(sq).buildSelect(Node(sq.value), b, rename); b += ") " += name
    case Subquery(Union(all, sqs), rename) =>
      b += "("
      var first = true
      for(sq <- sqs) {
        if(!first) b += (if(all) " UNION ALL " else " UNION ")
        subQueryBuilderFor(sq).buildSelect(Node(sq.value), b, first && rename)
        first = false
      }
      b += ") " += name
    case j: Join[_,_] => {
      var first = true
      for(n <- j.nodeChildren) {
        if(first) first = false
        else b += " natural join "
        table(n, nc.nameFor(n), b)
      }
    }
  }
}
