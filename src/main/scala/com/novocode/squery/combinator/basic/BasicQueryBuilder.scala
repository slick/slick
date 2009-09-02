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
    innerBuildSelect(b, false)
    insertFromClauses()
    b.build
  }

  protected def innerBuildSelect(b: SQLBuilder, rename: Boolean) {
    b += "SELECT "
    expr(Node(query.value), b, rename)
    fromSlot = b.createSlot
    appendClauses(b)
  }

  protected def appendClauses(b: SQLBuilder): Unit = {
    appendConditions(b)
    appendGroupClause(b)
    appendHavingConditions(b)
    appendOrderClause(b)
  }

  protected def appendGroupClause(b: SQLBuilder): Unit = query.typedModifiers[Grouping] match {
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

  protected def appendOrderClause(b: SQLBuilder): Unit = query.typedModifiers[Ordering] match {
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

  def buildUpdate = {
    if(!query.condHaving.isEmpty || !query.modifiers.isEmpty)
      throw new SQueryException("A query for an UPDATE statement must not have any modifiers other than WHERE restrictions")
    val b = new SQLBuilder += "UPDATE "
    val tableNameSlot = b.createSlot
    b += " SET "
    var tableName: String = null
    var table: Node = null

    def handleColumn(node: Node, idx: Int) {
      (node match {
        case NamedColumn(t @ Table(tn), n, tm, _) => (tn, n, tm, t)
        case NamedColumn(t @ Table.Alias(Table(tn)), n, tm, _) => (tn, n, tm, t)
        case n => throw new SQueryException("Cannot create an UPDATE statement from a \""+n+
          "\" expression; A single named column or a projection of named columns from the same aliased or base table is required")
      }) match { case (tn, n, tm, t) =>
        if(tableName eq null) { tableName = tn; table = t; }
        else if(tableName != tn)
          throw new SQueryException("All columns for an UPDATE statement must be from the same table")
        b += n += '=' +?= { (p, param) =>
          val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
          tm(profile).asInstanceOf[TypeMapperDelegate[Any]].setValue(v, p)
        }
      }
    }

    Node(query.value) match {
      case p: Projection[_] => {
        var i = 0
        for(ch <- p.nodeChildren) {
          if(i > 0) b += ','
          handleColumn(ch, i)
          i += 1
        }
      }
      case n => handleColumn(n, -1)
    }
    nc = nc.overrideName(table, tableName) // Alias table to itself because DELETE does not support aliases
    tableNameSlot += tableName
    appendConditions(b)
    if(localTables.size > 1)
      throw new SQueryException("An UPDATE statement must not use more than one table at the top level")
    b.build
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
    case SimpleLiteral(w) => b += w
    case Operator.CountAll(q) => b += "count(" += localTableName(q) += ".*)"
    case Operator.CountDistinct(e) => { b += "count(distinct "; expr(e, b); b += ')' }
    case s: SimpleBinaryOperator => { b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')' }
    case query:Query[_] => { b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")" }
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
    case t: Table[_] => expr(Node(t.*), b)
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
      b += "("; subQueryBuilderFor(sq).innerBuildSelect(b, rename); b += ") " += name
    case Subquery(Union(all, sqs), rename) =>
      b += "("
      var first = true
      for(sq <- sqs) {
        if(!first) b += (if(all) " UNION ALL " else " UNION ")
        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
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
