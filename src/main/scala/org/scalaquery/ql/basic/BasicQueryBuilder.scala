package org.scalaquery.ql.basic

import scala.collection.mutable.{HashMap, HashSet}
import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._

class ConcreteBasicQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], _profile: BasicProfile)
extends BasicQueryBuilder(_query, _nc, parent, _profile) {
  type Self = BasicQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new ConcreteBasicQueryBuilder(query, nc, Some(this), profile)
}

abstract class BasicQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], _profile: BasicProfile) {
  import _profile.sqlUtils._

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
  protected var selectSlot: SQLBuilder = _

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

  final def buildSelect: SQLBuilder.Result = {
    val b = new SQLBuilder
    buildSelect(b)
    b.build
  }

  def buildSelect(b: SQLBuilder): Unit = {
    innerBuildSelect(b, false)
    insertAllFromClauses()
  }

  protected def rewriteCountStarQuery(q: Query[_]) =
    q.modifiers.isEmpty && (Node(q.value) match {
      case AbstractTable.Alias(_: AbstractTable[_]) => true
      case _: AbstractTable[_] => true
      case _ => false
    })

  protected def innerBuildSelect(b: SQLBuilder, rename: Boolean) {
    Node(query.value) match {
      case ColumnOps.CountAll(Subquery(q: Query[_], false)) if rewriteCountStarQuery(q) =>
        val newQ = q.map(p => ColumnOps.CountAll(Node(p)))
        subQueryBuilderFor(newQ).innerBuildSelect(b, rename)
      case _ => innerBuildSelectNoRewrite(b, rename)
    }
  }

  protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    selectSlot = b.createSlot
    selectSlot += "SELECT "
    expr(Node(query.value), selectSlot, rename, true)
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
      expr(x.by, b, false, true)
      for(x <- xs) {
        b += ","
        expr(x.by, b, false, true)
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
    expr(o.by, b, false, true)
    if(o.isInstanceOf[Ordering.Desc]) b += " desc"
    o.nullOrdering match {
      case Ordering.NullsFirst => b += " nulls first"
      case Ordering.NullsLast => b += " nulls last"
      case Ordering.NullsDefault =>
    }
  }

  def buildDelete = {
    val b = new SQLBuilder += "DELETE FROM "
    val (delTable, delTableName) = Node(query.value) match {
      case t @ AbstractTable.Alias(base:AbstractTable[_]) => (t, base.tableName)
      case t:AbstractTable[_] => (t, t.tableName)
      case n => throw new SQueryException("Cannot create a DELETE statement from an \""+n+
        "\" expression; An aliased or base table is required")
    }
    b += quoteIdentifier(delTableName)
    nc = nc.overrideName(delTable, delTableName) // Alias table to itself because DELETE does not support aliases
    appendConditions(b)
    if(localTables.size > 1)
      throw new SQueryException("Conditions of a DELETE statement must not reference other tables")
    if(query.condHaving ne Nil)
      throw new SQueryException("DELETE statement must contain a HAVING clause")
    for(qb <- subQueryBuilders.valuesIterator)
      qb.insertAllFromClauses()
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
        case nc @ NamedColumn(t @ AbstractTable(tn), n, _) => (tn, n, nc.typeMapper, t)
        case nc @ NamedColumn(t @ AbstractTable.Alias(AbstractTable(tn)), n, _) => (tn, n, nc.typeMapper, t)
        case n => throw new SQueryException("Cannot create an UPDATE statement from a \""+n+
          "\" expression; A single named column or a projection of named columns from the same aliased or base table is required")
      }) match { case (tn, n, tm, t) =>
        if(tableName eq null) { tableName = tn; table = t; }
        else if(tableName != tn)
          throw new SQueryException("All columns for an UPDATE statement must be from the same table")
        b += quoteIdentifier(n) += '=' +?= { (p, param) =>
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
    tableNameSlot += quoteIdentifier(tableName)
    appendConditions(b)
    if(localTables.size > 1)
      throw new SQueryException("An UPDATE statement must not use more than one table at the top level")
    b.build
  }

  protected def expr(c: Node, b: SQLBuilder): Unit = expr(c, b, false, false)

  protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean): Unit = {
    var pos = 0
    c match {
      case p: Projection[_] => {
        p.nodeChildren.foreach { c =>
          if(pos != 0) b += ','
          pos += 1
          expr(c, b, false, true)
          if(rename) b += " as " += quoteIdentifier("c" + pos.toString)
        }
      }
      case _ => innerExpr(c, b)
    }
    if(rename && pos == 0) b += " as " += quoteIdentifier("c1")
  }

  protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ConstColumn(null) => b += "null"
    case ColumnOps.Not(ColumnOps.Is(l, ConstColumn(null))) => { b += '('; expr(l, b); b += " is not null)" }
    case ColumnOps.Not(e) => { b += "(not "; expr(e, b); b+= ')' }
    case ColumnOps.InSet(e, seq, tm, bind) => if(seq.isEmpty) b += "false" else { //TODO Use value provided by profile instead of "false"
      b += '('; expr(e, b); b += " in ("
      if(bind)
        for(x <- b.sep(seq, ","))
          b +?= { (p, param) => tm(profile).setValue(x, p) }
      else b += seq.map(tm(profile).valueToSQLLiteral).mkString(",")
      b += "))"
    }
    case ColumnOps.Is(l, ConstColumn(null)) => { b += '('; expr(l, b); b += " is null)" }
    case ColumnOps.Is(l, r) => { b += '('; expr(l, b); b += '='; expr(r, b); b += ')' }
    case s: SimpleFunction =>
      b += s.name += '('
      for(ch <- b.sep(s.nodeChildren, ",")) expr(ch, b)
      b += ')'
    case s: SimpleScalarFunction =>
      b += "{fn " += s.name += '('
      for(ch <- b.sep(s.nodeChildren, ",")) expr(ch, b)
      b += ")}"
    case SimpleLiteral(w) => b += w
    case ColumnOps.Between(left, start, end) => { expr(left, b); b += " between "; expr(start, b); b += " and "; expr(end, b) }
    case ColumnOps.CountAll(q) => b += "count(*)"; localTableName(q)
    case ColumnOps.CountDistinct(e) => { b += "count(distinct "; expr(e, b); b += ')' }
    case ColumnOps.Like(l, r, esc) => {
      b += '('; expr(l, b); b += " like "; expr(r, b);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') throw new SQueryException("Illegal escape character '"+ch+"' for LIKE expression")
        b += " {escape '" += ch += "'}"
      }
      b += ')'
    }
    case a @ ColumnOps.AsColumnOf(ch, name) => {
      b += "{fn convert("; expr(ch, b); b += ','
      b += name.getOrElse(mapTypeName(a.typeMapper(profile)))
      b += ")}"
    }
    case s: SimpleBinaryOperator => { b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')' }
    case query:Query[_] => { b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")" }
    //case Union.UnionPart(_) => "*"
    case c @ ConstColumn(v) => b += c.typeMapper(profile).valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(profile).setValue(v, p) }
    case pc @ ParameterColumn(idx) => b +?= { (p, param) =>
      val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
      pc.typeMapper(profile).setValue(v, p)
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
    case n: NamedColumn[_] => { b += quoteIdentifier(localTableName(n.table)) += '.' += quoteIdentifier(n.name) }
    case SubqueryColumn(pos, sq, _) => { b += quoteIdentifier(localTableName(sq)) += "." += quoteIdentifier("c" + pos.toString) }
    case a @ AbstractTable.Alias(t: WithOp) => expr(t.mapOp(_ => a), b)
    case t: AbstractTable[_] => expr(Node(t.*), b)
    case t: TableBase[_] => b += quoteIdentifier(localTableName(t)) += ".*"
    case fk: ForeignKey[_] => b += "(("; expr(fk.left, b); b += ")=("; expr(fk.right, b); b += "))"
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

  protected def insertAllFromClauses() {
    if(fromSlot ne null) insertFromClauses()
    for(qb <- subQueryBuilders.valuesIterator)
      qb.insertAllFromClauses()
  }

  protected def insertFromClauses() {
    var first = true
    for((name, t) <- new HashMap ++= localTables) {
      if(!parent.map(_.isDeclaredTable(name)).getOrElse(false)) {
        if(first) { fromSlot += " FROM "; first = false }
        else fromSlot += ','
        table(t, name, fromSlot)
        declaredTables += name
      }
    }
  }

  protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case AbstractTable.Alias(base: AbstractTable[_]) =>
      b += quoteIdentifier(base.tableName) += ' ' += quoteIdentifier(name)
    case base: AbstractTable[_] =>
      b += quoteIdentifier(base.tableName) += ' ' += quoteIdentifier(name)
    case Subquery(sq: Query[_], rename) =>
      b += "("; subQueryBuilderFor(sq).innerBuildSelect(b, rename); b += ") " += quoteIdentifier(name)
    case Subquery(Union(all, sqs), rename) => {
      b += "("
      var first = true
      for(sq <- sqs) {
        if(!first) b += (if(all) " UNION ALL " else " UNION ")
        subQueryBuilderFor(sq).innerBuildSelect(b, first && rename)
        first = false
      }
      b += ") " += quoteIdentifier(name)
    }
    case j: Join[_,_] =>
      /* There is no way to write all nested joins (if they are supported at all) properly in a
       * database-independent way because the {oj...} escape syntax does not support inner joins.
       * We let the first join determine the syntax and hope for the best. */
      if(j.joinType == Join.Inner) createJoin(j, b)
      else { b += "{oj "; createJoin(j, b); b += "}" }
  }

  protected def createJoin(j: Join[_,_], b: SQLBuilder): Unit = {
    val l = j.leftNode
    val r = j.rightNode
    table(l, nc.nameFor(l), b)
    b += " " += j.joinType.sqlName += " join "
    r match {
      case rj: Join[_,_] => createJoin(rj, b)
      case _ => table(r, nc.nameFor(r), b)
    }
    b += " on "
    expr(j.on, b)
  }

  protected def untupleColumn(columns: Node) = {
    val l = new scala.collection.mutable.ListBuffer[Node]
    def f(c: Any): Unit = c match {
      case p:Projection[_] =>
        for(i <- 0 until p.productArity)
          f(Node(p.productElement(i)))
      case t:AbstractTable[_] => f(Node(t.*))
      case n: Node => l += n
      case v => throw new SQueryException("Cannot untuple non-Node value "+v)
    }
    f(Node(columns))
    l.toList
  }
}
