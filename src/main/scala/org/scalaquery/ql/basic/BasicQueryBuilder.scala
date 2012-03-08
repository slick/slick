package org.scalaquery.ql.basic

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._
import org.scalaquery.ql.ColumnOps._
import org.scalaquery.ast._

class BasicQueryBuilder(_query: Query[_, _], _profile: BasicProfile) {
  import _profile.sqlUtils._

  protected val profile = _profile
  protected val query: Query[_, _] = _query
  protected val ast = profile.processAST(query)

  protected val mayLimit0 = true
  protected val scalarFrom: Option[String] = None
  protected val supportsTuples = true
  protected val supportsCast = true
  protected val concatOperator: Option[String] = None

  final def buildSelect: (SQLBuilder.Result, ValueLinearizer[_]) = {
    val b = new SQLBuilder
    buildSelect(b)
    (b.build, query.linearizer)
  }

  final def buildSelect(b: SQLBuilder) = buildComprehension(ast, b)

  protected def buildComprehension(n: Node, b: SQLBuilder): Unit = n match {
    case Comprehension(from, where, select) =>
      b += "SELECT "
      select match {
        case Some(n) => buildSelectClause(n, b)
        case None =>
          if(from.length <= 1) b += "*"
          else b += symbolName(from.last._1) += ".*"
      }
      if(from.isEmpty) scalarFrom.foreach { s => b += " FROM " += s }
      else {
        b += " FROM "
        b.sep(from, ", ") { case (sym, n) =>
          buildSubquery(n, b)
          b += ' ' += symbolName(sym)
        }
      }
      if(!where.isEmpty) {
        b += " WHERE "
        expr(where.reduceLeft(And), b)
      }
    case TakeDrop(from, take, drop) => buildTakeDrop(from, take, drop, b)
  }

  protected def buildTakeDrop(from: Node, take: Option[Int], drop: Option[Int], b: SQLBuilder) {
    b += "SELECT * FROM "
    buildSubquery(from, b)
    if(take == Some(0)) b += " WHERE 1=0"
    else appendTakeDropClause(take, drop, b)
  }

  protected def appendTakeDropClause(take: Option[Int], drop: Option[Int], b: SQLBuilder) = (take, drop) match {
    /* SQL:2008 syntax */
    case (Some(t), Some(d)) => b += " OFFSET " += d += " ROW FETCH NEXT " += t += " ROW ONLY"
    case (Some(t), None) => b += " FETCH NEXT " += t += " ROW ONLY"
    case (None, Some(d)) => b += " OFFSET " += d += " ROW"
    case _ =>
  }

  protected def buildSelectClause(n: Node, b: SQLBuilder): Unit = n match {
    case Pure(StructNode(ch)) =>
      b.sep(ch, ", ") { case (sym, n) =>
        expr(n, b)
        b += " as " += symbolName(sym)
      }
    case Pure(ProductNode(ch @ _*)) =>
      b.sep(ch, ", ")(expr(_, b))
    case Pure(n) => expr(n, b)
  }

  protected def buildSubquery(n: Node, b: SQLBuilder): Unit = n match {
    case AbstractTable(name) => b += quoteIdentifier(name)
    case n =>
      b += '('
      buildComprehension(n, b)
      b += ')'
  }

  protected def symbolName(s: Symbol): String = s match {
    case AnonSymbol(name) => name
    case s => quoteIdentifier(s.name)
  }

  protected def expr(n: Node, b: SQLBuilder): Unit = n match {
    case ConstColumn(null) => b += "null"
    case Not(Is(l, ConstColumn(null))) => b += '('; expr(l, b); b += " is not null)"
    case Not(e) => b += "(not "; expr(e, b); b+= ')'
    case i @ InSet(e, seq, bind) => if(seq.isEmpty) expr(ConstColumn.FALSE, b) else {
      b += '('; expr(e, b); b += " in ("
      if(bind) b.sep(seq, ",")(x => b +?= { (p, param) => i.tm(profile).setValue(x, p) })
      else b += seq.map(i.tm(profile).valueToSQLLiteral).mkString(",")
      b += "))"
    }
    case Is(l, ConstColumn(null)) => b += '('; expr(l, b); b += " is null)"
    case Is(l, r) => b += '('; expr(l, b); b += '='; expr(r, b); b += ')'
    case EscFunction("concat", l, r) if concatOperator.isDefined =>
      b += '('; expr(l, b); b += concatOperator.get; expr(r, b); b += ')'
    case s: SimpleFunction =>
      if(s.scalar) b += "{fn "
      b += s.name += '('
      b.sep(s.nodeChildren, ",")(expr(_, b))
      b += ')'
      if(s.scalar) b += '}'
    case SimpleLiteral(w) => b += w
    case s: SimpleExpression => s.toSQL(b, this)
    case Between(left, start, end) => expr(left, b); b += " between "; expr(start, b); b += " and "; expr(end, b)
    case CountDistinct(e) => b += "count(distinct "; expr(e, b); b += ')'
    case Like(l, r, esc) =>
      b += '('; expr(l, b); b += " like "; expr(r, b);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') throw new SQueryException("Illegal escape character '"+ch+"' for LIKE expression")
        // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
        b += " escape '" += ch += "'"
      }
      b += ')'
    case a @ AsColumnOf(ch, name) =>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(profile)))
      if(supportsCast) {
        b += "cast("
        expr(ch, b)
        b += " as " += tn += ")"
      } else {
        b += "{fn convert("
        expr(ch, b)
        b += ',' += tn += ")}"
      }
    case s: SimpleBinaryOperator => b += '('; expr(s.left, b); b += ' ' += s.name += ' '; expr(s.right, b); b += ')'
    case c @ ConstColumn(v) => b += c.typeMapper(profile).valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(profile).setValue(v, p) }
    case pc @ ParameterColumn(idx) => b +?= { (p, param) =>
      val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
      pc.typeMapper(profile).setValue(v, p)
    }
    case c: Case.CaseNode =>
      b += "(case"
      c.clauses.foldRight(()) { (w,_) =>
        b += " when "
        expr(w.asInstanceOf[Case.WhenNode].left, b)
        b += " then "
        expr(w.asInstanceOf[Case.WhenNode].right, b)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n =>
          b += " else "
          expr(n, b)
      }
      b += " end)"
    case FieldRef(struct, field) => b += symbolName(struct) += '.' += symbolName(field)
    case fk: ForeignKey[_, _] =>
      if(supportsTuples) {
        b += "(("; expr(fk.left, b); b += ")=("; expr(fk.right, b); b += "))"
      } else {
        val cols = fk.linearizedSourceColumns zip fk.linearizedTargetColumns
        b += "("
        b.sep(cols, " and "){ case (l,r) => expr(l, b); b += "="; expr(r, b) }
        b += ")"
      }
    //TODO case CountAll(q) => b += "count(*)"; localTableName(q)
    //TODO case query:Query[_, _] => b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")"
    //TODO case sq @ Subquery(_, _) => b += quoteIdentifier(localTableName(sq)) += ".*"
    case _ => throw new SQueryException("Don't know what to do with node "+n+" in an expression")
  }








  protected def rewriteCountStarQuery(q: Query[_, _]) =
    q.modifiers.isEmpty && (q.reified match {
      case AbstractTable.Alias(_: AbstractTable[_]) => true
      case _: AbstractTable[_] => true
      case _ => false
    })

  final protected def innerBuildSelect(b: SQLBuilder, rename: Boolean): Unit = sys.error("obsolete")

  protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean): Unit = sys.error("obsolete")

  protected def appendClauses(b: SQLBuilder): Unit = sys.error("obsolete")

  final protected def appendGroupClause(b: SQLBuilder): Unit = query.typedModifiers[Grouping] match {
    case Nil =>
    case xs => b += " GROUP BY "; b.sep(xs, ",")(x => expr(x.by, b, false, true))
  }

  final protected def appendOrderClause(b: SQLBuilder): Unit = query.typedModifiers[Ordering] match {
    case Nil =>
    case xs => b += " ORDER BY "; b.sep(xs, ",")(appendOrdering(_,b))
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
    val (delTable, delTableName) = query.reified match {
      case t @ AbstractTable.Alias(base:AbstractTable[_]) => (t, base.tableName)
      case t:AbstractTable[_] => (t, t.tableName)
      case n => throw new SQueryException("Cannot create a DELETE statement from an \""+n+
        "\" expression; An aliased or base table is required")
    }
    b += quoteIdentifier(delTableName)
    //TODO nc = nc.overrideName(delTable, delTableName) // Alias table to itself because DELETE does not support aliases
    appendConditions(b)
    //TODO if(localTables.size > 1)
    //  throw new SQueryException("Conditions of a DELETE statement must not reference other tables")
    //for(qb <- subQueryBuilders.valuesIterator)
    //  qb.insertAllFromClauses()
    b.build
  }

  def buildUpdate = {
    if(/*!query.condHaving.isEmpty ||*/ !query.modifiers.isEmpty)
      throw new SQueryException("A query for an UPDATE statement must not have any modifiers other than WHERE restrictions")
    val b = new SQLBuilder += "UPDATE "
    val tableNameSlot = b.createSlot
    b += " SET "
    var tableName: String = null
    var table: Node = null

    def handleColumn(node: Node) {
      (node match {
        case nc @ NamedColumn(t @ AbstractTable(tn), n, _) => (tn, n, nc.typeMapper, t)
        case nc @ NamedColumn(t @ AbstractTable.Alias(AbstractTable(tn)), n, _) => (tn, n, nc.typeMapper, t)
        case n => throw new SQueryException("Cannot create an UPDATE statement from a \""+n+
          "\" expression; A single named column or a projection of named columns from the same aliased or base table is required")
      }) match { case (tn, n, tm, t) =>
        if(tableName eq null) { tableName = tn; table = t; }
        else if(tableName != tn)
          throw new SQueryException("All columns for an UPDATE statement must be from the same table")
        b += quoteIdentifier(n) += "=?"
      }
    }

    def handleColumns(node: Node) {
      node match {
        case p: Projection[_] =>
          var i = 0
          for(ch <- p.nodeChildren) {
            if(i > 0) b += ','
            handleColumn(ch)
            i += 1
          }
        case t @ AbstractTable(tn) =>
          //TODO nc = nc.overrideName(t, tn)
          handleColumns(Node(t.*))
        case a @ AbstractTable.Alias(t @ AbstractTable(tn)) =>
          //TODO nc = nc.overrideName(a, tn)
          handleColumns(Node(t.*))
        case n => handleColumn(n)
      }
    }

    handleColumns(query.reified)
    //TODO nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
    tableNameSlot += quoteIdentifier(tableName)
    appendConditions(b)
    //TODO if(localTables.size > 1)
    //  throw new SQueryException("An UPDATE statement must not use more than one table at the top level")
    b.build
  }

  protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean) {
    sys.error("obsolete")
    /*var pos = 0
    c match {
      case p: ProductNode => {
        p.nodeChildren.foreach { c =>
          if(pos != 0) b += ','
          pos += 1
          expr(c, b, false, true)
          if(rename) b += " as " += quoteIdentifier("c" + pos.toString)
        }
      }
      case _ => innerExpr(c, b)
    }
    if(rename && pos == 0) {
      b += " as " += quoteIdentifier("c1")
      pos = 1
    }
    if(topLevel) this.maxColumnPos = pos*/
  }

  protected def innerExpr(c: Node, b: SQLBuilder): Unit = sys.error("obsolete")

  final protected def appendConditions(b: SQLBuilder): Unit = sys.error("obsolete")

  final protected def insertAllFromClauses(): Unit = sys.error("obsolete")

  final protected def insertFromClauses(): Unit = sys.error("obsolete")

  final protected def table(t: Node, name: String, b: SQLBuilder) = sys.error("obsolete")

  final protected def createJoin(j: Join[_,_], b: SQLBuilder) = sys.error("obsolete")
}
