package org.scalaquery.ql.basic

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.util._
import org.scalaquery.ql.ColumnOps._
import org.scalaquery.ast._

class BasicQueryBuilder(query: Query[_, _], driver: BasicProfile) {
  import driver.sqlUtils._

  protected final val ast = driver.processAST(query)
  protected final val b = new SQLBuilder

  protected val mayLimit0 = true
  protected val scalarFrom: Option[String] = None
  protected val supportsTuples = true
  protected val supportsCast = true
  protected val concatOperator: Option[String] = None

  def sqlBuilder = b

  final def buildSelect(): (SQLBuilder.Result, ValueLinearizer[_]) = {
    buildComprehension(ast)
    (b.build, query.linearizer)
  }

  protected def buildComprehension(n: Node): Unit = n match {
    case Comprehension(from, where, orderBy, select) =>
      b += "select "
      select match {
        case Some(n) => buildSelectClause(n)
        case None =>
          if(from.length <= 1) b += "*"
          else b += symbolName(from.last._1) += ".*"
      }
      if(from.isEmpty) buildScalarFrom
      else {
        b += " from "
        b.sep(from, ", ") { case (sym, n) =>
          buildFrom(n, Some(sym))
        }
      }
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And))
      }
      if(!orderBy.isEmpty) appendOrderClause(orderBy)
    case Pure(CountAll(q)) =>
      b += "select count(*) from "
      buildFrom(q, None)
    case p @ Pure(_) =>
      b += "select "
      buildSelectClause(p)
      buildScalarFrom
    case AbstractTable(name) =>
      b += "select * from " += quoteIdentifier(name)
    case TakeDrop(from, take, drop) => buildTakeDrop(from, take, drop)
    case Union(left, right, all, _, _) =>
      b += "select * from "
      buildFrom(left, None)
      b += (if(all) " union all " else " union ")
      buildFrom(right, None)
    case n => throw new SQueryException("Unexpected node "+n+" -- SQL prefix: "+b.build.sql)
  }

  protected def buildScalarFrom: Unit = scalarFrom.foreach { s => b += " from " += s }

  protected def buildTakeDrop(from: Node, take: Option[Int], drop: Option[Int]) {
    if(take == Some(0)) {
      b += "select * from "
      buildFrom(from, None)
      b += " where 1=0"
    } else {
      buildComprehension(from)
      appendTakeDropClause(take, drop)
    }
  }

  protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = (take, drop) match {
    /* SQL:2008 syntax */
    case (Some(t), Some(d)) => b += " offset " += d += " row fetch next " += t += " row only"
    case (Some(t), None) => b += " fetch next " += t += " row only"
    case (None, Some(d)) => b += " offset " += d += " row"
    case _ =>
  }

  protected def buildSelectClause(n: Node): Unit = n match {
    case Pure(StructNode(ch)) =>
      b.sep(ch, ", ") { case (sym, n) =>
        expr(n)
        b += " as " += symbolName(sym)
      }
    case Pure(ProductNode(ch @ _*)) =>
      b.sep(ch, ", ")(expr)
    case Pure(n) => expr(n)
  }

  protected def buildFrom(n: Node, alias: Option[Symbol]){
    def addAlias = alias foreach { s => b += ' ' += symbolName(s) }
    n match {
      case AbstractTable(name) =>
        b += quoteIdentifier(name)
        addAlias
      case BaseJoin(leftGen, rightGen, left, right, jt) =>
        buildFrom(left, Some(leftGen))
        b += ' ' += jt.sqlName += " join "
        buildFrom(right, Some(rightGen))
      case FilteredJoin(leftGen, rightGen, left, right, jt, on) =>
        buildFrom(left, Some(leftGen))
        b += ' ' += jt.sqlName += " join "
        buildFrom(right, Some(rightGen))
        b += " on "
        expr(on)
      case n =>
        b += '('
        buildComprehension(n)
        b += ')'
        addAlias
    }
  }

  protected def symbolName(s: Symbol): String = s match {
    case AnonSymbol(name) => name
    case s => quoteIdentifier(s.name)
  }

  def expr(n: Node): Unit = n match {
    case ConstColumn(null) => b += "null"
    case Not(Is(l, ConstColumn(null))) => b += '('; expr(l); b += " is not null)"
    case Not(e) => b += "(not "; expr(e); b+= ')'
    case i @ InSet(e, seq, bind) => if(seq.isEmpty) expr(ConstColumn.FALSE) else {
      b += '('; expr(e); b += " in ("
      if(bind) b.sep(seq, ",")(x => b +?= { (p, param) => i.tm(driver).setValue(x, p) })
      else b += seq.map(i.tm(driver).valueToSQLLiteral).mkString(",")
      b += "))"
    }
    case Is(l, ConstColumn(null)) => b += '('; expr(l); b += " is null)"
    case Is(left: ProductNode, right: ProductNode) =>
      if(supportsTuples) {
        b += "("
        expr(left)
        b += " = "
        expr(right)
        b += ")"
      } else {
        val cols = left.nodeChildren zip right.nodeChildren
        b += "("
        b.sep(cols, " and "){ case (l,r) => expr(l); b += "="; expr(r) }
        b += ")"
      }
    case ProductNode(ch @ _*) =>
      b += "("
      b.sep(ch, ", ")(expr)
      b += ")"
    case Is(l, r) => b += '('; expr(l); b += '='; expr(r); b += ')'
    case EscFunction("concat", l, r) if concatOperator.isDefined =>
      b += '('; expr(l); b += concatOperator.get; expr(r); b += ')'
    case s: SimpleFunction =>
      if(s.scalar) b += "{fn "
      b += s.name += '('
      b.sep(s.nodeChildren, ",")(expr)
      b += ')'
      if(s.scalar) b += '}'
    case SimpleLiteral(w) => b += w
    case s: SimpleExpression => s.toSQL(this)
    case Between(left, start, end) => expr(left); b += " between "; expr(start); b += " and "; expr(end)
    case CountDistinct(e) => b += "count(distinct "; expr(e); b += ')'
    case Like(l, r, esc) =>
      b += '('; expr(l); b += " like "; expr(r);
      esc.foreach { ch =>
        if(ch == '\'' || ch == '%' || ch == '_') throw new SQueryException("Illegal escape character '"+ch+"' for LIKE expression")
        // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
        b += " escape '" += ch += "'"
      }
      b += ')'
    case a @ AsColumnOf(ch, name) =>
      val tn = name.getOrElse(mapTypeName(a.typeMapper(driver)))
      if(supportsCast) {
        b += "cast("
        expr(ch)
        b += " as " += tn += ")"
      } else {
        b += "{fn convert("
        expr(ch)
        b += ',' += tn += ")}"
      }
    case s: SimpleBinaryOperator => b += '('; expr(s.left); b += ' ' += s.name += ' '; expr(s.right); b += ')'
    case c @ ConstColumn(v) => b += c.typeMapper(driver).valueToSQLLiteral(v)
    case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(driver).setValue(v, p) }
    case pc @ ParameterColumn(idx) => b +?= { (p, param) =>
      val v = if(idx == -1) param else param.asInstanceOf[Product].productElement(idx)
      pc.typeMapper(driver).setValue(v, p)
    }
    case c: Case.CaseNode =>
      b += "(case"
      c.clauses.foldRight(()) { (w,_) =>
        b += " when "
        expr(w.asInstanceOf[Case.WhenNode].left)
        b += " then "
        expr(w.asInstanceOf[Case.WhenNode].right)
      }
      c.elseClause match {
        case ConstColumn(null) =>
        case n =>
          b += " else "
          expr(n)
      }
      b += " end)"
    case FieldRef(struct, field) => b += symbolName(struct) += '.' += symbolName(field)
    //TODO case CountAll(q) => b += "count(*)"; localTableName(q)
    //TODO case query:Query[_, _] => b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")"
    //TODO case sq @ Subquery(_, _) => b += quoteIdentifier(localTableName(sq)) += ".*"
    case n => // try to build a sub-query
      b += '('
      buildComprehension(n)
      b += ')'
    //case _ => throw new SQueryException("Don't know what to do with node "+n+" in an expression")
  }

  protected def appendOrderClause(order: Seq[(Node, Ordering)]) {
    b += " order by "
    b.sep(order, ", "){ case (n, o) => appendOrdering(n, o) }
  }

  protected def appendOrdering(n: Node, o: Ordering) {
    expr(n)
    if(o.direction.desc) b += " desc"
    if(o.nulls.first) b += " nulls first"
    else if(o.nulls.last) b += " nulls last"
  }

  def buildUpdate: SQLBuilder.Result = {
    val (gen, from, where, select) = ast match {
      case Comprehension(Seq((sym, from: AbstractTable[_])), where, _, Some(Pure(select))) => select match {
        case f @ FieldRef(struct, _) if struct == sym => (sym, from, where, Seq(f.field))
        case ProductNode(ch @ _*) if ch.forall{ case FieldRef(struct, _) if struct == sym => true; case _ => false} =>
          (sym, from, where, ch.map{ case FieldRef(_, field) => field })
        case _ => throw new SQueryException("A query for an UPDATE statement must select table columns only -- Unsupported shape: "+select)
      }
      case _ => throw new SQueryException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
    }

    b += "update " += quoteIdentifier(from.tableName) += ' ' += symbolName(gen) += " set "
    b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
    if(!where.isEmpty) {
      b += " where "
      expr(where.reduceLeft(And))
    }
    //TODO nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
    b.build
  }

  def buildDelete: SQLBuilder.Result = {
    val (gen, from, where) = ast match {
      case Comprehension(Seq((sym, from: AbstractTable[_])), where, _, Some(Pure(select))) => (sym, from, where)
      case _ => throw new SQueryException("A query for a DELETE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
    }

    b += "delete from " += quoteIdentifier(from.tableName) += ' ' += symbolName(gen)
    if(!where.isEmpty) {
      b += " where "
      expr(where.reduceLeft(And))
    }
    //TODO nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
    b.build
  }





  protected def rewriteCountStarQuery(q: Query[_, _]) =
    /*q.modifiers.isEmpty &&*/ (q.packed match {
      case _: AbstractTable[_] => true
      case _ => false
    })

  protected def innerBuildSelectNoRewrite(rename: Boolean): Unit = sys.error("obsolete")

  protected def appendClauses(): Unit = sys.error("obsolete")

  /*TODO
  final protected def appendGroupClause(): Unit = query.typedModifiers[Grouping] match {
    case Nil =>
    case xs => b += " group by "; b.sep(xs, ",")(x => expr(x.by))
  }
  */

  protected def innerExpr(c: Node): Unit = sys.error("obsolete")

  final protected def appendConditions(): Unit = sys.error("obsolete")
}
