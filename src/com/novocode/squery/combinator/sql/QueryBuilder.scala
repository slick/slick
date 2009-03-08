package com.novocode.squery.combinator.sql

import scala.collection.mutable.HashMap
import java.io.PrintWriter
import com.novocode.squery.combinator._
import com.novocode.squery.RefId

class QueryBuilder private[QueryBuilder] (val query: Query[Column[_]], parent: Option[QueryBuilder]) {

  def this(query: Query[Column[_]]) = this(query, None)

  //TODO Pull tables out of subqueries where needed

  private val tnames = new HashMap[RefId[TableBase[_]], String]
  private[this] var nextTid = 1

  private[this] def nameFor(t: TableBase[_]) = parent.flatMap(_.tnames.get(RefId(t))) orElse tnames.get(RefId(t)) match {
    case Some(n) => n
    case None =>
      val n = "t"+getNextTid
      tnames.put(RefId(t), n)
      n
  }

  private def getNextTid: Int = parent match {
    case Some(s) => s.getNextTid
    case _ => {
      val id = nextTid
      nextTid += 1
      id
    }
  }

  private[this] val subQueries = new HashMap[RefId[Query[_]], QueryBuilder]
  
  private[this] def find(c: Column[_]): Unit = c match {
    case null => ()
    case w @ WithOp(op) if(!op.isInstanceOf[ColumnOp.BaseTableQueryOp]) =>
      op match {
        case ColumnOp.SortOp(base, by, desc) => { find(base); find(by) }
        case ColumnOp.SubQueryOp(query) => subQueries.put(RefId[Query[_]](query), new QueryBuilder(query, Some(this)))
        case ColumnOp.UnionOp(union @ Union(_, query1, query2)) =>
          //subQueries.put(RefId[Query[_]](query1), new QueryBuilder(query1, Some(this)))
          //subQueries.put(RefId[Query[_]](query2), new QueryBuilder(query2, Some(this)))
          nameFor(union)
      }
    case p: Projection[_] => p.elements.foreach(find _)
    case c: ConstColumn[_] => ()
    case w: WrappedColumn[_] => find(w.parent)
    case n: NamedColumn[_] =>
      nameFor(n.table)
      n.table match {
        case WithOp(ColumnOp.JoinOp(_, join)) => nameFor(join)
        case _ => ()
      }
    case t: TableBase[_] => nameFor(t)
    case o: Operator => o.children.foreach(find _)
  }

  find(query.value)
  for(w <- query.cond) find(w)

  private[this] def expr(c: Column[_]): String = c match {
    case null => "null"
    case w @ WithOp(op) if(!op.isInstanceOf[ColumnOp.BaseTableQueryOp]) =>
      op match {
        case ColumnOp.SortOp(base, _, _) => expr(base) //TODO Handle nested SortOps
        case ColumnOp.SubQueryOp(query) => "(" + subQueries(RefId[Query[_]](query)).buildSelect + ")"
        case ColumnOp.UnionOp(_) => "*"
      }
    case p: Projection[_] => p.elements.map(expr(_)).reduceLeft(_ + "," + _)
    case ConstColumn(i: Int) => i.toString
    case ConstColumn(b: Boolean) => b.toString
    case ConstColumn(s: String) => '\'' + sqlEncode(s) + '\''
    case w: WrappedColumn[_] => expr(w.parent)
    case n: NamedColumn[_] => { nameFor(n.table) + '.' + (n.name) }
    case t: Table[_] => expr(t.*)
    case t: TableBase[_] => { nameFor(t) + ".*" }
    case Operator.Not(Operator.Is(l, null)) => '(' + expr(l) + " is not null)"
    case Operator.Is(l, null) => '(' + expr(l) + " is null)"
    case Operator.Is(l, r) => '(' + expr(l) + '=' + expr(r) + ')'
    case Operator.In(l, r) => '(' + expr(l) + " in " + expr(r) + ')'
    case Operator.And(l, r) => '(' + expr(l) + " and " + expr(r) + ')'
    case Operator.Or(l, r) => '(' + expr(l) + " or " + expr(r) + ')'
    case Operator.Count(e) => "count(" + expr(e) + ')'
    case Operator.Max(e) => "max(" + expr(e) + ')'
    case Operator.Not(e) => "not " + expr(e)
  }

  private[this] def sortClause(c: Column[_]): String = c match {
    case null => "null"
    case WithOp(op) =>
      op match {
        case ColumnOp.SortOp(_, by, desc) =>
          val s = " ORDER BY " + expr(by)
          if(desc) s + " descending" else s
        case _ => null
      }
    case _ => null
  }

  private[this] def sqlEncode(s: String) = {
    val sb = new StringBuilder(s.length + 4)
    for(c <- s) c match {
      case '\'' => sb append "''"
      case _ => sb append c
    }
    sb.toString
  }

  private[this] def table(t: TableBase[_]): Option[String] = t match {
    case w @ WithOp(ColumnOp.BaseTableQueryOp(base: Table[_])) =>
      Some(base.tableName + " " + nameFor(w.asInstanceOf[TableBase[_]]))
    case w @ WithOp(ColumnOp.JoinOp(base: Table[_], join)) => None
    case join @ Join(t1: Table[_], t2: Table[_]) =>
      Some(t1.tableName + " " + nameFor(t1) + " natural join " + t2.tableName + " " + nameFor(t2))
    case union @ Union(all, q1, q2) =>
      Some(" xxx UNION xxx " + nameFor(union)) //TODO fix this
  }

  def buildSelect: String = buildSelect(query.value)

  def buildSelect(value: Column[_]): String = value match {
    case Operator.Count(e) => "SELECT count(*) from (" + buildSelect(e) + ")"
    case _ =>
      val b = new StringBuilder("SELECT ") append expr(value)
      var first = true
      forLocalTables { t =>
        table(t) match {
          case Some(s) =>
            b append (if(first) " FROM " else ", ") append s
            if(first) first = false
          case _ => ()
        }
      }
      appendConditions(b)
      val s = sortClause(value)
      if(!(s eq null)) b append s
      b.toString
  }

  def buildDelete: String = {
    val b = new StringBuilder("DELETE FROM ")
    var delTable:Table[_] = null
    forLocalTables { t => t match {
      case w @ WithOp(ColumnOp.BaseTableQueryOp(base: Table[_])) =>
        if(delTable eq null) {
          delTable = w.asInstanceOf[Table[_]]
          b append base.tableName
        } else throw new SQueryException("Cannot build DELETE statement for more than one table")
      case WithOp(ColumnOp.JoinOp(base: Table[_], join)) => ()
      case _ => throw new SQueryException("Cannot build DELETE statement for table "+t)
    }}
    var first = true
    tnames.put(RefId(delTable), delTable.tableName) // Alias table to itself because DELETE does not support aliases
    appendConditions(b)
    b.toString
  }

  private[this] def appendConditions(b: StringBuilder) {
    var first = true
    query.cond match {
      case Nil => ()
      case a :: l =>
        b append " WHERE " append expr(a)
        for(e <- l) b append " AND " append expr(e)
    }
  }

  private[this] def forLocalTables(f: TableBase[_] => Unit) =
    for((r @ RefId(t), n) <- tnames)
      if(!parent.flatMap(_.tnames.get(r)).isDefined) f(t)
    
}
