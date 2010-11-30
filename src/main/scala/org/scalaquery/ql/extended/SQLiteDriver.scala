package org.scalaquery.ql.extended

import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._

class SQLiteDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[SQLiteDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[SQLiteDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new SQLiteQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new SQLiteDDLBuilder(table, this).buildDDL
}

object SQLiteDriver extends SQLiteDriver

class SQLiteDDLBuilder(table: AbstractBasicTable[_], profile: SQLiteDriver) extends BasicDDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class SQLiteColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " PRIMARY KEY AUTOINCREMENT"
      else if(notNull) sb append " NOT NULL"
      else if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new SQLiteColumnDDLBuilder(c)

  override def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE TABLE " append table.tableName append " ("
    var first = true
    for(n <- table.create_*) {
      if(first) first = false
      else b append ","
      createColumnDDLBuilder(n).appendColumn(b)
    }
    for(fk <- table.foreignKeys) {
      b append ","
      addForeignKey(fk, b)
    }
    b append ")"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Iterable()
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP TABLE " + table.tableName)
    }
  }
}

class SQLiteQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: SQLiteDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = SQLiteQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new SQLiteQueryBuilder(query, nc, Some(this), profile)

  override protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    case j: Join[_,_] => createJoin(j, b)
    case _ => super.table(t, name, b)
  }

  override protected def appendOrdering(o: Ordering, b: SQLBuilder) {
    val desc = o.isInstanceOf[Ordering.Desc]
    if(o.nullOrdering == Ordering.NullsLast && !desc) {
      b += "("
      expr(o.by, b)
      b += ") is null,"
    } else if(o.nullOrdering == Ordering.NullsFirst && desc) {
      b += "("
      expr(o.by, b)
      b += ") is null desc,"
    }
    expr(o.by, b)
    if(desc) b += " desc"
  }
}
