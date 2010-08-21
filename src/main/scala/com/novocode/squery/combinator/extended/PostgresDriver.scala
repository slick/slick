package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.basic._

object PostgresDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[PostgresDriver.type]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[PostgresDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new BasicTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new PostgresQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new PostgresDDLBuilder(table).buildDDL
}

class PostgresQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: PostgresDriver.type)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._

  override type Self = PostgresQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new PostgresQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case a @ ColumnOps.AsColumnOf(ch, name) => {
      b += "cast("; expr(ch, b); b += " as "
      b += name.getOrElse(a.typeMapper(profile).sqlTypeName)
      b += ")"
    }
    case ColumnOps.Concat(l, r) => b += '('; expr(l, b); b += "||"; expr(r, b); b += ')'
    case _ => super.innerExpr(c, b)
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    super.appendClauses(b)
    appendLimitClause(b)
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    case TakeDrop(Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
    case TakeDrop(Some(t), None) => b += " LIMIT " += t
    case TakeDrop(None, Some(d)) => b += " OFFSET " += d
    case _ =>
  }
}

class PostgresDDLBuilder(table: AbstractBasicTable[_]) extends BasicDDLBuilder(table, PostgresDriver) {
  override def addTypeAndOptions(c: NamedColumn[_], sb: StringBuilder) {
    var sqlType: String = null
    var notNull = false
    var autoIncrement = false
    var primaryKey = false
    var defaultLiteral: String = null
    for(o <- c.options) o match {
      case BasicColumnOption.DBType(s) => sqlType = s
      case BasicColumnOption.NotNull => notNull = true
      case ExtendedColumnOption.AutoInc => autoIncrement = true
      case BasicColumnOption.PrimaryKey => primaryKey = true
      case BasicColumnOption.Default(v) => defaultLiteral = c.asInstanceOf[NamedColumn[Any]].typeMapper(PostgresDriver).valueToSQLLiteral(v)
    }
    if(sqlType eq null) sqlType = mapTypeName(c.typeMapper(PostgresDriver))
    if(autoIncrement) sb append "SERIAL"
    else sb append sqlType
    if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
    if(notNull) sb append " NOT NULL"
    if(primaryKey) sb append " PRIMARY KEY"
  }
}
