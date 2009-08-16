package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Query, NamingContext, Node, SQLBuilder}
import com.novocode.squery.combinator.basic.{BasicQueryBuilder, ConcreteBasicQueryBuilder, BasicTypeMapperDelegates}

object MySQLDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[MySQLDriver.type]
  type TypeMapperDelegatesT = MySQLTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[MySQLDriver.type] {
    implicit val squeryDriver = self
  }

  val typeMapperDelegates = new MySQLTypeMapperDelegates {}

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new MySQLQueryBuilder(query, nc, None, this)
}

trait MySQLTypeMapperDelegates extends BasicTypeMapperDelegates {
  override val stringTypeMapperDelegate = new BasicTypeMapperDelegates.StringTypeMapperDelegate {
    override def valueToSQLLiteral(value: String) = if(value eq null) "NULL" else {
      val sb = new StringBuilder
      sb append '\''
      for(c <- value) c match {
        case '\'' => sb append "\\'"
        case '"' => sb append "\\\""
        case 0 => sb append "\\0"
        case 26 => sb append "\\Z"
        case '\b' => sb append "\\b"
        case '\n' => sb append "\\n"
        case '\r' => sb append "\\r"
        case '\t' => sb append "\\t"
        case '\\' => sb append "\\\\"
        case _ => sb append c
      }
      sb append '\''
      sb.toString
    }
  }
}

class MySQLQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: MySQLDriver.type)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  override type Self = MySQLQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new MySQLQueryBuilder(query, nc, Some(this), profile)

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    case ExtendedOperator.Concat(l, r) => b += "concat("; expr(l, b); b += ','; expr(r, b); b += ')'
    case _ => super.innerExpr(c, b)
  }

  override protected def insertFromClauses() {
    super.insertFromClauses()
    if(fromSlot.isEmpty) fromSlot += " FROM DUAL"
  }
}
