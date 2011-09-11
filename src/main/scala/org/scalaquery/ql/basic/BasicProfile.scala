package org.scalaquery.ql.basic

import org.scalaquery.ql.{ColumnBase, Sequence, Query, Projection, DDL}
import org.scalaquery.util.{ValueLinearizer, NamingContext, SQLBuilder}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_ <: BasicProfile]
  type TypeMapperDelegatesT <: BasicTypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[_, R]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_, _], nc: NamingContext): BasicQueryBuilder = new ConcreteBasicQueryBuilder(query, nc, None, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT
  val sqlUtils = new BasicSQLUtils

  def buildSelectStatement(query: Query[_, _], nc: NamingContext): (SQLBuilder.Result, ValueLinearizer[_]) =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[_, _], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[_, _], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildInsertStatement(cb: Any): String = new BasicInsertBuilder(cb, this).buildInsert
  def buildInsertStatement(cb: Any, q: Query[_, _]): SQLBuilder.Result =
    new BasicInsertBuilder(cb, this).buildInsert(q)

  def buildTableDDL(table: AbstractBasicTable[_]): DDL = new BasicDDLBuilder(table, this).buildDDL
  def buildSequenceDDL(seq: Sequence[_]): DDL = new BasicSequenceDDLBuilder(seq, this).buildDDL
}
