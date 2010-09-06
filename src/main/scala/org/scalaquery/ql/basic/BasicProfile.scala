package org.scalaquery.ql.basic

import org.scalaquery.ql.{ColumnBase, Sequence, Query, Projection, DDL}
import org.scalaquery.util.{NamingContext, SQLBuilder}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_ <: BasicProfile]
  type TypeMapperDelegatesT <: BasicTypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[ColumnBase[R]]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_], nc: NamingContext): BasicQueryBuilder = new ConcreteBasicQueryBuilder(query, nc, None, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT
  val sqlUtils = new BasicSQLUtils

  def buildSelectStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[AbstractBasicTable[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildInsertStatement(cb: ColumnBase[_]): String = new BasicInsertBuilder(cb, this).buildInsert
  def buildInsertStatement[T](cb: ColumnBase[T], q: Query[ColumnBase[T]]): SQLBuilder.Result =
    new BasicInsertBuilder(cb, this).buildInsert(q)

  def buildTableDDL(table: AbstractBasicTable[_]): DDL = new BasicDDLBuilder(table, this).buildDDL
  def buildSequenceDDL(seq: Sequence[_]): DDL = new BasicSequenceDDLBuilder(seq, this).buildDDL
}
