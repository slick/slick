package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{ColumnBase, SQLBuilder, NamingContext, Sequence, Query, Projection}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_ <: BasicProfile]
  type TypeMapperDelegatesT <: BasicTypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[ColumnBase[R]]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_], nc: NamingContext): BasicQueryBuilder = new ConcreteBasicQueryBuilder(query, nc, None, this)
  def createDDLBuilder(table: BasicTable[_]): BasicDDLBuilder = new BasicDDLBuilder(table, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT

  def buildSelectStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[BasicTable[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildCreateTableStatement(table: BasicTable[_]): String = createDDLBuilder(table).buildCreateTable
  def buildInsertStatement(cb: ColumnBase[_]): String = new BasicInsertBuilder(cb, this).buildInsert
  def buildInsertStatement[T](cb: ColumnBase[T], q: Query[ColumnBase[T]]): SQLBuilder.Result =
    new BasicInsertBuilder(cb, this).buildInsert(q)
  def buildCreateSequenceStatement(seq: Sequence[_]): String = new BasicSequenceDDLBuilder(seq).buildCreateSequence
}
