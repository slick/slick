package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{ColumnBase, ColumnBaseU, Table, SQLBuilder, NamingContext, Sequence, Query, Projection}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_ <: BasicProfile]
  type TypeMapperDelegatesT <: BasicTypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[ColumnBase[R]]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_], nc: NamingContext): BasicQueryBuilder = new ConcreteBasicQueryBuilder(query, nc, None, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT

  def buildSelectStatement(query: Query[ColumnBaseU], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[ColumnBaseU], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[Table[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildCreateTableStatement(table: Table[_]): String = new BasicDDLBuilder(table, this).buildCreateTable
  def buildInsertStatement(cb: ColumnBaseU): String = new BasicInsertBuilder(cb).buildInsert
  def buildCreateSequenceStatement(seq: Sequence[_]): String = new BasicSequenceDDLBuilder(seq).buildCreateSequence
}
