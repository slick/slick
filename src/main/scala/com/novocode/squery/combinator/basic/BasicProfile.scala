package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{ColumnBase, SQLBuilder, NamingContext, Sequence, Query, Projection, DDL}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_ <: BasicProfile]
  type TypeMapperDelegatesT <: BasicTypeMapperDelegates

  def createQueryTemplate[P,R](query: Query[ColumnBase[R]]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_], nc: NamingContext): BasicQueryBuilder = new ConcreteBasicQueryBuilder(query, nc, None, this)
  def createDDLBuilder(table: AbstractBasicTable[_]): BasicDDLBuilder = new BasicDDLBuilder(table, this)

  val Implicit: ImplicitT
  val typeMapperDelegates: TypeMapperDelegatesT

  def buildSelectStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[AbstractBasicTable[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildInsertStatement(cb: ColumnBase[_]): String = new BasicInsertBuilder(cb, this).buildInsert
  def buildInsertStatement[T](cb: ColumnBase[T], q: Query[ColumnBase[T]]): SQLBuilder.Result =
    new BasicInsertBuilder(cb, this).buildInsert(q)

  def buildTableDDL(table: AbstractBasicTable[_]): DDL = new DDL {
    lazy val createPhase1 = Iterable(createDDLBuilder(table).buildCreateTable)
    lazy val createPhase2 = Iterable()
  }
  def buildSequenceDDL(seq: Sequence[_]): DDL = new DDL {
    lazy val createPhase1 = Iterable(new BasicSequenceDDLBuilder(seq).buildCreateSequence)
    lazy val createPhase2 = Iterable()
  }
}
