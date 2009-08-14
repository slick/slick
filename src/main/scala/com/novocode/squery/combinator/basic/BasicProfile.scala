package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.{ColumnBase, Table, SQLBuilder, NamingContext}

trait BasicProfile {
  type ImplicitT <: BasicImplicitConversions[_]

  def createQueryTemplate[P,R](query: Query[ColumnBase[R]]): BasicQueryTemplate[P,R] = new BasicQueryTemplate[P,R](query, this)
  def createQueryBuilder(query: Query[_], nc: NamingContext): BasicQueryBuilder = new BasicQueryBuilder(query, nc, None)

  val Implicit: ImplicitT

  def buildSelectStatement(query: Query[ColumnBase[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildSelect
  def buildUpdateStatement(query: Query[Projection[_]], nc: NamingContext): String =
    createQueryBuilder(query, nc).buildUpdate
  def buildDeleteStatement(query: Query[Table[_]], nc: NamingContext): SQLBuilder.Result =
    createQueryBuilder(query, nc).buildDelete

  def buildCreateTableStatement(table: Table[_]): String = new BasicDDLBuilder(table).buildCreateTable
  def buildInsertStatement(cb: ColumnBase[_]): String = new BasicInsertBuilder(cb).buildInsert
  def buildCreateSequenceStatement(seq: Sequence[_]): String = new BasicSequenceDDLBuilder(seq).buildCreateSequence
}
