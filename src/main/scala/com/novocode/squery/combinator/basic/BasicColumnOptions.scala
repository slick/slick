package com.novocode.squery.combinator.basic

import com.novocode.squery.combinator.ColumnOption

class BasicColumnOptions {
  val NotNull = BasicColumnOption.NotNull
  val PrimaryKey = BasicColumnOption.PrimaryKey
  def Default[T](defaultValue: T) = BasicColumnOption.Default[T](defaultValue)
  def DBType(dbType: String) = BasicColumnOption.DBType(dbType)
}

object BasicColumnOptions extends BasicColumnOptions

object BasicColumnOption {
  case object NotNull extends ColumnOption
  case object PrimaryKey extends ColumnOption
  case class Default[T](val defaultValue: T) extends ColumnOption[T]
  case class DBType(val dbType: String) extends ColumnOption
}
