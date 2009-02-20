package com.novocode.squery.combinator

abstract class ColumnOption

object ColumnOption {
  case object AutoInc extends ColumnOption
  case object NotNull extends ColumnOption
}
