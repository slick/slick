package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.basic.BasicTable

abstract class ExtendedTable[T](_tableName: String) extends BasicTable[T](_tableName) {

  override val O: ExtendedColumnOptions = ExtendedColumnOptions
}
