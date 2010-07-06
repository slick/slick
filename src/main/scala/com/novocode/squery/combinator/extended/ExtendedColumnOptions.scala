package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.ColumnOption
import com.novocode.squery.combinator.basic.BasicColumnOptions

class ExtendedColumnOptions extends BasicColumnOptions {
  val AutoInc = ExtendedColumnOption.AutoInc
}

object ExtendedColumnOptions extends ExtendedColumnOptions

object ExtendedColumnOption {
  case object AutoInc extends ColumnOption
}
