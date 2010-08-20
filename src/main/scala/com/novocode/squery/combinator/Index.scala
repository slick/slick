package com.novocode.squery.combinator

class Index(val name: String, val table: AbstractTable[_], val on: ColumnBase[_], val unique: Boolean)
