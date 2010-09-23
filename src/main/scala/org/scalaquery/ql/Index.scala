package org.scalaquery.ql

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(val name: String, val table: AbstractTable[_], val on: ColumnBase[_], val unique: Boolean)
