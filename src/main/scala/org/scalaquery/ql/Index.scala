package org.scalaquery.ql

import org.scalaquery.util.Node

/**
 * An index (or foreign key constraint with an implicit index).
 */
class Index(val name: String, val table: AbstractTable[_], val on: IndexedSeq[Node], val unique: Boolean)
