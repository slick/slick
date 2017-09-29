package slick.jdbc

import slick.basic.Capability

/** Capabilities for [[slick.jdbc.JdbcProfile]]. */
object JdbcCapabilities {
  /** Can be used for reverse-engineering the database schema */
  val createModel = Capability("jdbc.createModel")
  /** Can insert into AutoInc columns. */
  val forceInsert = Capability("jdbc.forceInsert")
  /** Supports a native insertOrUpdate command. Ootherwise the functionality
    * is emulated on the client side. The emulation uses transactions for
    * consistency but does not guarantee atomicity, so it may fail if another
    * insert for the same key happens concurrently. */
  val insertOrUpdate = Capability("jdbc.insertOrUpdate")
  /** Supports mutable result sets */
  val mutable = Capability("jdbc.mutable")
  /** Can return primary key of inserted rows */
  val returnInsertKey = Capability("jdbc.returnInsertKey")
  /** Can also return non-primary-key columns of inserted rows */
  val returnInsertOther = Capability("jdbc.returnInsertOther")
  /** Returns column default values in meta data */
  val defaultValueMetaData = Capability("jdbc.defaultValueMetaData")
  /** Doesn't map types to Boolean in DatabaseMetaData */
  val booleanMetaData = Capability("jdbc.booleanMetaData")
  /** Reports no default and NULL default differently in meta data */
  val nullableNoDefault = Capability("jdbc.nullableNoDefault")
  /** Makes a difference between different integer types */
  val distinguishesIntTypes = Capability("jdbc.distinguishesIntTypes")
  /** Has a datatype directly corresponding to Scala Byte */
  val supportsByte = Capability("jdbc.supportsByte")
  /** Supports FOR UPDATE row level locking */
  val forUpdate = Capability("jdbc.forUpdate")

  /** Supports all JdbcProfile features which do not have separate capability values */
  val other = Capability("jdbc.other")

  /** All JDBC capabilities */
  val all = Set(other, createModel, forceInsert, insertOrUpdate, mutable, returnInsertKey, defaultValueMetaData, booleanMetaData, nullableNoDefault, distinguishesIntTypes, supportsByte, returnInsertOther, forUpdate)
}
