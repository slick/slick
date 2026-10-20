package slick.jdbc

import slick.basic.{Capabilities, Capability}

/** Capabilities for [[slick.jdbc.JdbcProfile]]. */
object JdbcCapabilities { caps =>
  /** Can be used for reverse-engineering the database schema */
  val createModel = Capability("jdbc.createModel")
  /** Can insert into AutoInc columns. */
  val forceInsert = Capability("jdbc.forceInsert")
  /** Supports a native insertOrUpdate command. Otherwise the functionality
   * is emulated on the client side. The emulation uses transactions for
   * consistency but does not guarantee atomicity, so it may fail if another
   * insert for the same key happens concurrently. */
  val insertOrUpdate = Capability("jdbc.insertOrUpdate")
  /** Supports `insertOrUpdate` with only primary keys (equivalent to "insert if not exists") */
  val insertOrUpdateWithPrimaryKeyOnly = Capability("jdbc.insertOrUpdateWithPrimaryKeyOnly")
  /** Supports `insertAll` with `rowsPerStatement = RowsPerStatement.One` */
  val insertMultipleRowsWithSingleStatement = Capability("jdbc.insertMultipleRowsWithSingleStatement")
  /** Supports mutable result sets */
  val mutable = Capability("jdbc.mutable")
  /** Can return primary key of inserted rows */
  val returnInsertKey = Capability("jdbc.returnInsertKey")
  /** Can return multiple primary key of inserted rows in a single statement */
  val returnMultipleInsertKey = Capability("jdbc.returnMultipleInsertKey")
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
  /** Supports FOR SHARE row level locking */
  val forShare = Capability("jdbc.forShare")

  /** Supports all JdbcProfile features which do not have separate capability values */
  val other = Capability("jdbc.other")

  /** All JDBC capabilities */
  val all = new All{}.capabilities
  trait All extends Capabilities {
    implicit val booleanMetaData: Registration[caps.booleanMetaData.type] = register(caps.booleanMetaData)
    implicit val createModel: Registration[caps.createModel.type] = register(caps.createModel)
    implicit val defaultValueMetaData: Registration[caps.defaultValueMetaData.type] = register(caps.defaultValueMetaData)
    implicit val distinguishesIntTypes: Registration[caps.distinguishesIntTypes.type] = register(caps.distinguishesIntTypes)
    implicit val forUpdate: Registration[caps.forUpdate.type] = register(caps.forUpdate)
    implicit val forShare: Registration[caps.forShare.type] = register(caps.forShare)
    implicit val forceInsert: Registration[caps.forceInsert.type] = register(caps.forceInsert)
    implicit val insertOrUpdate: Registration[caps.insertOrUpdate.type] = register(caps.insertOrUpdate)
    implicit val insertOrUpdateWithPrimaryKeyOnly: Registration[caps.insertOrUpdateWithPrimaryKeyOnly.type] = register(caps.insertOrUpdateWithPrimaryKeyOnly)
    implicit val insertMultipleRowsWithSingleStatement: Registration[caps.insertMultipleRowsWithSingleStatement.type] = register(caps.insertMultipleRowsWithSingleStatement)
    implicit val mutable: Registration[caps.mutable.type] = register(caps.mutable)
    implicit val nullableNoDefault: Registration[caps.nullableNoDefault.type] = register(caps.nullableNoDefault)
    implicit val otherJdbc: Registration[caps.other.type] = register(caps.other)
    implicit val returnInsertKey: Registration[caps.returnInsertKey.type] = register(caps.returnInsertKey)
    implicit val returnInsertOther: Registration[caps.returnInsertOther.type] = register(caps.returnInsertOther)
    implicit val returnMultipleInsertKey: Registration[caps.returnMultipleInsertKey.type] = register(caps.returnMultipleInsertKey)
    implicit val supportsByte: Registration[caps.supportsByte.type] = register(caps.supportsByte)
  }
}
