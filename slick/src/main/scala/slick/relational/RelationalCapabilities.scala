package slick.relational

import slick.basic.{Capabilities, Capability}
import slick.model.ForeignKeyAction

/** Capabilities for [[slick.relational.RelationalProfile]]. */
object RelationalCapabilities { caps =>
  /** Computes `lengthLong` and `countDistinctLong` with a 64-bit count.
    * A profile without this capability uses a 32-bit count, so these
    * methods are limited to the `Int` range. */
  val countLong = Capability("relational.countLong")
  /** Supports onUpdate foreign key actions */
  final class OnUpdateAction[T <: ForeignKeyAction](a: T) extends Capability(s"relational.foreignKeyActions.update.${a.action}")
  /** Supports onDelete foreign key actions */
  final class OnDeleteAction[T <: ForeignKeyAction](a: T) extends Capability(s"relational.foreignKeyActions.delete.${a.action}")

  /** Supports the ''database'' function to get the current database name.
    * A profile without this capability will return an empty string. */
  val functionDatabase = Capability("relational.functionDatabase")
  /** Supports the ''user'' function to get the current database user.
    * A profile without this capability will return an empty string. */
  val functionUser = Capability("relational.functionUser")
  /** Supports repeat method on string columns */
  val repeat = Capability("relational.repeat")
  /** Supports full outer joins */
  val joinFull = Capability("relational.joinFull")
  /** Supports right outer joins */
  val joinRight = Capability("relational.joinRight")
  /** Supports replace method on string columns */
  val replace = Capability("relational.replace")
  /** Supports reverse method on string columns */
  val reverse = Capability("relational.reverse")
  /** Supports the BigDecimal data type */
  val typeBigDecimal = Capability("relational.typeBigDecimal")
  /** Supports the Blob data type */
  val typeBlob = Capability("relational.typeBlob")
  /** Supports zip, zipWith and zipWithIndex */
  val zip = Capability("relational.zip")

  /** Supports all RelationalProfile features which do not have separate capability values */
  val other = Capability("relational.other")

  /** All relational capabilities */
  val all = new All{}.capabilities
  trait All extends Capabilities {
    implicit val otherRelationalCap: Registration[caps.other.type] = register(caps.other)
    implicit val countLongCap: Registration[caps.countLong.type] = register(caps.countLong)
    implicit val functionDatabaseCap: Registration[caps.functionDatabase.type] = register(caps.functionDatabase)
    implicit val functionUserCap: Registration[caps.functionUser.type] = register(caps.functionUser)
    implicit val joinFullCap: Registration[caps.joinFull.type] = register(caps.joinFull)
    implicit val joinRightCap: Registration[caps.joinRight.type] = register(caps.joinRight)
    implicit val typeBigDecimalCap: Registration[caps.typeBigDecimal.type] = register(caps.typeBigDecimal)
    implicit val typeBlobCap: Registration[caps.typeBlob.type] = register(caps.typeBlob)
    implicit val zipCap: Registration[caps.zip.type] = register(caps.zip)
    implicit val replaceCap: Registration[caps.replace.type] = register(caps.replace)
    implicit val reverseCap: Registration[caps.reverse.type] = register(caps.reverse)
    implicit val repeatCap: Registration[caps.repeat.type] = register(caps.repeat)

    implicit val onUpdateActionNoCap: Registration[OnUpdateAction[ForeignKeyAction.NoAction.type]] = register(new OnUpdateAction(ForeignKeyAction.NoAction))
    implicit val onUpdateActionSetNullCap: Registration[OnUpdateAction[ForeignKeyAction.SetNull.type]] = register(new OnUpdateAction(ForeignKeyAction.SetNull))
    implicit val onUpdateActionCascadeCap: Registration[OnUpdateAction[ForeignKeyAction.Cascade.type]] = register(new OnUpdateAction(ForeignKeyAction.Cascade))
    implicit val onUpdateActionRestrictCap: Registration[OnUpdateAction[ForeignKeyAction.Restrict.type]] = register(new OnUpdateAction(ForeignKeyAction.Restrict))
    implicit val onUpdateActionSetDefaultCap: Registration[OnUpdateAction[ForeignKeyAction.SetDefault.type]] = register(new OnUpdateAction(ForeignKeyAction.SetDefault))

    implicit val onDeleteActionNoCap: Registration[OnDeleteAction[ForeignKeyAction.NoAction.type]] = register(new OnDeleteAction(ForeignKeyAction.NoAction))
    implicit val onDeleteActionSetNullCap: Registration[OnDeleteAction[ForeignKeyAction.SetNull.type]] = register(new OnDeleteAction(ForeignKeyAction.SetNull))
    implicit val onDeleteActionCascadeCap: Registration[OnDeleteAction[ForeignKeyAction.Cascade.type]] = register(new OnDeleteAction(ForeignKeyAction.Cascade))
    implicit val onDeleteActionRestrictCap: Registration[OnDeleteAction[ForeignKeyAction.Restrict.type]] = register(new OnDeleteAction(ForeignKeyAction.Restrict))
    implicit val onDeleteActionSetDefaultCap: Registration[OnDeleteAction[ForeignKeyAction.SetDefault.type]] = register(new OnDeleteAction(ForeignKeyAction.SetDefault))
  }
}
