package org.scalaquery.session

import java.sql.ResultSet

sealed abstract class ResultSetHoldability(val intValue: Int) { self =>
  def apply[T](base: Session)(f: Session => T): T = f(base.forParameters(rsHoldability = self))
  def apply[T](f: => T)(implicit base: Session): T = apply(base)(Database.dyn.withValue(_)(f))
  def withDefault(r: ResultSetHoldability) = this
}

object ResultSetHoldability {
  case object Auto                  extends ResultSetHoldability(0) {
    override def withDefault(r: ResultSetHoldability) = r
  }
  case object Default               extends ResultSetHoldability(0)
  case object HoldCursorsOverCommit extends ResultSetHoldability(ResultSet.HOLD_CURSORS_OVER_COMMIT)
  case object CloseCursorsAtCommit  extends ResultSetHoldability(ResultSet.CLOSE_CURSORS_AT_COMMIT)
}
