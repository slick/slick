package com.novocode.squery.session

import java.sql.ResultSet

sealed abstract case class ResultSetHoldability(intValue: Int) { self =>
  def apply[T](base: Session)(f: Session => T): T = f(base.forParameters(rsHoldability = self))
  def apply[T](f: => T)(implicit base: Session): T = apply(base)(Database.dyn.withValue(_)(f))
}

object ResultSetHoldability {
  case object Default               extends ResultSetHoldability(0)
  case object HoldCursorsOverCommit extends ResultSetHoldability(ResultSet.HOLD_CURSORS_OVER_COMMIT)
  case object CloseCursorsAtCommit  extends ResultSetHoldability(ResultSet.CLOSE_CURSORS_AT_COMMIT)
}
