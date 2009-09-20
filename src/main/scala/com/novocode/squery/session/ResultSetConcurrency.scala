package com.novocode.squery.session

import java.sql.ResultSet

sealed abstract case class ResultSetConcurrency(intValue: Int) { self =>
  def apply[T](base: Session)(f: Session => T): T = f(base.forParameters(rsConcurrency = self))
  def apply[T](f: => T)(implicit base: Session): T = apply(base)(Database.dyn.withValue(_)(f))
}

object ResultSetConcurrency {
  case object ReadOnly  extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY)
  case object Updatable extends ResultSetConcurrency(ResultSet.CONCUR_UPDATABLE)
}
