package com.novocode.squery.session

import java.sql.ResultSet

sealed abstract case class ResultSetConcurrency(intValue: Int) { self =>
  def apply[T](base: Session)(f: Session => T): T = f(base.forParameters(rsConcurrency = self))
  def apply[T](f: => T)(implicit base: Session): T = apply(base)(Database.dyn.withValue(_)(f))
  def withDefault(r: ResultSetConcurrency) = this
}

object ResultSetConcurrency {
  case object Auto      extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY) {
    override def withDefault(r: ResultSetConcurrency) = r
  }
  case object ReadOnly  extends ResultSetConcurrency(ResultSet.CONCUR_READ_ONLY)
  case object Updatable extends ResultSetConcurrency(ResultSet.CONCUR_UPDATABLE)
}
