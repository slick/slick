package org.scalaquery.util

import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.session.{PositionedParameters, PositionedResult}

/**
 * Converts between unpacked (e.g. in query results) and linearized (a
 * sequence of columns) form of values.
 */

trait ValueLinearizer[T] {
  def getResult(profile: BasicProfile, rs: PositionedResult): T
  def updateResult(profile: BasicProfile, rs: PositionedResult, value: T): Unit
  def setParameter(profile: BasicProfile, ps: PositionedParameters, value: Option[T]): Unit
}
