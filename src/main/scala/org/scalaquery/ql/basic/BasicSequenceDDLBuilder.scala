package org.scalaquery.ql.basic

import org.scalaquery.ql._

class BasicSequenceDDLBuilder(seq: Sequence[_], val profile: BasicProfile) {
  import profile.sqlUtils._

  def buildDDL: DDL = {
    val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
    seq._increment.foreach { b append " INCREMENT " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    seq._start.foreach { b append " START " append _ }
    if(seq._cycle) b append " CYCLE"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Nil
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }
}
