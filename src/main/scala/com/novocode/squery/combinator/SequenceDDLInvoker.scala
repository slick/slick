package com.novocode.squery.combinator

import com.novocode.squery.session.Session
import com.novocode.squery.combinator.sql.SequenceDDLBuilder

class SequenceDDLInvoker(seq: Sequence[_]) {

  lazy val createSequenceStatement = new SequenceDDLBuilder(seq).buildCreateSequence

  def createSequence(implicit session: Session): Unit =
    session.withPS(createSequenceStatement)(_.execute)
}
