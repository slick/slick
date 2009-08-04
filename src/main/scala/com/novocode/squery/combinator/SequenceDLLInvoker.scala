package com.novocode.squery.combinator

import com.novocode.squery.session.Session
import com.novocode.squery.combinator.sql.SequenceDDLBuilder

class SequenceDDLInvoker(seq: Sequence[_]) {

  lazy val createSequenceStatement = new SequenceDDLBuilder(seq).buildCreateSequence

  def createSequence(implicit session: Session) {
    val st = session.allocPS(createSequenceStatement)
    try { st.execute } finally session.freePS(createSequenceStatement, st)
  }
}
