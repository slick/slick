package com.novocode.squery.combinator.basic

import com.novocode.squery.session.Session
import com.novocode.squery.combinator.Sequence

class BasicSequenceDDLInvoker(seq: Sequence[_], profile: BasicProfile) {

  lazy val createSequenceStatement = profile.buildCreateSequenceStatement(seq)

  def createSequence(implicit session: Session): Unit =
    session.withPreparedStatement(createSequenceStatement)(_.execute)
}
