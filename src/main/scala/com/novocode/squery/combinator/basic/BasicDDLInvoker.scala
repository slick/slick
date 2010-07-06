package com.novocode.squery.combinator.basic

import com.novocode.squery.session.Session

class BasicDDLInvoker[T](table: BasicTable[T], profile: BasicProfile) {

  lazy val createTableStatement = profile.buildCreateTableStatement(table)

  def createTable(implicit session: Session): Unit =
    session.withPreparedStatement(createTableStatement)(_.execute)
}
