package com.novocode.squery.combinator.basic

import com.novocode.squery.session.Session
import com.novocode.squery.combinator.Table

class BasicDDLInvoker[T](table: Table[T], profile: BasicProfile) {

  lazy val createTableStatement = profile.buildCreateTableStatement(table)

  def createTable(implicit session: Session): Unit =
    session.withPS(createTableStatement)(_.execute)
}
