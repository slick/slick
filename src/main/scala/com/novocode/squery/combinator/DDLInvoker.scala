package com.novocode.squery.combinator

import com.novocode.squery.session.Session
import com.novocode.squery.combinator.sql.DDLBuilder

class DDLInvoker[T](table: Table[T]) {

  lazy val createTableStatement = new DDLBuilder(table).buildCreateTable

  def createTable(implicit session: Session): Unit =
    session.withPS(createTableStatement)(_.execute)
}
