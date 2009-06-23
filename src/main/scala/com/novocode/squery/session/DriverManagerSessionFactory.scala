package com.novocode.squery.session

import java.sql.{Connection, DriverManager}

/**
 * A session factory that uses the DriverManager to open new connections.
 */
class DriverManagerSessionFactory(driver:String, connUrl:String) extends SessionFactory {

  Class.forName(driver)

  protected[squery] def createConnection(): Connection = DriverManager.getConnection(connUrl)
}
