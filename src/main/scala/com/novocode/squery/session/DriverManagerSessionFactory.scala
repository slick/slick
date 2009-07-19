package com.novocode.squery.session

import java.sql.{Connection, DriverManager}

/**
 * A session factory that uses the DriverManager to open new connections.
 */
class DriverManagerSessionFactory(url:String, driver:String) extends SessionFactory {

  if(driver ne null) Class.forName(driver)

  protected[squery] def createConnection(): Connection = DriverManager.getConnection(url)
}
