package com.novocode.squery.session

import java.sql.Connection
import javax.sql.DataSource

/**
 * A session factory based on a DataSource.
 */
class DataSourceSessionFactory(driver: String, ds: DataSource) extends SessionFactory {

  def this(ds: DataSource) = this(null, ds)

  if(!(driver eq null)) Class.forName(driver)

  protected[squery] def createConnection(): Connection = ds.getConnection
}
