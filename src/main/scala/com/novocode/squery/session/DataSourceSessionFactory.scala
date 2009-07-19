package com.novocode.squery.session

import java.sql.Connection
import javax.sql.DataSource

/**
 * A session factory based on a DataSource.
 */
class DataSourceSessionFactory(ds: DataSource, driver: String) extends SessionFactory {

  def this(ds: DataSource) = this(ds, null)

  if(driver ne null) Class.forName(driver)

  protected[squery] def createConnection(): Connection = ds.getConnection
}
