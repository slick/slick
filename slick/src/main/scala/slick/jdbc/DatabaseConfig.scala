package slick.jdbc

import java.sql.Driver
import java.util.Properties
import javax.naming.InitialContext
import javax.sql.DataSource

import slick.SlickException

trait DatabaseConfig extends slick.basic.DatabaseConfig {

  def forSource[P <: JdbcProfile](
    profile: P,
    source: JdbcDataSource
  ): JdbcDatabaseConfig[P] =
    new JdbcDatabaseConfig(profile, source)

  def forDataSource[P <: JdbcProfile](
    profile: P,
    ds: DataSource,
    keepAliveConnection: Boolean = false
  ): JdbcDatabaseConfig[P] =
    forSource(profile, new DataSourceJdbcDataSource(ds, keepAliveConnection, maxConnections = None))

  def forURL[P <: JdbcProfile](
    profile: P,
    url: String,
    user: String = null,
    password: String = null,
    prop: Properties = null,
    driver: String = null,
    keepAliveConnection: Boolean = false,
    classLoader: ClassLoader = slick.util.ClassLoaderUtil.defaultClassLoader
  ): JdbcDatabaseConfig[P] = {
    val ds = new DriverDataSource(url, user, password, prop, driver, classLoader = classLoader)
    forSource(profile, new DataSourceJdbcDataSource(ds, keepAliveConnection, maxConnections = None))
  }

  def forDriver[P <: JdbcProfile](
    profile: P,
    driver: Driver,
    url: String,
    user: String = null,
    password: String = null,
    prop: Properties = null,
    classLoader: ClassLoader = slick.util.ClassLoaderUtil.defaultClassLoader
  ): JdbcDatabaseConfig[P] = {
    val ds = new DriverDataSource(url, user, password, prop, driverObject = driver)
    forSource(profile, new DataSourceJdbcDataSource(ds, keepAliveConnection = false, maxConnections = None))
  }

  def forName[P <: JdbcProfile](
    profile: P,
    name: String
  ): JdbcDatabaseConfig[P] = {
    new InitialContext().lookup(name) match {
      case ds: DataSource =>
        forSource(profile, new DataSourceJdbcDataSource(ds, keepAliveConnection = false, maxConnections = None))
      case x =>
        throw new SlickException("Expected a DataSource for JNDI name " + name + ", but got " + x)
    }
  }
}

object DatabaseConfig extends DatabaseConfig
