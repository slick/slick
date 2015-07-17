package slick.jdbc

import scala.language.reflectiveCalls

import java.io.{PrintWriter, Closeable}
import java.sql._
import java.util.Properties
import java.util.logging.Logger
import javax.sql.DataSource

import slick.SlickException
import slick.util.{ClassLoaderUtil, Logging, ignoreFollowOnError}

import scala.beans.BeanProperty
import scala.collection.JavaConverters._
import scala.util.control.NonFatal

object DatabaseUrlDataSource {

  private val PostgresFullUrl = "^postgres://([a-zA-Z0-9_]+):([^@]+)@([^/]+)/([^\\s]+)$".r
  private val MysqlFullUrl = "^mysql://([a-zA-Z0-9_]+):([^@]+)@([^/]+)/([^\\s]+)$".r
  private val MysqlCustomProperties = ".*\\?(.*)".r

  def extractUrl(databaseUrl: Option[String]): String = {
    extractUrlComponents(Some(databaseUrl.getOrElse(defaultUrl)))._1.orNull
  }

  def extractUser(databaseUrl: Option[String], default:String): String = {
    extractUrlComponents(Some(databaseUrl.getOrElse(defaultUrl)))._2.map(_._1).getOrElse(default)
  }

  def extractPassword(databaseUrl: Option[String], default:String): String = {
    extractUrlComponents(Some(databaseUrl.getOrElse(defaultUrl)))._2.map(_._2).getOrElse(default)
  }

  def extractUrlComponents(databaseUrl: Option[String]): (Option[String], Option[(String, String)]) = {
    databaseUrl match {
      case Some(PostgresFullUrl(username, password, host, dbname)) =>
        Some(s"jdbc:postgresql://$host/$dbname") -> Some(username -> password)

      case Some(url @ MysqlFullUrl(username, password, host, dbname)) =>
        val defaultProperties = "?useUnicode=yes&characterEncoding=UTF-8&connectionCollation=utf8_general_ci"
        val addDefaultPropertiesIfNeeded = MysqlCustomProperties.findFirstMatchIn(url).map(_ => "").getOrElse(defaultProperties)
        Some(s"jdbc:mysql://$host/${dbname + addDefaultPropertiesIfNeeded}") -> Some(username -> password)

      case Some(url) =>
        Some(url) -> None

      case None =>
        None -> None
    }
  }

  def defaultUrl():String = {
    System.getenv("DATABASE_URL")
  }
}

/** A DataSource that wraps the DriverManager API. It can be configured as a Java Bean and used
  * both stand-alone and as a source for a connection pool. */
class DatabaseUrlDataSource(
    /** The Database URL */
    dbUrl: String,
    /** Optional user name */
    dbUrlUser: String = null,
    /** Optional password */
    dbUrlPassword: String = null,
    /** Optional connection properties */
    dbUrlProperties: Properties = null,
    /** Name of the `java.sql.Driver` class. This must be set unless a `driverObject` is set
      * directly or the driver is already registered with the DriverManager. */
    dbUrlDriverClassName: String = null,
    /** When `close()` is called, try to deregister a driver that was registered by this instance. */
    dbUrlDeregisterDriver: Boolean = false,
    /** The JDBC driver to use. If this is set, `driverClassName` will be ignored. */
    dbUrlDriverObject: Driver = null,
    /** The ClassLoader that is used to load `driverClassName` */
    dbUrlClassLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ) extends DriverDataSource(
    DatabaseUrlDataSource.extractUrl(Some(dbUrl)),
    DatabaseUrlDataSource.extractUser(Some(dbUrl), dbUrlUser),
    DatabaseUrlDataSource.extractPassword(Some(dbUrl), dbUrlPassword),
    dbUrlProperties,
    dbUrlDriverClassName,
    dbUrlDeregisterDriver,
    dbUrlDriverObject,
    dbUrlClassLoader
  ) { }
