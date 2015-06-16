package slick.jdbc

import java.util.Properties
import java.util.concurrent.TimeUnit

import org.slf4j.LoggerFactory
import slick.dbio.DBIO
import slick.util.SlickLogger

import scala.beans.BeanProperty

import java.io.{Closeable, PrintWriter}
import java.sql._
import java.util.logging.Logger
import javax.sql.DataSource

import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object ProxyDataSource {
  private[jdbc] final val LoginTimeout = 5.seconds
}

/**
 * A ProxyDataSource looks up a connection on demand via an implementor's lookup function.
 * Resolving a connection this way supports the use-case where the address of a database
 * may change at any time, or indeed be or become unknown. This use-case is quite common
 * for reactive systems that require resiliency i.e. most systems. Its use is therefore
 * encouraged.
 * 
 * A lookup method is required to be implemented with its purpose being to resolve the 
 * location of a database.
 */
abstract class ProxyDataSource extends DataSource with Closeable {
  
  /**
   * Override this function in order to resolve some address. If the address
   * cannot be obtained for whatever reason then return None. The function
   * is expected to be re-entrant.
   */
  def lookup(serviceName: String): Future[Option[(String, Int)]]
  
  import ProxyDataSource._

  protected val logger = new SlickLogger(LoggerFactory.getLogger(classOf[ProxyDataSource]))

  private var state = 0 // 0 = not initialized, 1 = initialized, 2 = shutting down
  private var registeredDriver: Driver = _
  private var cachedEndpoint: Future[Option[(String, Int)]] = Future.successful(None)

  @BeanProperty var url: String = _
  @BeanProperty var user: String = _
  @BeanProperty var password: String = _
  @BeanProperty var serviceName: String = _
  @BeanProperty var driver: String = _
  @BeanProperty var properties: Properties = _

  override final def getConnection: Connection = getConnection(properties, user, password)

  override final def getConnection(username: String, password: String): Connection = 
    getConnection(properties, username, password)

  private def getConnection(prop: Properties, user: String, password: String): Connection = {
    val connectionProps = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
      val p = new Properties(prop)
      if(user ne null) p.setProperty("user", user)
      if(password ne null) p.setProperty("password", password)
      p
    }
    getConnection(currentEndpoint, connectionProps)
  }

  private def currentEndpoint: (String, Int) = {
    val ce = synchronized {
      val oldCached = cachedEndpoint
      val valid = cachedEndpoint.value match {
        case None => false
        case Some(Success(Some(_))) => true
        case Some(Success(None)) => false
        case Some(Failure(ex)) => false
      }
      if(!valid) {
        cachedEndpoint = lookup(serviceName)
        logger.debug(s"""Requested new host and port for service "$serviceName" (cached: ${oldCached.value})""")
      }
      cachedEndpoint
    }
    Await.result(ce, loginTimeout) match {
      case Some(addr) => addr
      case None => throw new SQLException(s"""No binding available for service "$serviceName"""")
    }
  }

  private def getConnection(hostAndPort: (String, Int), props: Properties): Connection = {
    val (host, port) = hostAndPort
    val realUrl = url.replace("{host}", host).replace("{port}", port.toString)
    val st = synchronized {
      if((driver ne null) && state == 0) {
        registerDriver(driver, realUrl)
        state = 1
      }
      state
    }
    if(st == 2) throw new SQLException("ProxyDataSource is shutting down")
    DriverManager.getConnection(realUrl, props)
  }

  private var loginTimeout: FiniteDuration = LoginTimeout
  def getLoginTimeout: Int = loginTimeout.toSeconds.toInt
  def setLoginTimeout(s: Int): Unit =
    loginTimeout = 
      if(s==0) LoginTimeout 
      else FiniteDuration(s, TimeUnit.SECONDS)

  private var logWriter: PrintWriter = _
  def setLogWriter(out: PrintWriter): Unit = logWriter = out
  def getLogWriter: PrintWriter = logWriter

  def getParentLogger: Logger = throw new SQLFeatureNotSupportedException()

  def isWrapperFor(iface: Class[_]): Boolean = false

  override def unwrap[T](iface: Class[T]): T =
    if(iface.isInstance(this)) this.asInstanceOf[T]
    else throw new SQLException(getClass.getName+" is not a wrapper for "+iface)

  private def registerDriver(driverName: String, url: String): Unit = if(driverName ne null) {
    val oldDriver = try DriverManager.getDriver(url) catch { case ex: SQLException if "08001" == ex.getSQLState => null }
    if(oldDriver eq null) {
      Class.forName(driverName)
      registeredDriver = DriverManager.getDriver(url)
    }
  }

  override protected def close(): Unit = synchronized {
    if(state == 1) {
      logger.debug("Shutting down ProxyDataSource")
      if(registeredDriver ne null) DriverManager.deregisterDriver(registeredDriver)
      state = 2
    }
  }
}
