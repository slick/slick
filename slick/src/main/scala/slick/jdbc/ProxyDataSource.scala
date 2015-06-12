package slick.jdbc

import java.net.InetSocketAddress
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
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

abstract class ProxyDataSource extends DataSource with Closeable {
  protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[ProxyDataSource]))

  private[this] val defaultLoginTimeout = 5 // seconds
  private[this] var driverClass: Class[_] = _
  private[this] var state = 0 // 0 = not initialized, 1 = initialized, 2 = shutting down
  private[this] var registeredDriver: Driver = _
  private[this] var cachedEndpoint: Future[Option[(InetSocketAddress, Option[Long])]] = Future.successful(None)

  @BeanProperty var url: String = _
  @BeanProperty var user: String = _
  @BeanProperty var password: String = _
  @BeanProperty var serviceName: String = _
  @BeanProperty var driver: String = _
  @BeanProperty var properties: Properties = _

  final def getConnection: Connection = getConnection(properties, user, password)

  final def getConnection(username: String, password: String): Connection = getConnection(properties, username, password)

  private[this] def getConnection(prop: Properties, user: String, password: String): Connection = {
    val connectionProps = if(prop.ne(null) && user.eq(null) && password.eq(null)) prop else {
      val p = new Properties(prop)
      if(user ne null) p.setProperty("user", user)
      if(password ne null) p.setProperty("password", password)
      p
    }
    getRealConnection(currentEndpoint, connectionProps)
  }

  private[this] def currentEndpoint: InetSocketAddress = {
    val ce = synchronized {
      val oldCached = cachedEndpoint
      val valid = cachedEndpoint.value match {
        case None => true
        case Some(Success(Some((_, Some(until))))) => System.currentTimeMillis <= until
        case Some(Success(Some((_, None)))) => true
        case Some(Success(None)) => false
        case Some(Failure(ex)) => false
      }
      if(!valid) {
        cachedEndpoint = lookup(serviceName).map(_.map { case (addrOpt, ttlOpt) =>
          (addrOpt, ttlOpt.map(ttl => System.currentTimeMillis + ttl.toMillis))
        })(DBIO.sameThreadExecutionContext)
        logger.debug("Requested new endpoint for service \""+serviceName+"\" (cached endpoint: "+oldCached.value+")")
      }
      cachedEndpoint
    }
    Await.result(ce, loginTimeoutDuration) match {
      case Some((addr, _)) => addr
      case None => throw new SQLException("No binding available for service \""+serviceName+"\"")
    }
  }

  private[this] def getRealConnection(endpoint: InetSocketAddress, props: Properties): Connection = {
    val realUrl = url.replace("{host}", endpoint.getHostString).replace("{port}", String.valueOf(endpoint.getPort))
    val st = synchronized {
      if((driver ne null) && state == 0) {
        registerDriver(driver, realUrl)
        state = 1
      }
      state
    }
    if(state == 2) throw new SQLException("ProxyDataSource is shutting down")
    DriverManager.getConnection(realUrl, props)
  }

  def lookup(serviceName: String): Future[Option[(InetSocketAddress, Option[FiniteDuration])]]

  private[this] var loginTimeout: Int = 0
  private[this] var loginTimeoutDuration: FiniteDuration = _
  def getLoginTimeout: Int = loginTimeout
  def setLoginTimeout(s: Int): Unit = {
    loginTimeout = s
    loginTimeoutDuration =
      new FiniteDuration(if(s == 0) defaultLoginTimeout else s, TimeUnit.SECONDS)
  }
  setLoginTimeout(0)

  private[this] var logWriter: PrintWriter = _
  def setLogWriter(out: PrintWriter): Unit = logWriter = out
  def getLogWriter: PrintWriter = logWriter

  def getParentLogger: Logger = throw new SQLFeatureNotSupportedException()

  def isWrapperFor(iface: Class[_]): Boolean = false

  def unwrap[T](iface: Class[T]): T =
    if(iface.isInstance(this)) this.asInstanceOf[T]
    else throw new SQLException(getClass.getName+" is not a wrapper for "+iface)

  protected[this] def registerDriver(driverName: String, url: String): Unit = if(driverName ne null) {
    val oldDriver = try DriverManager.getDriver(url) catch { case ex: SQLException if "08001" == ex.getSQLState => null }
    if(oldDriver eq null) {
      Class.forName(driverName)
      registeredDriver = DriverManager.getDriver(url)
    }
  }

  def deregisterDriver(): Boolean =
    if(registeredDriver ne null) { DriverManager.deregisterDriver(registeredDriver); true }
    else false

  def close(): Unit = synchronized {
    if(state == 1) {
      logger.debug("Shutting down ProxyDataSource")
      deregisterDriver()
      state = 2
    }
  }
}
