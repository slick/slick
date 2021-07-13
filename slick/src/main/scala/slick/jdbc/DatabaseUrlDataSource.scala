package slick.jdbc

/** A DataSource that wraps the DriverManager API. It can be configured as a Java Bean and used
  * both stand-alone and as a source for a connection pool. This implementation is design
  * specifically to handle a non-JDBC Database URL in the format defined by the libpq standard.
  */
class DatabaseUrlDataSource extends DriverDataSource(null) {

  private val PostgresFullUrl = "^(?:postgres|postgresql)://([a-zA-Z0-9_]+)(?::([^@]+))?@([^/]+)/([^\\s]+)$".r
  private val MysqlFullUrl = "^mysql://([a-zA-Z0-9_]+)(?::([^@]+))?@([^/]+)/([^\\s]+)$".r
  private val MysqlCustomProperties = ".*\\?(.*)".r

  @volatile private[this] var initialized = false

  override def init(): Unit = if(!initialized) {
    val (jdbcUrl, userAndPass) = extractUrl(Some(if (url == null) defaultUrl() else url))
    url = jdbcUrl.orNull
    user = userAndPass.map(_._1).getOrElse(user)
    password = userAndPass.map(_._2).getOrElse(password)
    initialized = true
    super.init()
  }

  private[this] def extractUrl(databaseUrl: Option[String]): (Option[String], Option[(String, String)]) = {
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

  private[this] def defaultUrl():String = {
    System.getenv("DATABASE_URL")
  }
}
