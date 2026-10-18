package slick.basic

import java.net.URI

import scala.reflect.ClassTag
import scala.util.control.NonFatal

import slick.{ControlsConfig, SlickException}
import slick.util.{ClassLoaderUtil, SlickLogger}
import slick.util.ConfigExtensionMethods.configExtensionMethods

import com.typesafe.config.{Config, ConfigFactory}

private[basic] object DatabaseConfigHelpers {
  private lazy val logger = SlickLogger[DatabaseConfig]

  def resolveProfileName(basePath: String, config: Config): String =
    config.getStringOpt(basePath + "profile").getOrElse {
      val nOld = config.getStringOpt(basePath + "driver").map {
        case "slick.driver.DerbyDriver$"    => "slick.jdbc.DerbyProfile$"
        case "slick.driver.H2Driver$"       => "slick.jdbc.H2Profile$"
        case "slick.driver.HsqldbDriver$"   => "slick.jdbc.HsqldbProfile$"
        case "slick.driver.MySQLDriver$"    => "slick.jdbc.MySQLProfile$"
        case "slick.driver.PostgresDriver$" => "slick.jdbc.PostgresProfile$"
        case "slick.driver.SQLiteDriver$"   => "slick.jdbc.SQLiteProfile$"
        case "slick.memory.MemoryDriver$"   => "slick.memory.MemoryProfile$"
        case n                               => n
      }
      if (nOld.isDefined)
        logger.warn(s"Use `${basePath}profile` instead of `${basePath}driver`. The latter is deprecated since Slick 3.2 and will be removed.")
      nOld.getOrElse(config.getString(basePath + "profile"))
    }

  def instantiateProfile[P <: BasicProfile: ClassTag](n: String, classLoader: ClassLoader): P = {
    val untypedP = try {
      if (n.endsWith("$")) classLoader.loadClass(n).getField("MODULE$").get(null)
      else classLoader.loadClass(n).getConstructor().newInstance()
    } catch {
      case NonFatal(ex) =>
        throw new SlickException(s"""Error getting instance of profile "$n"""", ex)
    }

    val pClass = implicitly[ClassTag[P]].runtimeClass
    if (!pClass.isInstance(untypedP))
      throw new SlickException(s"Configured profile $n does not conform to requested profile ${pClass.getName}")
    untypedP.asInstanceOf[P]
  }
}

trait DatabaseConfig {
  import DatabaseConfigHelpers._

  /** Load a database configuration through [[https://github.com/lightbend/config Typesafe Config]], reading the
    * profile from it: `profile` is the fully qualified name of the profile object, e.g.
    * `"slick.jdbc.PostgresProfile$"`. See [[forProfileConfig]] for the other configuration keys.
    *
    * @param path The path in the configuration file for the database configuration, or an empty string for the
    *             top level of the `Config` object.
    * @param config The `Config` object to read from. This defaults to the global app config (e.g. in
    *               `application.conf` at the root of the class path). */
  def forConfig[P <: BasicProfile: ClassTag](
    path: String,
    config: Config = ConfigFactory.load(),
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): BasicDatabaseConfig[P] = {
    val basePath = if (path.isEmpty) "" else path + "."
    val n = resolveProfileName(basePath, config)
    val p = instantiateProfile[P](n, classLoader)
    forProfileConfig(p, path, config, classLoader)
  }

  /** Load a database configuration for the given profile through
    * [[https://github.com/lightbend/config Typesafe Config]].
    *
    * The following keys go directly in the configuration at `path` or in its `db` sub-section:
    * <ul>
    *   <li>`connectionPool` (String, optional, default: `HikariCP`): The connection pool implementation.
    *     `HikariCP` requires the "slick-hikaricp" dependency. Use `disabled` to use a DataSource or the JDBC driver
    *     directly, or the fully qualified name of a third-party [[slick.jdbc.JdbcDataSourceFactory]] object.</li>
    *   <li>`url` (String): The JDBC URL.</li>
    *   <li>`driver` (String, optional): The fully qualified name of the JDBC driver class.</li>
    *   <li>`user` and `password` (String, optional): The credentials.</li>
    *   <li>`dataSourceClass` (String, optional): The name of a `javax.sql.DataSource` class provided by the JDBC
    *     driver, to use instead of `url` and `driver`.</li>
    *   <li>`properties` (Map, optional): Properties for the JDBC driver, or for the DataSource when
    *     `dataSourceClass` is set.</li>
    *   <li>`keepAliveConnection` (Boolean, optional, default: false, only without a connection pool): Keep one
    *     connection open as long as the database is open.</li>
    *   <li>`isolation` (String, optional, e.g. `READ_COMMITTED`), `readOnly` (Boolean, optional) and `catalog`
    *     (String, optional): Settings for each new connection.</li>
    *   <li>`maxConnections` (Int, optional, default: 20): The maximum number of concurrent JDBC connections. With
    *     HikariCP this is also the maximum pool size.</li>
    *   <li>`maxInflightActions` (Int, optional, default: twice `maxConnections`), `queueSize` (Int, optional,
    *     default: 1000), `inflightAdmissionTimeout` and `connectionAcquireTimeout` (Duration, optional): The
    *     limits on concurrently running and waiting database actions, see [[slick.ControlsConfig]].</li>
    * </ul>
    *
    * With HikariCP, `minConnections`, `connectionTimeout`, `idleTimeout`, `maxLifetime`, `connectionTestQuery`,
    * `poolName`, `registerMbeans` and the other
    * [[https://github.com/brettwooldridge/HikariCP#gear-configuration-knobs-baby HikariCP settings]] are passed
    * on to the pool under their HikariCP names.
    *
    * @param profile The profile to use.
    * @param path The path in the configuration file for the database configuration, or an empty string for the
    *             top level of the `Config` object.
    * @param config The `Config` object to read from. This defaults to the global app config (e.g. in
    *               `application.conf` at the root of the class path). */
  def forProfileConfig[P <: BasicProfile](
    profile: P,
    path: String,
    config: Config = ConfigFactory.load(),
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): BasicDatabaseConfig[P] = {
    val profileConfig = if (path.isEmpty) config else config.getConfig(path)
    // Read concurrency settings from the optional "db" sub-section (or the profile config itself
    // for the flat format), mirroring the key names used by the legacy numThreads/maxConnections
    // config style.  These become the initial ControlsConfig; callers may override via
    // BasicDatabaseConfig#withControls.
    val dbConfig = if (profileConfig.hasPath("db")) profileConfig.getConfig("db") else profileConfig
    val configuredMaxConnections =
      dbConfig.getIntOpt("maxConnections").orElse(dbConfig.getIntOpt("numThreads")).getOrElse(ControlsConfig().maxConnections)
    val queueSize    = dbConfig.getIntOr("queueSize", ControlsConfig().queueSize)
    val maxInflight  = dbConfig.getIntOpt("maxInflightActions")
    val inflightAdmissionTimeout = dbConfig.getFiniteDurationOpt("inflightAdmissionTimeout")
    val connectionAcquireTimeout = dbConfig.getFiniteDurationOpt("connectionAcquireTimeout")
    val controls = ControlsConfig(
      maxConnections           = configuredMaxConnections,
      queueSize                = queueSize,
      maxInflight              = maxInflight,
      inflightAdmissionTimeout = inflightAdmissionTimeout,
      connectionAcquireTimeout = connectionAcquireTimeout
    )
    new BasicDatabaseConfig[P](
      profile,
      profileConfig,
      path,
      config,
      classLoader,
      profile.getClass.getName,
      controls
    )
  }

  protected def splitURI(uri: URI): (String, String) = {
    val f = uri.getRawFragment
    val s = uri.toString
    if (s.isEmpty) (null, "")
    else if (f eq null) (s, "")
    else if (s.startsWith("#")) (null, uri.getFragment)
    else (s.substring(0, s.length - f.length - 1), uri.getFragment)
  }

  def forURI[P <: BasicProfile: ClassTag](
    uri: URI,
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): BasicDatabaseConfig[P] = {
    val (base, path) = splitURI(uri)
    val root =
      if (base eq null) ConfigFactory.load(classLoader)
      else ConfigFactory.parseURL(new URI(base).toURL).resolve()
    forConfig[P](path, root, classLoader)
  }

  def forProfileURI[P <: BasicProfile](
    profile: P,
    uri: URI,
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): BasicDatabaseConfig[P] = {
    val (base, path) = splitURI(uri)
    val root =
      if (base eq null) ConfigFactory.load(classLoader)
      else ConfigFactory.parseURL(new URI(base).toURL).resolve()
    forProfileConfig(profile, path, root, classLoader)
  }
}

object DatabaseConfig extends DatabaseConfig
