package slick.basic

import java.net.URI

import scala.reflect.ClassTag
import scala.util.control.NonFatal

import slick.SlickException
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

  def forProfileConfig[P <: BasicProfile](
    profile: P,
    path: String,
    config: Config = ConfigFactory.load(),
    classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader
  ): BasicDatabaseConfig[P] =
    new BasicDatabaseConfig[P](
      profile,
      if (path.isEmpty) config else config.getConfig(path),
      path,
      config,
      classLoader,
      profile.getClass.getName
    )

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
