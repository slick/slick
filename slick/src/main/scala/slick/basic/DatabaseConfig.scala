package slick.basic

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, Annotation}
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context
import scala.util.control.NonFatal

import java.net.{URL, URI}

import slick.util.{SlickLogger, ClassLoaderUtil}
import slick.util.ConfigExtensionMethods.configExtensionMethods
import slick.SlickException

import com.typesafe.config.{ConfigFactory, Config}

/** A configuration for a Database plus a matching Profile. */
trait DatabaseConfig[P <: BasicProfile] {
  /** Get the configured Database. It is instantiated lazily when this method is called for the
    * first time, and must be closed after use. */
  def db: P#Backend#Database

  /** The configured Profile. */
  val profile: P
  @deprecated("Use `profile` instead of `driver`", "3.2")
  val driver: P

  /** The raw configuration. */
  def config: Config

  /** The name of the Profile class or object (without a trailing "$"). */
  def profileName: String
  @deprecated("Use `profileName` instead of `driverName`", "3.2")
  final def driverName: String = profileName

  /** Whether the `profileName` represents an object instead of a class. */
  def profileIsObject: Boolean
  @deprecated("Use `profileIsObject` instead of `driverIsObject`", "3.2")
  final def driverIsObject: Boolean = profileIsObject
}

object DatabaseConfig {
  private[this] lazy val logger = SlickLogger[DatabaseConfig[_]]

  /** Load a Profile and database configuration through
    * [[https://github.com/typesafehub/config Typesafe Config]].
    *
    * The following config parameters are available:
    * <ul>
    *   <li>`profile` (String, required): The fully qualified name of a class or object which
    *   implements the specified profile. If the name ends with `$` it is assumed to be an object
    *   name, otherwise a class name.</li>
    *   <li>`db` (Config, optional): The configuration of a database for the profile's backend.
    *   For profiles extending `JdbcProfile` (which always use `JdbcBackend`), see
    *   `JdbcBackend.DatabaseFactory.forConfig` for parameters that should be defined inside of
    *   `db`.</li>
    * </ul>
    *
    * Note: Slick 3.2 also supports the old `driver` parameter as an alternative to `profile`.
    * Old profile names (e.g. ``slick.driver.DerbyDriver$`` for ``slick.jdbc.DerbyProfile$``) are
    * recognized and translated to the new names. This feature is deprecated and will be removed
    * in a future release.
    *
    * @param path The path in the configuration file for the database configuration (e.g. `foo.bar`
    *             would find a profile name at config key `foo.bar.profile`) or an empty string
    *             for the top level of the `Config` object.
    * @param config The `Config` object to read from. This defaults to the global app config
    *               (e.g. in `application.conf` at the root of the class path) if not specified.
    * @param classLoader The ClassLoader to use to load any custom classes from. The default is to
    *                    try the context ClassLoader first and fall back to Slick's ClassLoader.
    */
  def forConfig[P <: BasicProfile : ClassTag](path: String, config: Config = ConfigFactory.load(),
                                              classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): DatabaseConfig[P] = {
    val basePath = (if(path.isEmpty) "" else path + ".")
    val n = config.getStringOpt(basePath + "profile").getOrElse {
      val nOld = config.getStringOpt(basePath + "driver").map {
        case "slick.driver.DerbyDriver$" => "slick.jdbc.DerbyProfile$"
        case "slick.driver.H2Driver$" => "slick.jdbc.H2Profile$"
        case "slick.driver.HsqldbDriver$" => "slick.jdbc.HsqldbProfile$"
        case "slick.driver.MySQLDriver$" => "slick.jdbc.MySQLProfile$"
        case "slick.driver.PostgresDriver$" => "slick.jdbc.PostgresProfile$"
        case "slick.driver.SQLiteDriver$" => "slick.jdbc.SQLiteProfile$"
        case "slick.memory.MemoryDriver$" => "slick.memory.MemoryProfile$"
        case n => n
      }
      if(nOld.isDefined)
        logger.warn(s"Use `${basePath}profile` instead of `${basePath}driver`. The latter is deprecated since Slick 3.2 and will be removed.")
      nOld.getOrElse(config.getString(basePath+"profile")) // trigger the correct error
    }

    val untypedP = try {
      if(n.endsWith("$")) classLoader.loadClass(n).getField("MODULE$").get(null)
      else classLoader.loadClass(n).newInstance()
    } catch { case NonFatal(ex) =>
      throw new SlickException(s"""Error getting instance of profile "$n"""", ex)
    }
    val pClass = implicitly[ClassTag[P]].runtimeClass
    if(!pClass.isInstance(untypedP))
      throw new SlickException(s"Configured profile $n does not conform to requested profile ${pClass.getName}")
    val root = config
    new DatabaseConfig[P] {
      lazy val db: P#Backend#Database =
        profile.backend.createDatabase(root, (if(path.isEmpty) "" else path + ".") + "db")
      val profile: P = untypedP.asInstanceOf[P]
      val driver: P = untypedP.asInstanceOf[P]
      lazy val config: Config = if(path.isEmpty) root else root.getConfig(path)
      def profileName = if(profileIsObject) n.substring(0, n.length-1) else n
      def profileIsObject = n.endsWith("$")
    }
  }

  /** Load a profile and database configuration from the specified URI. If only a fragment name
    * is given, it is resolved as a path in the global app config (e.g. in `application.conf` at
    * the root of the class path), otherwise as a path in the configuration located at the URI
    * without the fragment, which must be a valid URL. Without a fragment, the whole config object
    * is used. */
  def forURI[P <: BasicProfile : ClassTag](uri: URI, classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): DatabaseConfig[P] = {
    val (base, path) = {
      val f = uri.getRawFragment
      val s = uri.toString
      if(s.isEmpty) (null, "")
      else if(f eq null) (s, "")
      else if(s.startsWith("#")) (null, uri.getFragment)
      else (s.substring(0, s.length-f.length-1), uri.getFragment)
    }
    val root =
      if(base eq null) ConfigFactory.load(classLoader)
      else ConfigFactory.parseURL(new URL(base)).resolve()
    forConfig[P](path, root, classLoader)
  }

  /** Load a profile and database configuration from the URI specified in a [[StaticDatabaseConfig]]
    * annotation in the static scope of the caller. */
  def forAnnotation[P <: BasicProfile](classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader)(implicit ct: ClassTag[P]): DatabaseConfig[P] =
    macro StaticDatabaseConfigMacros.getWithClassLoaderImpl[P]

  /** Load a profile and database configuration from the URI specified in a [[StaticDatabaseConfig]]
    * annotation in the static scope of the caller. */
  def forAnnotation[P <: BasicProfile](implicit ct: ClassTag[P]): DatabaseConfig[P] =
    macro StaticDatabaseConfigMacros.getImpl[P]
}

/** An annotation for injecting a DatabaseConfig at compile time. The URI parameter must be a
  * literal String. This annotation is required for providing a statically scoped database
  * configuration to the `tsql` interpolator. */
final class StaticDatabaseConfig(val uri: String) extends Annotation with StaticAnnotation

object StaticDatabaseConfigMacros {
  private[slick] def getURI(c: Context): String = {
    import c.universe._

    def findUri(ann: Seq[c.universe.Annotation]): Option[String] =
      ann.map(a => c.typecheck(a.tree, pt = weakTypeOf[StaticDatabaseConfig], silent = true)).collectFirst {
        case Apply(Select(_, _), List(Literal(Constant(uri: String)))) => uri
      }

    val scopes = Iterator.iterate(c.internal.enclosingOwner)(_.owner).takeWhile(_ != NoSymbol)
    val uriOpt = scopes.map(s => findUri(s.annotations)).find(_.isDefined).flatten
    uriOpt.getOrElse(c.abort(c.enclosingPosition, "No @StaticDatabaseConfig annotation found in enclosing scope"))
  }

  def getImpl[P <: BasicProfile : c.WeakTypeTag](c: Context)(ct: c.Expr[ClassTag[P]]): c.Expr[DatabaseConfig[P]] = {
    import c.universe._
    val uri = c.Expr[String](Literal(Constant(getURI(c))))
    reify(DatabaseConfig.forURI[P](new URI(uri.splice))(ct.splice))
  }

  def getWithClassLoaderImpl[P <: BasicProfile : c.WeakTypeTag](c: Context)(classLoader: c.Expr[ClassLoader])(ct: c.Expr[ClassTag[P]]): c.Expr[DatabaseConfig[P]] = {
    import c.universe._
    val uri = c.Expr[String](Literal(Constant(getURI(c))))
    reify(DatabaseConfig.forURI[P](new URI(uri.splice), classLoader.splice)(ct.splice))
  }
}
