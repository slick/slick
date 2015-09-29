package slick.backend

import slick.util.ClassLoaderUtil

import scala.language.experimental.macros

import java.net.{URL, URI}
import scala.annotation.{StaticAnnotation, Annotation}
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context
import scala.util.control.NonFatal
import slick.SlickException
import slick.profile.BasicProfile
import com.typesafe.config.{ConfigFactory, Config}

/** A configuration for a Database plus a matching Slick driver.  */
trait DatabaseConfig[P <: BasicProfile] {
  /** Get the configured Database. It is instantiated lazily when this method is called for the
    * first time, and must be closed after use. */
  def db: P#Backend#Database

  /** The configured driver. */
  val driver: P

  /** The raw configuration. */
  def config: Config

  /** The name of the driver class or object (without a trailing "$"). */
  def driverName: String

  /** Whether the `driverName` represents an object instead of a class. */
  def driverIsObject: Boolean
}

object DatabaseConfig {
  /** Load a driver and database configuration through
    * [[https://github.com/typesafehub/config Typesafe Config]].
    *
    * The following config parameters are available:
    * <ul>
    *   <li>`driver` (String, required): The fully qualified name of a class or object which
    *   implements the specified profile. If the name ends with `$` it is assumed to be an object
    *   name, otherwise a class name.</li>
    *   <li>`db` (Config, optional): The configuration of a database for the driver's backend.
    *   For JdbcProfile-based' drivers (and thus JdbcBackend), see
    *   `JdbcBackend.DatabaseFactory.forConfig` for parameters that should be defined inside of
    *   `db`.</li>
    * </ul>
    *
    * @param path The path in the configuration file for the database configuration (e.g. `foo.bar`
    *             would find a driver name at config key `foo.bar.driver`) or an empty string
    *             for the top level of the `Config` object.
    * @param config The `Config` object to read from. This defaults to the global app config
    *               (e.g. in `application.conf` at the root of the class path) if not specified.
    * @param classLoader The ClassLoader to use to load any custom classes from. The default is to
    *                    try the context ClassLoader first and fall back to Slick's ClassLoader.
    */
  def forConfig[P <: BasicProfile : ClassTag](path: String, config: Config = ConfigFactory.load(),
                                              classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader): DatabaseConfig[P] = {
    val n = config.getString((if(path.isEmpty) "" else path + ".") + "driver")
    val untypedP = try {
      if(n.endsWith("$")) classLoader.loadClass(n).getField("MODULE$").get(null)
      else classLoader.loadClass(n).newInstance()
    } catch { case NonFatal(ex) =>
      throw new SlickException(s"""Error getting instance of Slick driver "$n"""", ex)
    }
    val pClass = implicitly[ClassTag[P]].runtimeClass
    if(!pClass.isInstance(untypedP))
      throw new SlickException(s"Configured Slick driver $n is not an instance of requested profile ${pClass.getName}")
    val root = config
    new DatabaseConfig[P] {
      lazy val db: P#Backend#Database =
        driver.backend.createDatabase(root, (if(path.isEmpty) "" else path + ".") + "db")
      val driver: P = untypedP.asInstanceOf[P]
      lazy val config: Config = if(path.isEmpty) root else root.getConfig(path)
      def driverName = if(driverIsObject) n.substring(0, n.length-1) else n
      def driverIsObject = n.endsWith("$")
    }
  }

  /** Load a driver and database configuration from the specified URI. If only a fragment name
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
      if(base eq null) ConfigFactory.load()
      else ConfigFactory.parseURL(new URL(base)).resolve()
    forConfig[P](path, root, classLoader)
  }

  /** Load a driver and database configuration from the URI specified in a [[StaticDatabaseConfig]]
    * annotation in the static scope of the caller. */
  def forAnnotation[P <: BasicProfile](classLoader: ClassLoader = ClassLoaderUtil.defaultClassLoader)(implicit ct: ClassTag[P]): DatabaseConfig[P] =
    macro StaticDatabaseConfigMacros.getWithClassLoaderImpl[P]

  /** Load a driver and database configuration from the URI specified in a [[StaticDatabaseConfig]]
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
