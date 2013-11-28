package scala.slick.typeproviders
import java.io.File
import scala.slick.driver.JdbcProfile
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.JdbcBackend
import com.typesafe.config.ConfigFactory
import scala.slick.SlickException
import com.typesafe.config.ConfigException
import scala.slick.schema.Retriever
import scala.slick.schema.naming.NamingConfigured
import scala.slick.schema.naming.MappingConfiguration

trait ConfigHandler { self: MacroHelpers =>
  import universe._
  import Flag._

  import scala.reflect.runtime.{ universe => runtimeUniverse }
  val runtimeMirror = runtimeUniverse.runtimeMirror(self.getClass.getClassLoader)

  private lazy val conf = {
    val confFile = {
      val confFileName = if (configFileName.endsWith(".conf")) configFileName else configFileName + ".conf"
      val file = new File(confFileName)
      if (file.isFile() && file.exists())
        file
      else
        throw new SlickException("Configuration file you provided does not exist")
    }
    ConfigFactory.parseFile(confFile)
  }

  lazy val naming = {
    val namingSourceKey = "naming.scala-source"
    val mapping = MappingConfiguration(conf)
    try {
      val className = conf.getString(namingSourceKey)
      val classSymbol = runtimeMirror.staticClass(className)
      val classMirror = runtimeMirror.reflectClass(classSymbol)
      val ctor = classSymbol.typeSignature.declaration(scala.reflect.runtime.universe.nme.CONSTRUCTOR).asMethod
      val ctorMirror = classMirror.reflectConstructor(ctor)
      ctorMirror(mapping).asInstanceOf[NamingConfigured]
    } catch {
      case e: ConfigException.Missing => new NamingConfigured(mapping)
      case e: ConfigException.WrongType => throw new SlickException(s"The value for $namingSourceKey should be String", e)
    }
  }

  lazy val typeMapper = {
    val typingSourceKey = "naming.type-source"
    try {
      val objectName = conf.getString(typingSourceKey)
      val module = runtimeMirror.staticModule(objectName)
      val reflectedModule = runtimeMirror.reflectModule(module)
      (reflectedModule.instance.asInstanceOf[TypeMapper], Some(objectName, module.moduleClass))
    } catch {
      case e: ConfigException.Missing => (new TypeMapper {}, None)
      case e: ConfigException.WrongType => throw new SlickException(s"The value for $typingSourceKey should be String", e)
    }

  }

  lazy val jdbcClass = getFromConfig("jdbc-driver")
  lazy val urlForConnection = getFromConfig("url")
  lazy val slickDriverObject = getFromConfig("slick-object")
  lazy val userForConnection = getFromConfig("username")
  lazy val passForConnection = getFromConfig("password")

  @inline def getFromConfig(key: String): String = try {
    conf.getString(key)
  } catch {
    case e: ConfigException.Missing => ""
    case e: ConfigException.WrongType => throw new SlickException(s"The value for $key should be String", e)
  }

  lazy val driver: JdbcDriver = {
    val conString = urlForConnection
    val module = runtimeMirror.staticModule(slickDriverObject)
    val reflectedModule = runtimeMirror.reflectModule(module)
    val driver = reflectedModule.instance.asInstanceOf[JdbcDriver]
    driver
  }
}