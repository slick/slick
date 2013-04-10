package scala.slick.typeproviders

import scala.reflect.macros.Context
import java.sql._
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

object Macros {
  import scala.reflect.runtime.{ universe => runtimeUniverse }
  
  def DbImpl(c: Context)(configurationFileName: c.Expr[String]) = {
    import c.universe._
    val configFileName: String =
      try {
        val Expr(Literal(Constant(configFileName: String))) = configurationFileName
        configFileName
      } catch {
        case e: MatchError => throw new SlickException("You have to provide the config file name as literal", e)
      }

    val macroHelper = new {
      val universe: c.universe.type = c.universe
    } with MacroHelpers(c, configFileName)

    val tableTrees = macroHelper.generateTreeForTables
    val imports = macroHelper.getImports
    val connectionString = macroHelper.urlForConnection
    import macroHelper.{ userForConnection, passForConnection, slickDriverObject, jdbcClass }

    val slickDriverTree = macroHelper.createObjectFromString(slickDriverObject)
    val slickDriverExpr = c.Expr[JdbcDriver](slickDriverTree)
    val databaseTree = macroHelper.createObjectFromString(s"_root_.$slickDriverObject.simple.Database")
    val databaseExpr = c.Expr[JdbcBackend#DatabaseFactoryDef](databaseTree)

    val completeExpr = reify {
      class CONTAINER {
        // import statements will be spliced here
        val driver = slickDriverExpr.splice
        val database = databaseExpr.splice.forURL(c.literal(connectionString).splice, driver = c.literal(jdbcClass).splice,
          user = c.literal(userForConnection).splice, password = c.literal(passForConnection).splice)
        // generated code will be spliced here
      }
    }
    val Expr(Block(List(ClassDef(_, containerType, _, Template(parents, self, body))), _)) = completeExpr

    val packageName = c.enclosingPackage.pid.toString
    val className = c.freshName(c.enclosingImpl.name).toTypeName

    c.introduceTopLevel(packageName, ClassDef(NoMods, className, Nil, Template(parents, self, imports ++ body ++ tableTrees)))
  }
}
