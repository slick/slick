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
  val runtimeMirror = scala.reflect.runtime.universe.runtimeMirror(this.getClass.getClassLoader)

  def DbImpl(c: Context)(configurationFileName: c.Expr[String]) = {
    import c.universe._

    val (jdbcClass, urlConfig, slickDriverObject, userForConnection, passForConnection, naming, typeMapper) = {
      val testDbs = "test-dbs/type-provider/conf/"
      val confFile = {
        try {
          val Expr(Literal(Constant(configFileName: String))) = configurationFileName
          val confFileName = if (configFileName.endsWith(".conf")) configFileName else configFileName + ".conf"
          val file = new File(confFileName)
          if (file.isFile() && file.exists())
            file
          else {
            val newFile = new File(testDbs + confFileName)
            if (newFile.isFile() && newFile.exists())
              newFile
            else
              throw new SlickException("Configuration file you provided does not exist")
          }
        } catch {
          case e: MatchError => throw new SlickException("You have to provide the config file name as literal", e)
        }
      }
      val conf = ConfigFactory.parseFile(confFile)
      @inline def c(key: String): String = try {
        conf.getString(key)
      } catch {
        case e: ConfigException.Missing => ""
        case e: ConfigException.WrongType => throw new SlickException(s"The value for $key should be String", e)
      }
      val namingSourceKey = "naming.scala-source"
      val mapping = MappingConfiguration(conf)
      val naming = {
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

      val typingSourceKey = "naming.type-source"
      val typeMapper = {
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
      (c("jdbc-driver"), c("url"), c("slick-object"), c("username"), c("password"), naming, typeMapper)
    }

    val connectionString: String = {
      urlConfig
    }

    val macroHelper = new {
      val context: c.type = c
    } with MacroHelpers(naming)

    lazy val driver: JdbcDriver = {
      val conString = connectionString
      val module = runtimeMirror.staticModule(slickDriverObject)
      val reflectedModule = runtimeMirror.reflectModule(module)
      val driver = reflectedModule.instance.asInstanceOf[JdbcDriver]
      driver
    }

    def generateCodeForTables(): List[Tree] = {
      val db = driver.simple.Database.forURL(connectionString, driver = jdbcClass,
        user = userForConnection, password = passForConnection)
      val tables = Retriever.tables(driver, db, c.universe)(naming)
      tables.flatMap(table => {
        // generate the dto case class
        val tableType = typeMapper._1.tableType(c.universe)(table.name) match {
          case None => macroHelper.tableToCaseClass(table)
          case Some(tpe) => macroHelper.tableToType(table)(tpe)
        }
        // extractor!
        val tableTypeVal = typeMapper._1.tableExtractor(c.universe)(table.name) match {
          case None => Nil
          case Some(obj) => List(macroHelper.tableToTypeVal(table)(obj, tableType.name))
        }
        // generate the table object
        val tableModule = macroHelper.tableToModule(table)

        tableTypeVal ++ List(tableType, tableModule)
      })
    }

    def implicitMembersNameOfType(tpe: scala.reflect.runtime.universe.Type): List[String] = {
      val members = tpe.members.toList
      val implicitMembers = members.filter(_.isImplicit)
      implicitMembers.map(m => m.name.decoded.trim)
    }

    def implicitMembersName[T <: AnyRef](obj: T)(implicit ttag: TypeTag[obj.type]): List[String] = {
      implicitMembersNameOfType(runtimeUniverse.typeOf[obj.type])
    }

    val slickDriverTree = macroHelper.createObjectFromString(slickDriverObject)
    val slickDriverExpr = c.Expr[JdbcDriver](slickDriverTree)
    val databaseTree = macroHelper.createObjectFromString(s"_root_.$slickDriverObject.simple.Database")
    val databaseExpr = c.Expr[JdbcBackend#DatabaseFactoryDef](databaseTree)
    val importSimpleWild = macroHelper.createImport(macroHelper.createObjectFromString(s"_root_.$slickDriverObject.simple"), Nil)
    val importTypeMapper = typeMapper match {
      case (_, None) => Nil
      case (typeMapperObject, Some((typeMapperName, typeMapperClass))) => {
        val typeMapperImplicits = implicitMembersNameOfType(typeMapperClass.typeSignature)
        def implicitNameConvertor(implicitName: String) = s"typeMapperObject_$implicitName"
        val importTypeMapper = macroHelper.createImport(macroHelper.createObjectFromString(typeMapperName), typeMapperImplicits, implicitNameConvertor)
        List(importTypeMapper)
      }
    }
    val imports = List(importSimpleWild) ++ importTypeMapper

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

    c.introduceTopLevel(packageName, ClassDef(NoMods, className, Nil, Template(parents, self, imports ++ body ++ generateCodeForTables())))
  }
}
