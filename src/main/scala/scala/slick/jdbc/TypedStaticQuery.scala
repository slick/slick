package scala.slick.jdbc

import scala.language.experimental.macros
import scala.reflect.macros.Context
import com.typesafe.config.{ ConfigFactory, ConfigException }

abstract class TypedStaticQuery[-P, +R](query: String, rconv: GetResult[R], pconv: SetParameter[P])
  extends StaticQuery[P, R](query, pconv, rconv) {
  //    protected[this] type Self = TypedStaticQuery[P, R]
}

trait CompileTimeConnection {
  val dbName: String
}

object TypedStaticQuery {
  implicit class TypedSQLInterpolator(ctxt: StringContext) {
    def tsql(params: Any*) = macro tsqlImpl
  }

  def tsqlImpl(ctxt: Context)(params: ctxt.Expr[Any]*): ctxt.Expr[TypedStaticQuery[_, _]] = {
    import ctxt.universe._

    val macroHelper = new {
      val c = ctxt
    } with MacroHelpers

    macroHelper.connection withSession { implicit session =>
      val preparedStmt = session.prepareStatement(macroHelper.query)
      val resultMeta = preparedStmt.getMetaData
      val resultTypes = List.tabulate(resultMeta.getColumnCount) { i =>
        meta.jdbcTypeToScala(resultMeta.getColumnType(i))
      }

    }

    ctxt.Expr[TypedStaticQuery[Int, Int]] {
      Block()
    }
  }
  
  private[this] abstract class MacroHelpers extends MacroBaseStructureHandler with MacroConfigHandler with MacroConnectionHandler 

  private[this] trait MacroBaseStructureHandler {

    val c: Context

    import c.universe._

    val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = c.macroApplication

    val Ident(implTree) = c.inferImplicitValue(typeOf[CompileTimeConnection], silent = false)

    val query = strArg.map { tree =>
      c.eval(c.Expr(c.resetAllAttrs(tree.duplicate)))
    } mkString (" ? ")

    def abort(msg: String) = c.abort(c.enclosingPosition, msg)

  }

  private[this] trait MacroConfigHandler { base: MacroBaseStructureHandler =>

    val configFileName = "Some Random Shit"
    val configGlobalPrefix = "typedsql."

    lazy val conf = {
      val confFile = {
        val file = new java.io.File(configFileName)
        if (file.isFile() && file.exists())
          file
        else
          base.abort("Configuration file you provided does not exist")
      }
      ConfigFactory.parseFile(confFile)
    }

    @inline def getFromConfig(key: String): String = try {
      conf.getString(configGlobalPrefix + key)
    } catch {
      case e: ConfigException.Missing   => null
      case e: ConfigException.WrongType => base.abort(s"The value for $key should be String in the configuration file.")
    }
  }

  private[this] trait MacroConnectionHandler { base: MacroBaseStructureHandler with MacroConfigHandler =>

    val scope = base.c.enclosingClass
    val valName = "dbName"

    lazy val connection = JdbcBackend.Database.forURL(base.getFromConfig(databaseName + ".url"),
      user = base.getFromConfig(databaseName + ".user"),
      password = base.getFromConfig(databaseName + ".password"),
      driver = base.getFromConfig(databaseName + ".driver"))

    lazy val databaseName: String = {
      import base.c.universe._

      val macroTree = Apply(Select(Apply(Ident(newTermName("StringContext")), strArg), newTermName("func")), paramList)

      val valTree = scope.collect { // Collect all blocks
        case x: Block if x.exists(_ equalsStructure macroTree) => x
      }.flatMap(_.collect { // Find all modules in each block
        case x: ModuleDef if (x.mods.hasFlag(Flag.IMPLICIT) && x.name.toString == showRaw(base.implTree)) => x
      }).headOption.getOrElse { // There must be just one such module
        c.abort(c.enclosingPosition, s"${showRaw(implTree)} must be an object with constant definitions")
      }.collect { //Find the val we are interested in
        case x: ValDef if (x.name.toString == valName) => x.rhs
      }.head

      c.eval(c.Expr(c.resetAllAttrs(valTree.duplicate)))
    }
  }
}
