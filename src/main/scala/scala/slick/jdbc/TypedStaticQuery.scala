package scala.slick.jdbc

import com.typesafe.config.{ ConfigFactory, ConfigException }
import java.sql.PreparedStatement
import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.macros.TypecheckException
import scala.slick.collection.heterogenous._
import scala.slick.collection.heterogenous.syntax._

class TypedStaticQuery[+R](query: String, rconv: GetResult[R], param: List[Any], pconv: List[SetParameter[_]])
extends StatementInvoker[R] {
  def getStatement = query
  
  protected def setParam(st: PreparedStatement) = {
    val pp = new PositionedParameters(st)
    param.zip(pconv) foreach { i =>
      val (p, sp) = i
      sp.asInstanceOf[SetParameter[Any]](p, pp)
    }
  }
  protected def extractValue(rs: PositionedResult): R = rconv(rs)
}

object TypedStaticQuery {
  def tsqlImpl(ctxt: Context)(params: ctxt.Expr[Any]*): ctxt.Expr[TypedStaticQuery[Any]] = {
    import ctxt.universe._
    
    /*def typeToTree(tpe: Type): Tree = {
      def erasedTypeToTree(etpe: Type): Tree =
        createClassTreeFromString(etpe.typeSymbol.fullName)
      tpe.asInstanceOf[TypeRef].args match {
        case Nil => erasedTypeToTree(tpe.erasure)
        case args => AppliedTypeTree(erasedTypeToTree(tpe.erasure), args.map(t => typeToTree(t)))
      }
    }*/
    
    def createClassTreeFromString(classString: String, generator: String => Name = newTypeName(_)): Tree = {
      val tokens = classString.split('.').toList
      tokens match {
        case Nil => ctxt.abort(ctxt.enclosingPosition, s"No class/object name defined for $classString")
        case _ => {
          val packages = tokens.dropRight(1)
          val classType = generator(tokens.last)
          val packagesType = packages map (newTermName(_))
          if (packagesType.isEmpty)
            Ident(classType)
          else {
            val firstPackage = packages.head match {
              case "_root_" => Ident(nme.ROOTPKG)
              case _ => Ident(packagesType.head)
            }
            val others = (packagesType.tail :+ classType)
            others.foldLeft[Tree](firstPackage)((prev, elem) => {
              Select(prev, elem)
            })
          }
        }
      }
    }

    val macroHelper = new {
      val c = ctxt
    } with MacroHelpers

    macroHelper.connection withSession { implicit session =>
      val preparedStmt = session.prepareStatement(macroHelper.query)
      val resultMeta = preparedStmt.getMetaData
      val count = resultMeta.getColumnCount
      val resultTypes = List.tabulate(count) { i =>
        meta.jdbcTypeToScala(resultMeta.getColumnType(i + 1))
      }.map (_.runtimeClass.getCanonicalName match {
        case "int" => TypeTree(typeOf[Int])
        case "byte" => TypeTree(typeOf[Byte])
        case "long" => TypeTree(typeOf[Long])
        case "short" => TypeTree(typeOf[Short])
        case "float" => TypeTree(typeOf[Float])
        case "double" => TypeTree(typeOf[Double])
        case "boolean" => TypeTree(typeOf[Boolean])
        case x => TypeTree(ctxt.mirror.staticClass(x).selfType)
      })
      
      //val ptypeTree = weakTypeOf[P]
      val rtypeTree = count match {
        case 1 => resultTypes(0)
        case n if (n <= 22) => AppliedTypeTree(
          Select(Ident(newTermName("scala")), newTypeName("Tuple" + count)),
          resultTypes
        )
        case _ => {
          val syntaxTree = createClassTreeFromString("scala.slick.collection.heterogenous.syntax", newTermName(_))
          val zero = TypeTree(typeOf[HNil])
          val :: = Select(syntaxTree, newTypeName("$colon$colon"))
          resultTypes.foldRight[TypTree](zero) { (typ, prev) =>
            AppliedTypeTree(::, List(typ, prev))
          }
        }
      }
      
      def implicitTree(reqType: Tree) = TypeApply(
        Select(
          Select(Ident(newTermName("scala")), newTermName("Predef")), 
          newTermName("implicitly")
        ), 
        List(reqType)
      )
      
      def implicitGetResultTree(reqType: Tree) = implicitTree(
        AppliedTypeTree(
          createClassTreeFromString("scala.slick.jdbc.GetResult"), 
          List(reqType)
        )
      )
      
      def implicitSetParameterTree(reqType: Tree) = implicitTree(
        AppliedTypeTree(
          createClassTreeFromString("scala.slick.jdbc.SetParameter"), 
          List(reqType)
        )
      )
      
      val rconvTree = count match {
        case n if (n <= 22) => implicitGetResultTree(Ident(newTypeName("rtype")))
        case n => {
          val zero = createClassTreeFromString("scala.slick.collection.heterogenous.HNil", newTermName(_))
          val zipped = (0 until n) zip resultTypes
          val << = Select(Ident(newTermName("p")), newTermName("$less$less"))
          Apply(
            TypeApply( 
              Select(createClassTreeFromString("scala.slick.jdbc.GetResult", newTermName(_)), newTermName("apply")), 
              List(rtypeTree)
            ),
            List( 
              Function(
                List(ValDef(Modifiers(Flag.PARAM), newTermName("p"), TypeTree(), EmptyTree)),
                Block(
                  zipped.map { tup =>
                    val (i: Int, typ: Tree) = tup
                    ValDef(Modifiers(), newTermName("gr" + i), TypeTree(), implicitGetResultTree(typ))
                  }.toList,
                  zipped.foldRight(zero) { (tup, prev) =>
                    val (i: Int, typ: Tree) = tup
                    Block(
                      List(ValDef(Modifiers(), newTermName("pv" + i), TypeTree(), Apply(<<, List(Ident(newTermName("gr" + i)))))),
                      Apply(Select(prev, newTermName("$colon$colon")), List(Ident(newTermName("pv" + i))))
                    )
                  }
                )
              )
            )
          )
        }
      }
      
      val pconvTree = Apply(
        Select(Ident(newTermName("scala")), newTermName("List")), 
        params.map( p =>
          implicitSetParameterTree(TypeTree(p.actualType))
        ).toList
      )
      
      val ret = ctxt.Expr[TypedStaticQuery[Any]] {
        Block( 
          List (
            TypeDef(Modifiers(), newTypeName("rtype"), List(), rtypeTree),
            ValDef(Modifiers(), newTermName("rconv"), TypeTree(), rconvTree),
            ValDef(Modifiers(), newTermName("pconv"), TypeTree(), pconvTree)
          ),
          Apply(
            Select(
              New( AppliedTypeTree(
                createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery"), 
                List(Ident(newTypeName("rtype")))
              )), nme.CONSTRUCTOR
            ), List(
              Literal(Constant(macroHelper.query)), 
              Ident(newTermName("rconv")), 
              Apply(
                Select(Ident(newTermName("scala")), newTermName("List")),
                params.toList.map(_.tree)
              ),
              Ident(newTermName("pconv"))
            )
          )
        )
      }
      println("*[ Start Tree")
      println(ret)
      println
      println(showRaw(ret.tree))
      println("*] End Tree")
      ret
    }
  }
  
  private[this] abstract class MacroHelpers extends MacroBaseStructureHandler with ConfigHandler with MacroConnectionHandler {
    //val configFileName = "reference.conf" -- defined as final in Config Handler
    override def error(msg: String) = abort(msg)
  }

  private[this] trait MacroBaseStructureHandler {
    val c: Context
    val scope = c.enclosingClass
    val valName = "dbName"
    val defName = "tsql"
    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
  }

  private[this] trait MacroConnectionHandler { base: MacroBaseStructureHandler =>
    import c.universe._
    
    val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = c.macroApplication

    val implTree = {
      try {
        c.inferImplicitValue(typeOf[CompileTimeConnection], silent = false)
      } catch {
        case _: TypecheckException => abort(s"There should be an implicit object implementation of CompileTimeConnection trait")
      }
    } match { //Find the name of object / val we are interested in
      case Ident(x) => x
      case Select(_, x) => x
    }

    val query: String = strArg.map[String, List[String]]{ 
      case Literal(Constant(x: String)) => x
      case _ => abort("The interpolation contained something other than constants...")
    } mkString ("?")
    
    lazy val databaseName: String = {
      import base.c.universe._

      val macroTree = Apply(Select(Apply(Ident(newTermName("StringContext")), strArg), newTermName(defName)), paramList)
      val valTree = scope.collect { // Collect all blocks
        case x: Block if x.exists(_ equalsStructure macroTree) => x
      }.:+(scope).flatMap(_.collect { // Find all modules in each block
        case x: ModuleDef if (x.mods.hasFlag(Flag.IMPLICIT) && x.name.toString == showRaw(base.implTree)) => x
        case x: ValDef if (x.mods.hasFlag(Flag.IMPLICIT) && x.name.toString == showRaw(base.implTree)) => x
      }).headOption.getOrElse { // There must be just one such module
        c.abort(c.enclosingPosition, s"${implTree} must be an implicit val or implicit object with the definition in place")
      }.collect { //Find the val we are interested in
        case x: ValDef if (x.name.toString == valName) => x.rhs
      }.headOption.getOrElse { //The implicit val or object is not defined in place
        c.abort(c.enclosingPosition, s"${implTree} must be defined in place and cannot refer to predefined an object")
      }

      c.eval(c.Expr(c.resetAllAttrs(valTree.duplicate)))
    }
  }
  
  trait ConfigHandler {
    
    val databaseName: String
    final val configFileName = "reference.conf"
    final val configGlobalPrefix = "typedsql."
    def error(msg: String): Nothing = sys.error(msg)
    
    lazy val conf = {
      val confFile = {
        val file = new java.io.File(configFileName)
        if (file.isFile() && file.exists())
          file
        else
          error(s"Configuration file does not exist. Create a file: ${file.getAbsolutePath}")
      }
      ConfigFactory.parseFile(confFile)
    }

    @inline def getFromConfig(key: String): Option[String => String] = try {
      Some{ _key =>
        val c = conf.getConfig(configGlobalPrefix + key)
        if (c.hasPath(_key)) c.getString(_key) else null
      }
    } catch {
      case _: ConfigException.Missing => None
    }
    
    lazy val connection = databaseConfig match {
      case Some(config) => JdbcBackend.Database.forURL(config("url"),
          user = config("user"),
          password = config("password"),
          driver = config("driver")
        )
      case None => error(s"Configuration for compile-time database $databaseName not found")
    }
    
    lazy val databaseConfig = getFromConfig(databaseName)
  }

  trait CompileTimeConnection {
    val dbName: String
  }
}
