package scala.slick.jdbc

import com.typesafe.config.{ ConfigFactory, ConfigException }
import java.sql.PreparedStatement
import scala.language.experimental.macros
import scala.reflect.ClassTag
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
    
    val macroConnHelper = new {
      val c = ctxt
    } with MacroConnectionHelper

    macroConnHelper.connection withSession (_.withPreparedStatement(macroConnHelper.query) { implicit preparedStmt =>
      val resultMeta = preparedStmt.getMetaData
      val rTypes = List.tabulate(resultMeta.getColumnCount) { i =>
        meta.jdbcTypeToScala(resultMeta.getColumnType(i + 1))
      }
      
      val macroTreeBuilder = new {
        val c: ctxt.type = ctxt
        val resultTypes = rTypes
        val paramsList = params.toList
      } with MacroTreeBuilderHelper
      
      val ret = ctxt.Expr[TypedStaticQuery[Any]] {
        Block( 
          List (
            TypeDef(Modifiers(), newTypeName("rtype"), List(), macroTreeBuilder.rtypeTree),
            ValDef(Modifiers(), newTermName("rconv"), TypeTree(), macroTreeBuilder.rconvTree),
            ValDef(Modifiers(), newTermName("pconv"), TypeTree(), macroTreeBuilder.pconvTree)
          ),
          Apply(
            Select(
              New( AppliedTypeTree(
                macroTreeBuilder.TypedStaticQueryTypeTree, 
                List(Ident(newTypeName("rtype")))
              )), nme.CONSTRUCTOR
            ), List(
              Literal(Constant(macroConnHelper.query)), 
              Ident(newTermName("rconv")), 
              macroTreeBuilder.pListTree,
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
    })
  }
  
  private[this] abstract class MacroTreeBuilderHelper extends MacroTreeBuilderBase with MacroTreeBuilder 
  
  private[this] trait MacroTreeBuilderBase {
    val c: Context
    val resultTypes: List[ClassTag[_]]
    val paramsList: List[c.Expr[Any]]
    
    import c.universe._
    
    private def createClassTreeFromString(classString: String, generator: String => Name): Tree = {
      val tokens = classString.split('.').toList
      val packages = tokens.dropRight(1) map (newTermName(_))
      val classType = generator(tokens.last)
      val firstPackage = Ident(nme.ROOTPKG)
      val others = (packages :+ classType)
      others.foldLeft[Tree](firstPackage)((prev, elem) => {
        Select(prev, elem)
      })
    }
    
    def implicitTree(reqType: Tree, baseType: Tree) = TypeApply(
      ImplicitlyTree, List(AppliedTypeTree(baseType, List(reqType)))
    )
    
    lazy val GetResultTypeTree = createClassTreeFromString("scala.slick.jdbc.GetResult", newTypeName(_))
    lazy val SetParameterTypeTree = createClassTreeFromString("scala.slick.jdbc.SetParameter", newTypeName(_))
    lazy val TypedStaticQueryTypeTree = createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery", newTypeName(_))
    lazy val GetResultTree = createClassTreeFromString("scala.slick.jdbc.GetResult", newTermName(_))
    lazy val ImplicitlyTree = createClassTreeFromString("scala.Predef.implicitly", newTermName(_))
    lazy val HeterogenousTree = createClassTreeFromString("scala.slick.collection.heterogenous", newTermName(_))
    lazy val ListTree = createClassTreeFromString("scala.List", newTermName(_))
  }
  
  private[this] trait MacroTreeBuilder { base: MacroTreeBuilderBase => 
    import base.c.universe._
    
    lazy val resultCount = resultTypes.size
    
    lazy val resultTypeTreeList = resultTypes.map (_.runtimeClass.getCanonicalName match {
      case "int" => TypeTree(typeOf[Int])
      case "byte" => TypeTree(typeOf[Byte])
      case "long" => TypeTree(typeOf[Long])
      case "short" => TypeTree(typeOf[Short])
      case "float" => TypeTree(typeOf[Float])
      case "double" => TypeTree(typeOf[Double])
      case "boolean" => TypeTree(typeOf[Boolean])
      case x => TypeTree(base.c.mirror.staticClass(x).selfType)
    })
    
    lazy val rtypeTree = resultCount match {
      case 1 => resultTypeTreeList(0)
      case n if (n <= 22) => AppliedTypeTree(
        Select(Ident(newTermName("scala")), newTypeName("Tuple" + resultCount)),
        resultTypeTreeList
      )
      case _ => {
        val zero = TypeTree(typeOf[HNil])
        val :: = Select(Select(base.HeterogenousTree, newTermName("syntax")), newTypeName("$colon$colon"))
        resultTypeTreeList.foldRight[TypTree](zero) { (typ, prev) =>
          AppliedTypeTree(::, List(typ, prev))
        }
      }
    }
    
    lazy val rconvTree = resultCount match {
      case n if (n <= 22) => base.implicitTree(Ident(newTypeName("rtype")), base.GetResultTypeTree)
      case n => {
        val zero = Select(base.HeterogenousTree, newTermName("HNil"))
        val zipped = (0 until n) zip resultTypeTreeList
        val << = Select(Ident(newTermName("p")), newTermName("$less$less"))
        Apply(
          TypeApply( 
            Select(base.GetResultTree, newTermName("apply")), 
            List(rtypeTree)
          ),
          List( 
            Function(
              List(ValDef(Modifiers(Flag.PARAM), newTermName("p"), TypeTree(), EmptyTree)),
              Block(
                zipped.map { tup =>
                  val (i: Int, typ: Tree) = tup
                  ValDef(Modifiers(), newTermName("gr" + i), TypeTree(), base.implicitTree(typ, base.GetResultTypeTree))
                }.toList,
                zipped.foldRight[Tree](zero) { (tup, prev) =>
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
    
    lazy val pListTree = Apply(base.ListTree, paramsList.toList.map(_.tree))

    lazy val pconvTree = Apply(
      base.ListTree, 
      base.paramsList.map( p =>
        base.implicitTree(TypeTree(p.actualType), base.SetParameterTypeTree)
      ).toList
    )
  }
  
  private[this] abstract class MacroConnectionHelper extends MacroConnectionHandlerBase with ConfigHandler with MacroConnectionHandler {
    //val configFileName = "reference.conf" -- defined as final in Config Handler
    override def error(msg: String) = abort(msg)
  }

  private[this] trait MacroConnectionHandlerBase {
    val c: Context
    val scope = c.enclosingClass
    val defName = "tsql"
    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
  }

  private[TypedStaticQuery] trait ConnectionParameters {
    def databaseName:Option[String] = None
    def url         :Option[String] = None
    def user        :Option[String] = None
    def password    :Option[String] = None
    def jdbcDriver  :Option[String] = None
    def slickDriver :Option[String] = None
    
    private[TypedStaticQuery] final def connectionParameter[T](value: Option[T], 
            name: String): T = value match {
      case Some(x) => x
      case None => error(s"Configuration for essential parameter ${name} not found")
    }
    
    private[TypedStaticQuery] final def connectionParameter[T](value: Option[T], 
            default: Option[T]): T = value.orElse(default).get
    
    def error(msg: String): Nothing = sys.error(msg)
  }
  
  private[this] trait MacroConnectionHandler extends ConnectionParameters { base: MacroConnectionHandlerBase =>
    import base.c.universe._
    
    //Deconstruct macro application to determine the passed string and the actual parameters
    val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = base.c.macroApplication

    //create SQL query string 
    val query: String = strArg.map[String, List[String]]{ 
      case Literal(Constant(x: String)) => x
      case _ => base.abort("The interpolation contained something other than constants...")
    } mkString ("?")
    
    //local name of the implicit ConfigHandler implementation
    val moduleName = { 
      try {
        base.c.inferImplicitValue(typeOf[ConfigHandler], silent = false)
      } catch {
        case _: TypecheckException => abort("There should be an implicit object implementation of ConfigHandler trait")
      }
    } match { //Find the name of object / val we are interested in
      case Ident(x) => x      //When defined locally
      case Select(_, x) => x  //When defined globally
    }

    //tree of the implicit ConfigHandler implementation
    lazy val moduleTree = { 
      val macroTree = Apply(Select(Apply(Ident(newTermName("StringContext")), strArg), newTermName(defName)), paramList)
      scope.collect { // Collect all blocks
        case x: Block if x.exists(_ equalsStructure macroTree) => x
      }.:+(scope).flatMap(_.collect { // Find all modules in each block matching the name we found implicitly
        case x: ModuleDef if (x.mods.hasFlag(Flag.IMPLICIT) && x.name.toString == showRaw(moduleName)) => x
        case x: ValDef if (x.mods.hasFlag(Flag.IMPLICIT) && x.name.toString == showRaw(moduleName)) => x
      }).headOption.getOrElse { // There must be just one such module
        base.abort(s"${moduleName} must be an implicit val or implicit object with the definition in place")
      }
    }
    
    //Find the val we are interested in
    def findInTree[T](valName: String): Option[T] = moduleTree.collect { 
      case x: ValDef if (x.name.toString == valName) => x.rhs
      case x: DefDef if (x.name.toString == valName) => x.rhs
    }.headOption.map { valTree =>
      base.c.eval[Some[T]](base.c.Expr(base.c.resetAllAttrs(valTree.duplicate))).get
    }
    
    override lazy val databaseName: Option[String] = findInTree("databaseName")
    override lazy val url          :Option[String] = findInTree("url")         .orElse(super.url)
    override lazy val user         :Option[String] = findInTree("user")        .orElse(super.user)
    override lazy val password     :Option[String] = findInTree("password")    .orElse(super.password)
    override lazy val jdbcDriver   :Option[String] = findInTree("jdbcDriver")  .orElse(super.jdbcDriver)
    override lazy val slickDriver  :Option[String] = findInTree("slickDriver") .orElse(super.slickDriver)
    
  }
  
  trait ConfigHandler extends ConnectionParameters {
    
    final val configFileName = "reference.conf"
    final val configGlobalPrefix = "typedsql."
    
    lazy private[this] val conf = {
      val confFile = {
        val file = new java.io.File(configFileName)
        if (file.isFile() && file.exists())
          file
        else
          error(s"Configuration file does not exist. Create a file: ${file.getAbsolutePath}")
      }
      ConfigFactory.parseFile(confFile)
//      ConfigFactory.load()
    }

    lazy private[this] val databaseConfig: Option[String => String] = try {
      databaseName.map { dbName => _key =>
        val c = conf.getConfig(configGlobalPrefix + dbName)
        if (c.hasPath(_key)) c.getString(_key) else null
      }
    } catch {
      case _: ConfigException.Missing => None
    }
    
    override def url         :Option[String] = databaseConfig.map(_.apply("url"))         .orElse(super.url)
    override def user        :Option[String] = databaseConfig.map(_.apply("user"))        .orElse(super.user)
    override def password    :Option[String] = databaseConfig.map(_.apply("password"))    .orElse(super.password)
    override def jdbcDriver  :Option[String] = databaseConfig.map(_.apply("jdbcDriver"))  .orElse(super.jdbcDriver)
    override def slickDriver :Option[String] = databaseConfig.map(_.apply("slickDriver")) .orElse(super.slickDriver)
    
    lazy final val connection = JdbcBackend.Database.forURL(connectionParameter(url, "url"), 
      user     = connectionParameter(user, Some(null)),
      password = connectionParameter(password, Some(null)),
      driver   = connectionParameter(jdbcDriver, "driver")
    )

    lazy final val JdbcDriver = Class.forName(connectionParameter(jdbcDriver, "jdbcDriver")).asInstanceOf[java.sql.Driver]
    
    lazy final val SlickDriver = Class.forName(connectionParameter(slickDriver, "slickDriver")).asInstanceOf[scala.slick.driver.JdbcDriver]

  }
}
