package scala.slick.jdbc

import com.typesafe.config.{ ConfigFactory, ConfigException }
import java.sql.PreparedStatement
import scala.annotation.{Annotation, StaticAnnotation}
import scala.collection.mutable.{Map => MutableMap}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.slick.SlickException
import scala.slick.collection.heterogenous._

class TypedStaticQuery[+R](query: String, rconv: GetResult[R], param: List[Any], pconv: List[SetParameter[_]])
extends StatementInvoker[R] {
  def getStatement = query
  
  protected def setParam(st: PreparedStatement) = {
    import scala.language.existentials
    val pp = new PositionedParameters(st)
    param.zip(pconv) foreach { i =>
      val (p, sp) = i
      sp.asInstanceOf[SetParameter[Any]](p, pp)
    }
  }

  protected def extractValue(rs: PositionedResult): R = rconv(rs)
  
  def as[T](f: R => T) = new TypedStaticQuery(query, rconv.andThen(f), param, pconv)
}

object TypedStaticQuery {
  def tsqlImpl(ctxt: Context)(params: ctxt.Expr[Any]*): ctxt.Expr[TypedStaticQuery[Any]] = {
    import ctxt.universe._
    
    val macroConnHelper = new {
      val c = ctxt
    } with MacroConnectionHelper
    
    macroConnHelper.configHandler.connection withSession { session =>
      
      val rTypes = session.withPreparedStatement(macroConnHelper.query) { preparedStmt =>
        val resultMeta = preparedStmt.getMetaData
        List.tabulate(resultMeta.getColumnCount) { i =>
          meta.jdbcTypeToScala(resultMeta.getColumnType(i + 1))
        }
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
    }
  }
  
  final class TSQLConfig(dbName: String = null, url: String = null, user: String = null,
                         pass: String = null, driver: String = null, slickDriver: String = null)
  extends Annotation with StaticAnnotation {
    val dbNameOption      = Option(dbName)
    val urlOption         = Option(url)
    val userOption        = Option(user)
    val passOption        = Option(pass)
    val jdbcdriverOption  = Option(driver)
    val slickDriverOption = Option(slickDriver)
  }

  def getConfigHandler() = macro getCHimpl

  def getCHimpl(ctxt: Context)(): ctxt.Expr[TypedStaticQuery.ConfigHandler] = {
    val macroConnHelper = new {
      val c: ctxt.type = ctxt
    } with MacroConnectionHelper

    val ret = macroConnHelper.createConfigHandler(macroConnHelper.configHandler)

    println("Happy Birthday")
    println("*[ Start Tree")
    println(ret)
    println
    println(ctxt.universe.showRaw(ret))
    println("*] End Tree")
    ctxt.Expr(ret)
  }

  object GetNoResult extends GetResult[Nothing] {
    def apply(pr: PositionedResult) = throw new SlickException("The result type of query could not be determined. You must call the 'as' before executing statement.")
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
    lazy val GetNoResultTree = createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.GetNoResult", newTermName(_))
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
      case 0 => TypeTree(typeOf[Nothing])
      case 1 => resultTypeTreeList(0)
      case n if (n <= 22) => AppliedTypeTree(
        Select(Select(Ident(nme.ROOTPKG), newTermName("scala")), newTypeName("Tuple" + resultCount)),
        resultTypeTreeList
      )
      case _ => {
        val zero = TypeTree(typeOf[scala.slick.collection.heterogenous.syntax.HNil])
        val :: = Select(Select(base.HeterogenousTree, newTermName("syntax")), newTypeName("$colon$colon"))
        resultTypeTreeList.foldRight[TypTree](zero) { (typ, prev) =>
          AppliedTypeTree(::, List(typ, prev))
        }
      }
    }
    
    lazy val rconvTree = resultCount match {
      case 0 => base.GetNoResultTree
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
  
  private[this] abstract class MacroConnectionHelper extends MacroConnectionHandlerBase with MacroConnectionHandler

  private[this] trait MacroConnectionHandlerBase {
    val c: Context
    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
  }

  private[this] trait MacroConnectionHandler  { base: MacroConnectionHandlerBase =>
    import base.c.universe._
    
    //create SQL query string 
    lazy val query: String = {
      //Deconstruct macro application to determine the passed string and the actual parameters
      val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = base.c.macroApplication
      strArg.map[String, List[String]]{
        case Literal(Constant(x: String)) => x
        case _ => base.abort("The interpolation contained something other than constants...")
      } mkString ("?")
    }
    
    //Create a ConfigHandler Expr from a TSQLConfig Expr
    def createConfigHandler(config: TSQLConfig) = {
      @inline def valdef[T](name: String, opt: Option[T]) = opt map { t =>
        ValDef(Modifiers(Flag.OVERRIDE | Flag.LAZY), newTermName(name), TypeTree(),
          Apply(
            Select(createClassTreeFromString("scala.Option", newTermName(_)), newTermName("apply")),
            List(Literal(Constant(t)))
          )
        )
      }
      Block(
        List(
          ClassDef(Modifiers(Flag.FINAL), newTypeName("$anon"), List(),
            Template(
              List(createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.ConfigHandler", newTypeName(_))),
              emptyValDef,
              List(
                DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
                  Block(
                    List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
                    Literal(Constant(()))
                  )
                )
              ) ++
              valdef("databaseName", config.dbNameOption) ++
              valdef("jdbcDriver",   config.jdbcdriverOption) ++
              valdef("url",          config.urlOption) ++
              valdef("user",         config.userOption) ++
              valdef("password",     config.passOption) ++
              valdef("slickDriver",  config.slickDriverOption)
            )
          )
        ),
        Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())
      )
    }

    //Create a ConfigHandler Expr from an existing ConfigHandler instance
    def createConfigHandler(config: ConfigHandler) = {
      @inline def valdef[T](name: String, opt: Option[T]) = opt map { t =>
        ValDef(Modifiers(Flag.OVERRIDE | Flag.LAZY), newTermName(name), TypeTree(),
          Apply(
            Select(createClassTreeFromString("scala.Option", newTermName(_)), newTermName("apply")),
            List(Literal(Constant(t)))
          )
        )
      }
      Block(
        List(
          ClassDef(Modifiers(Flag.FINAL), newTypeName("$anon"), List(),
            Template(
              List(createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.ConfigHandler", newTypeName(_))),
              emptyValDef,
              List(
                DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
                  Block(
                    List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())),
                    Literal(Constant(()))
                  )
                )
              ) ++
              valdef("databaseName", config.databaseName) ++
              valdef("jdbcDriver",   config.jdbcDriver) ++
              valdef("url",          config.url) ++
              valdef("user",         config.user) ++
              valdef("password",     config.password) ++
              valdef("slickDriver",  config.slickDriver)
            )
          )
        ),
        Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())
      )
    }

    //convert a TSQLConfig tree to a ConfigHandler object
    def fix(tree: Tree): Tree = {
      //Work around for a problem -
      // Reflective compilation failed : Type not found : TSQLConfig
      //when you try to c.eval( tree )
      val Apply(Select(_, _), args) = tree
      val realTree = Apply(
        Select(
          New(createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.TSQLConfig", newTypeName(_))),
          nme.CONSTRUCTOR
        ), args map (_.duplicate)
      )
      realTree
    }
    
    def createClassTreeFromString(classString: String, generator: String => Name): Tree = {
      val tokens = classString.split('.').toList
      val packages = tokens.dropRight(1) map (newTermName(_))
      val classType = generator(tokens.last)
      val firstPackage = Ident(nme.ROOTPKG)
      val others = (packages :+ classType)
      others.foldLeft[Tree](firstPackage)((prev, elem) => {
        Select(prev, elem)
      })
    }
    
    lazy val configHandler: ConfigHandler = {

      //Shorthand for c.eval
      def eval[T](tree: Tree): T = base.c.eval(base.c.Expr[T](base.c.resetLocalAttrs(tree)))

      //Convert a given Name to its fully qualified name in String format
      def completeNameOf(name: Name): String = base.c.typeCheck(
        Typed(
          Select(Select(Ident(newTermName("scala")), newTermName("Predef")), newTermName("$qmark$qmark$qmark")),
          Ident(newTypeName(name.toString))
        )
      ).tpe.typeSymbol.fullName

      //From a list of annotations determine the TSQLConfig annotation
      def findAnnotationTree(ann: List[Tree]): Option[Tree] = ann.flatMap {
        tree =>
        base.c.typeCheck(tree, pt = weakTypeOf[TSQLConfig], silent = true) match {
          case EmptyTree => None
          case _ => {
            val Apply(Select(_, _), args) = tree
            val realTree = Apply(
              Select(
                New(createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.TSQLConfig", newTypeName(_))),
                nme.CONSTRUCTOR
              ), args map (_.duplicate)
            )
            Some(realTree)
          }
        }
      }.headOption

      //Determine the trees
      val clasDef = base.c.enclosingClass.asInstanceOf[MemberDef]
      val methDef = base.c.enclosingMethod.asInstanceOf[MemberDef]

      //Determine their names
      val clasName = completeNameOf(clasDef.name)
      val methName = clasName + "." + methDef.name

      //Determine the annotations and evaluate corresponding ConfigHandlers
      val clasConf = findAnnotationTree(clasDef.mods.annotations) map {t =>
        eval[ConfigHandler](createConfigHandler(eval[TSQLConfig](fix(t))))
      }
      val methConf = findAnnotationTree(methDef.mods.annotations) map {t =>
        eval[ConfigHandler](createConfigHandler(eval[TSQLConfig](fix(t))))
      }

      val map = Map.empty[String, ConfigHandler] ++
        clasConf.map(("." + clasName) -> _) ++
        methConf.map(("." + methName) -> _)
      cache.getFromCache(methName.split('.').toList, new StringBuffer(methName.length), map,
                         base.abort("Cannot find suitable config handler for this invocation")
      )
    }
  }

  trait ConfigHandler {
    
    final val configFileName = "reference.conf"
    final val configGlobalPrefix = "typedsql."
    
    private[TypedStaticQuery] final def connectionParameter[T](value: Option[T],
            name: String): T = value match {
      case Some(x) => x
      case None => error(s"Configuration for essential parameter ${name} not found")
    }

    def error(msg: String): Nothing = sys.error(msg)

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
    
    lazy val databaseName:Option[String] = None
    lazy val url         :Option[String] = databaseConfig.map(_.apply("url"))
    lazy val user        :Option[String] = databaseConfig.map(_.apply("user"))
    lazy val password    :Option[String] = databaseConfig.map(_.apply("password"))
    lazy val jdbcDriver  :Option[String] = databaseConfig.map(_.apply("jdbcDriver"))
    lazy val slickDriver :Option[String] = databaseConfig.map(_.apply("slickDriver"))
    
    lazy final val connection = JdbcBackend.Database.forURL(connectionParameter(url, "url"), 
      user     = user getOrElse null,
      password = password getOrElse null,
      driver   = connectionParameter(jdbcDriver, "driver")
    )

    lazy final val JdbcDriver = Class.forName(connectionParameter(jdbcDriver, "jdbcDriver")).asInstanceOf[java.sql.Driver]
    
    lazy final val SlickDriver = Class.forName(connectionParameter(slickDriver, "slickDriver")).asInstanceOf[scala.slick.driver.JdbcDriver]

  }

  //Cache implementation
  private[this] class HierarchalCache[T](val ident: String) {
    val subCaches = MutableMap[String, HierarchalCache[T]]()
    var config: Option[T] = None

    def getFromCache(path: List[String], prev: StringBuffer, map: Map[String, T], default: => T): T = {
      config = config orElse map.get(prev.toString + ident)
      path match {
        case x :: xs => subCaches.getOrElseUpdate(x, new HierarchalCache[T](x)).
          getFromCache(xs, prev.append(ident + "."), map, config.getOrElse(default))
        case Nil => config.getOrElse(default)
      }
    }
  }

  private[this] val cache = new HierarchalCache[ConfigHandler]("")
}
