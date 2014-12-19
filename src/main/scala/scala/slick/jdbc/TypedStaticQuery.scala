package scala.slick.jdbc

import com.typesafe.config.{ ConfigFactory, ConfigException }
import scala.annotation.{Annotation, StaticAnnotation}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.Context
import scala.slick.collection.heterogenous._

/**
 * An implementation of the macros involved in the Plain SQL API
 */
object TypedStaticQuery {
  
  /**
   * An annotation used with the tsql interpolation macro for defining
   * the configuration of compile-time database connections through a config file
   */
  final class TSQLConfig(val dbName: String) extends Annotation with StaticAnnotation

  /**
   * The function used to fetch a ConfigHandler instance that ensures
   * uniform database connections at compile-time and at run-time.
   * tsql interpolation macro must always be used in conjunction with a
   * ConfigHandler, more specifically a ConfigHandler.connection
   */
  def getConfigHandler() = macro getCHimpl

  def getCHimpl(ctxt: Context)(): ctxt.Expr[TypedStaticQuery.ConfigHandler] = {
    val macroConnHelper = new MacroConnectionHelper(ctxt) {
      override val c: ctxt.type = ctxt
    }
    ctxt.Expr(macroConnHelper.configHandlerTree)
  }
  
  /**
   * Helps macros build the output trees by constructing individual compiler trees
   * for different arguments to SQLInterpolationResult class
   */
  private[jdbc] abstract class MacroTreeBuilderHelper extends MacroTreeBuilderBase with MacroTreeBuilder
  
  /**
   * This trait encapsulates some basic functionalities needed to build compiler trees
   */
  private[jdbc] trait MacroTreeBuilderBase {
    val c: Context
    val resultTypes: List[ClassTag[_]]
    val paramsList: List[c.Expr[Any]]
    val queryParts: List[String]
    
    import c.universe._
    
    /**
     * Create a Tree of the static name of a class
     * eg java.lang.String becomes
     * Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "String")
     */
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
    
    /**
     * Creates a tree equivalent to an implicity resolution of a given type
     * eg for type GetResult[Int], this function gives the tree equivalent of
     * scala.Predef.implicitly[GetResult[Int]]
     */
    def implicitTree(reqType: Tree, baseType: Tree) = TypeApply(
      ImplicitlyTree, List(AppliedTypeTree(baseType, List(reqType)))
    )
    
    //Some commonly used trees that are created on demand
    lazy val GetResultTypeTree = createClassTreeFromString("scala.slick.jdbc.GetResult", newTypeName(_))
    lazy val SetParameterTypeTree = createClassTreeFromString("scala.slick.jdbc.SetParameter", newTypeName(_))
    lazy val TypedStaticQueryTypeTree = createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery", newTypeName(_))
    lazy val GetResultTree = createClassTreeFromString("scala.slick.jdbc.GetResult", newTermName(_))
    lazy val SetParameterTree = createClassTreeFromString("scala.slick.jdbc.SetParameter", newTermName(_))
    lazy val ImplicitlyTree = createClassTreeFromString("scala.Predef.implicitly", newTermName(_))
    lazy val HeterogenousTree = createClassTreeFromString("scala.slick.collection.heterogenous", newTermName(_))
    lazy val ArrayTree = createClassTreeFromString("scala.Array", newTermName(_))
    lazy val GetNoResultTree = createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.GetNoResult", newTermName(_))
  }
  
  /**
   * Trait that creates actual compiler trees using the
   * helper function from base trait MacroTreeBuilderBase
   */
  private[jdbc] trait MacroTreeBuilder { base: MacroTreeBuilderBase =>
    import base.c.universe._
    
    /**
     * The number of columns returned by the query that is being processed
     */
    lazy val resultCount = resultTypes.size
    
    /**
     * Converts the column types from ClassTag[] (which is returned by
     * the function meta.jdbcTypeToScala) to TypeTree
     */
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
    
    /**
     * Creates the return type of the tsql macro
     */
    lazy val rtypeTree = resultCount match {
      case 0 => TypeTree(typeOf[Int])
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
    
    /**
     * Creates the tree for GetResult[] of the tsql macro
     */
    lazy val rconvTree = resultCount match {
      case 0 => base.implicitTree(rtypeTree , base.GetResultTypeTree)
      case n if (n <= 22) => base.implicitTree(rtypeTree, base.GetResultTypeTree)
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
    
    /**
     * Processing of the query to fill in constants and prepare
     * the query to be used and a list of SetParameter[]
     */
    private lazy val interpolationResultParams = if (queryParts.length == 1) {
      (Literal(Constant(queryParts.head)), Select(base.SetParameterTree, newTermName("SetUnit")))
    } else {
      import scala.collection.mutable.ListBuffer
      val params = paramsList.iterator
      val b = new StringBuilder
      val remaining = new ListBuffer[base.c.Expr[SetParameter[Unit]]]()
      params.zip(queryParts.iterator).foreach { zipped =>
        b.append(zipped._2)
        val p = zipped._1
        p.tree match {
          case Literal(Constant(s)) => b.append(s.toString)
          case _ => {
              b.append('?')
              remaining += base.c.Expr[SetParameter[Unit]] {
                Apply(
                  Select(
                    base.implicitTree(TypeTree(p.actualType), base.SetParameterTypeTree),
                    newTermName("applied")
                  ),
                  List(p.tree)
                )
              }
          }
        }
      }
      b.append(queryParts.last)
      (Literal(Constant(b.toString)), remaining.length match {
        case 0 => Select(base.SetParameterTree, newTermName("SetUnit"))
        case _ => Apply(
          Select(base.SetParameterTree, newTermName("apply")),
          List(
            Function(
              List(
                ValDef(Modifiers(Flag.PARAM), newTermName("u"), TypeTree(), EmptyTree),
                ValDef(Modifiers(Flag.PARAM), newTermName("pp"), TypeTree(), EmptyTree)
              ),
              Block(
                remaining.toList map ( sp =>
                  Apply(
                    Select(sp.tree, newTermName("apply")),
                    List(Ident(newTermName("u")), Ident(newTermName("pp")))
                  )
                ), Literal(Constant(()))
              )
            )
          )
        )
      })
    }

    lazy val query = interpolationResultParams._1

    lazy val pconvTree = interpolationResultParams._2
  }
  
  /**
   * Helps fetch a ConfigHandler by scanning the cache or else searching
   * enclosing class and method definitions for TSQLConfig annotation and 
   * parsing it
   */
  private[jdbc] class MacroConnectionHelper(ctxt: Context) {
    val c: Context = ctxt
    import c.universe._

    def abort(msg: String) = c.abort(c.enclosingPosition, msg)
    
    // create a list of strings passed to this interpolation
    lazy val queryParts: List[String] = {
      //Deconstruct macro application to determine the passed string and the actual parameters
      val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = c.macroApplication
      strArg map {
        case Literal(Constant(x: String)) => x
        case _ => abort("The interpolation contained something other than constants...")
      }
    }
    
    /**
     * create SQL query string
     */
    lazy val rawQuery: String = queryParts.mkString("?")

    /**
     * Create a ConfigHandler Expr from a TSQLConfig object,
     * most probably retreived from an annotation in the client code
     */
    def createConfigHandler(config: TSQLConfig) = Apply(
      Select(
        New(
          createClassTreeFromString("scala.slick.jdbc.TypedStaticQuery.ConfigHandler", newTypeName(_))
        ), nme.CONSTRUCTOR),
      List(Literal(Constant(config.dbName)))
    )
    
    /**
     * Create a Tree of the static name of a class
     * eg java.lang.String becomes
     * Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "String")
     */
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
    
    //Shorthand for c.eval
    private[this] def eval[T](tree: Tree): T = c.eval(c.Expr[T](c.resetLocalAttrs(tree)))

    /**
     * Actually locates a TSQLConfig annotation and
     * creates a Tree of ConfigHandler
     */
    lazy val configHandlerTree: Tree = {

      //From a list of annotations determine the TSQLConfig annotation
      def findAnnotationTree(ann: List[Tree]): Option[Tree] = ann.flatMap { tree =>
        c.typeCheck(tree, pt = weakTypeOf[TSQLConfig], silent = true) match {
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
      val clasDef = c.enclosingClass.asInstanceOf[MemberDef]
      val methDef = Option(c.enclosingMethod).filter(_ != EmptyTree).map(_.asInstanceOf[MemberDef])

      //Determine the annotations and evaluate corresponding ConfigHandlers
      val clasConf = findAnnotationTree(clasDef.mods.annotations) map {t =>
        createConfigHandler(eval[TSQLConfig](t))
      }
      val methConf = methDef.flatMap(md => findAnnotationTree(md.mods.annotations) map {t =>
        createConfigHandler(eval[TSQLConfig](t))
      })

      methConf.getOrElse {
        clasConf.getOrElse{
          abort("Cannot find suitable config handler for this invocation")
        }
      }
    }

    /**
     * Actually locates a TSQLConfig annotation and
     * creates an instance of ConfigHandler
     */
    lazy val configHandler: ConfigHandler = eval[ConfigHandler](configHandlerTree)
  }

  /**
   * The class that is used to ensure an uniform database access mechanism
   * at the compile-time and run-time by factoring the connection parameters
   */
  final class ConfigHandler(databaseName: String) {
    
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
      Option{ _key =>
        val c = conf.getConfig(configGlobalPrefix + databaseName)
        if (c.hasPath(_key)) c.getString(_key) else null
      }
    } catch {
      case _: ConfigException.Missing => None
    }
    
    //lazy val databaseName:Option[String] = None
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
}