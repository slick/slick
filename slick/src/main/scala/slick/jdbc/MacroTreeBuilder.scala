package slick.jdbc

import scala.language.experimental.macros

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context

/** AST builder used by the SQL interpolation macros. */
private[jdbc] class MacroTreeBuilder[C <: Context](val c: C)(paramsList: List[C#Expr[Any]]) {
  import c.universe._

  def abort(msg: String) = c.abort(c.enclosingPosition, msg)

  // create a list of strings passed to this interpolation
  lazy val rawQueryParts: List[String] = {
    //Deconstruct macro application to determine the passed string and the actual parameters
    val Apply(Select(Apply(_, List(Apply(_, strArg))), _), paramList) = c.macroApplication
    strArg map {
      case Literal(Constant(x: String)) => x
      case _ => abort("The interpolation contained something other than constants...")
    }
  }

  /**
   * Create a Tree of the static name of a class
   * eg java.lang.String becomes
   * Select(Select(Select(Ident(nme.ROOTPKG), "java"), "lang"), "String")
   */
  private def createClassTreeFromString(classString: String, generator: String => Name): Tree = {
    val tokens = classString.split('.').toList
    val packages = tokens.dropRight(1) map (TermName(_))
    val classType = generator(tokens.last)
    val firstPackage = Ident(termNames.ROOTPKG)
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
  lazy val GetResultTypeTree = createClassTreeFromString("slick.jdbc.GetResult", TypeName(_))
  lazy val SetParameterTypeTree = createClassTreeFromString("slick.jdbc.SetParameter", TypeName(_))
  lazy val TypedStaticQueryTypeTree = createClassTreeFromString("slick.jdbc.TypedStaticQuery", TypeName(_))
  lazy val GetResultTree = createClassTreeFromString("slick.jdbc.GetResult", TermName(_))
  lazy val SetParameterTree = createClassTreeFromString("slick.jdbc.SetParameter", TermName(_))
  lazy val ImplicitlyTree = createClassTreeFromString("scala.Predef.implicitly", TermName(_))
  lazy val HeterogenousTree = createClassTreeFromString("slick.collection.heterogeneous", TermName(_))
  lazy val VectorTree = createClassTreeFromString("scala.collection.immutable.Vector", TermName(_))
  lazy val GetNoResultTree = createClassTreeFromString("slick.jdbc.TypedStaticQuery.GetNoResult", TermName(_))

  /**
   * Creates the tree for GetResult[] of the tsql macro
   */
  def rconvTree(resultTypes: Vector[ClassTag[_]]) = {
    val resultTypeTrees = resultTypes.map (_.runtimeClass.getCanonicalName match {
      case "int" => TypeTree(typeOf[Int])
      case "byte" => TypeTree(typeOf[Byte])
      case "long" => TypeTree(typeOf[Long])
      case "short" => TypeTree(typeOf[Short])
      case "float" => TypeTree(typeOf[Float])
      case "double" => TypeTree(typeOf[Double])
      case "boolean" => TypeTree(typeOf[Boolean])
      case x => TypeTree(c.mirror.staticClass(x).selfType)
    })

    resultTypes.size match {
      case 0 => implicitTree(TypeTree(typeOf[Int]) , GetResultTypeTree)
      case 1 => implicitTree(resultTypeTrees(0), GetResultTypeTree)
      case n if (n <= 22) =>
        implicitTree(AppliedTypeTree(
          Select(Select(Ident(termNames.ROOTPKG), TermName("scala")), TypeName("Tuple" + resultTypes.size)),
          resultTypeTrees.toList
        ), GetResultTypeTree)
      case n =>
        val rtypeTree = {
          val zero = TypeTree(typeOf[slick.collection.heterogeneous.syntax.HNil])
          val :: = Select(Select(HeterogenousTree, TermName("syntax")), TypeName("$colon$colon"))
          resultTypeTrees.foldRight[TypTree](zero) { (typ, prev) =>
            AppliedTypeTree(::, List(typ, prev))
          }
        }
        val zero = Select(HeterogenousTree, TermName("HNil"))
        val zipped = (0 until n) zip resultTypeTrees
        val << = Select(Ident(TermName("p")), TermName("$less$less"))
        Apply(
          TypeApply(
            Select(GetResultTree, TermName("apply")),
            List(rtypeTree)
          ),
          List(
            Function(
              List(ValDef(Modifiers(Flag.PARAM), TermName("p"), TypeTree(), EmptyTree)),
              Block(
                zipped.map { tup =>
                  val (i: Int, typ: Tree) = tup
                  ValDef(Modifiers(), TermName("gr" + i), TypeTree(), implicitTree(typ, GetResultTypeTree))
                }.toList,
                zipped.foldRight[Tree](zero) { (tup, prev) =>
                  val (i: Int, typ: Tree) = tup
                  Block(
                    List(ValDef(Modifiers(), TermName("pv" + i), TypeTree(), Apply(<<, List(Ident(TermName("gr" + i)))))),
                    Apply(Select(prev, TermName("$colon$colon")), List(Ident(TermName("pv" + i))))
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
  private lazy val interpolationResultParams: (List[Tree], Tree) = {
    def decode(s: String): (String, Boolean) = {
      if(s.endsWith("##")) {
        val (str, bool) = decode(s.substring(0, s.length-2))
        (str + "#", bool)
      } else if(s.endsWith("#")) {
        (s.substring(0, s.length-1), true)
      } else {
        (s, false)
      }
    }

    /** Fuse adjacent string literals */
    def fuse(l: List[Tree]): List[Tree] = l match {
      case Literal(Constant(s1: String)) :: Literal(Constant(s2: String)) :: ss => fuse(Literal(Constant(s1 + s2)) :: ss)
      case s :: ss => s :: fuse(ss)
      case Nil => Nil
    }

    if(rawQueryParts.length == 1)
      (List(Literal(Constant(rawQueryParts.head))), Select(SetParameterTree, TermName("SetUnit")))
    else {
      val queryString = new ListBuffer[Tree]
      val remaining = new ListBuffer[c.Expr[SetParameter[Unit]]]
      paramsList.asInstanceOf[List[c.Expr[Any]]].iterator.zip(rawQueryParts.iterator).foreach { case (param, rawQueryPart) =>
        val (queryPart, append) = decode(rawQueryPart)
        queryString.append(Literal(Constant(queryPart)))
        if(append) queryString.append(param.tree)
        else {
          queryString.append(Literal(Constant("?")))
          remaining += c.Expr[SetParameter[Unit]] {
            Apply(
              Select(
                implicitTree(TypeTree(param.actualType), SetParameterTypeTree),
                TermName("applied")
              ),
              List(param.tree)
            )
          }
        }
      }
      queryString.append(Literal(Constant(rawQueryParts.last)))
      val pconv =
        if(remaining.isEmpty) Select(SetParameterTree, TermName("SetUnit"))
        else Apply(
          Select(SetParameterTree, TermName("apply")),
          List(
            Function(
              List(
                ValDef(Modifiers(Flag.PARAM), TermName("u"), TypeTree(), EmptyTree),
                ValDef(Modifiers(Flag.PARAM), TermName("pp"), TypeTree(), EmptyTree)
              ),
              Block(
                remaining.toList map ( sp =>
                  Apply(
                    Select(sp.tree, TermName("apply")),
                    List(Ident(TermName("u")), Ident(TermName("pp")))
                  )
                ), Literal(Constant(()))
              )
            )
          )
        )
      (fuse(queryString.result()), pconv)
    }
  }

  lazy val queryParts: Tree =
    Apply(Select(VectorTree, TermName("apply")), interpolationResultParams._1)

  def staticQueryString: String = interpolationResultParams._1 match {
    case Literal(Constant(s: String)) :: Nil => s
    case _ => c.abort(c.enclosingPosition, "Only constant strings may be used after '#$' in 'tsql' interpolation")
  }

  lazy val pconvTree: Tree = interpolationResultParams._2
}
