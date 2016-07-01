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
    strArg map { str =>
      evalString(str).getOrElse(abort("The interpolation contained something other than constants..."))
    }
  }

  /**
    * Tries to evaluate the given tree to a String literal known at compile-time
    */
  private def evalString(tree: Tree): Option[String] = tree match {
    case Literal(Constant(x: String)) => Some(x)
    case Literal(Constant(x)) => Some(String.valueOf(x))
    case other => try {
      val x1 = c.untypecheck(q"String.valueOf(${other.duplicate})")
      Some(c.eval(c.Expr[String](x1)))
    } catch {
      case _ : Throwable => None
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

  //Some commonly used trees that are created on demand
  lazy val GetResultTypeTree = createClassTreeFromString("slick.jdbc.GetResult", TypeName(_))
  lazy val GetResultTree = createClassTreeFromString("slick.jdbc.GetResult", TermName(_))
  lazy val HeterogenousTree = createClassTreeFromString("slick.collection.heterogeneous", TermName(_))

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
      case 0 => q"scala.Predef.implicitly[slick.jdbc.GetResult[Int]]"
      case 1 => q"scala.Predef.implicitly[slick.jdbc.GetResult[${resultTypeTrees(0)}]]"
      case n if (n <= 22) => q"scala.Predef.implicitly[slick.jdbc.GetResult[(..$resultTypeTrees)]]"
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
                zipped.map { case (i: Int, typ: Tree) =>
                  val termName = TermName("gr" + i)
                  q"val $termName = scala.Predef.implicitly[slick.jdbc.GetResult[$typ]]"
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
      case s1 :: s2 :: ss => (evalString(s1), evalString(s2)) match {
        case (Some(str1), Some(str2)) => fuse(Literal(Constant(str1 + str2)) :: ss)
        case (None, Some(str2))       => s1 :: fuse(s2 :: ss)
        case (_, None)                => s1 :: s2 :: fuse(ss)
      }
      case s :: ss => s :: fuse(ss)
      case Nil => Nil
    }

    if(rawQueryParts.length == 1)
      (List(Literal(Constant(rawQueryParts.head))), q"slick.jdbc.SetParameter.SetUnit")
    else {
      val queryString = new ListBuffer[Tree]
      val remaining = new ListBuffer[c.Expr[SetParameter[Unit]]]
      paramsList.asInstanceOf[List[c.Expr[Any]]].iterator.zip(rawQueryParts.iterator).foreach { case (param, rawQueryPart) =>
        val (queryPart, append) = decode(rawQueryPart)
        queryString.append(Literal(Constant(queryPart)))
        if(append) queryString.append(param.tree)
        else {
          queryString.append(Literal(Constant("?")))
          val tpe = TypeTree(param.actualType)
          remaining += c.Expr[SetParameter[Unit]] {
            q"scala.Predef.implicitly[slick.jdbc.SetParameter[$tpe]].applied($param)"
          }
        }
      }
      queryString.append(Literal(Constant(rawQueryParts.last)))
      val pconv =
        if(remaining.isEmpty) q"slick.jdbc.SetParameter.SetUnit"
        else q"slick.jdbc.SetParameter.compose(..$remaining)"
      (fuse(queryString.result()), pconv)
    }
  }

  lazy val queryParts: Tree = q"scala.collection.immutable.Vector(..${interpolationResultParams._1})"

  def staticQueryString: String = interpolationResultParams._1 match {
    case Literal(Constant(s: String)) :: Nil => s
    case _ => c.abort(c.enclosingPosition, "Only constant strings may be used after '#$' in 'tsql' interpolation")
  }

  lazy val pconvTree: Tree = interpolationResultParams._2
}
