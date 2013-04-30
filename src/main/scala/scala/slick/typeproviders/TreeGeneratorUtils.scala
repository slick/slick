package scala.slick.typeproviders

import scala.slick.schema.Table
import scala.slick.schema.Column
import scala.slick.schema.ForeignKey
import scala.slick.schema.Index
import scala.slick.schema.naming.Naming
import scala.slick.schema.PrimaryKey
import scala.slick.SlickException
import scala.reflect.api.Universe
import scala.language.reflectiveCalls

trait TreeGeneratorUtils { self: MacroHelpers =>
  import universe._
  import Flag._

  import scala.reflect.runtime.{ universe => runtimeUniverse }

  /**
   * Creates Module or Class tree for a given qualified name
   */
  def createObjectOrClass(generator: String => Name)(name: String, packages: scala.List[String]): Tree = {
    val classType = generator(name)
    val packagesType = packages map (TermName(_))
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

  /**
   * Creates Class tree for a given qualified name
   */
  def createClass(className: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TypeName(x))(className, packages)

  /**
   * Creates Module tree for a given qualified name
   */
  def createObject(objectName: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TermName(x))(objectName, packages)

  /**
   * Creates Module or Class tree for a given qualified name, given in dot-separated form
   */
  def createObjectOrClassFromString(generator: String => Name)(string: String): Tree = {
    val tokens = string.split('.').toList
    tokens match {
      case Nil => throw new SlickException("No class/object name defined")
      case _ => createObjectOrClass(generator)(tokens.last, tokens.dropRight(1))
    }
  }

  /**
   * Creates Class tree for a given qualified name, given in dot-separated form
   */
  def createClassFromString(classString: String): Tree = {
    createObjectOrClassFromString(x => TypeName(x))(classString)
  }

  /**
   * Creates Module tree for a given qualified name, given in dot-separated form
   */
  def createObjectFromString(objectString: String): Tree = {
    createObjectOrClassFromString(x => TermName(x))(objectString)
  }

  /**
   * Creates Tuple creation tree, i.e. TupleX.apply(elem1, ..., elemX), for given elements
   */
  def createTuple[T <: Tree](elems: List[T]): Tree =
    Apply(Select(tupleObject(elems.length), TermName("apply")), elems)

  /**
   * If there is only one elements, returns that elements, if there is more, creates tuple
   * from those elements
   */
  def createTupleOrSingleton[T <: Tree](elems: List[T]): Tree =
    elems match {
      case List(elem) => elem
      case other => createTuple(other)
    }

  /**
   * Creates import statements for a given expression tree and selectors
   */
  def createImport(expr: Tree, selects: List[String], renamer: String => String = (str => str)): Import = {
    val selectors = {
      selects match {
        case Nil => List((nme.WILDCARD, null))
        case l => l.map(n => (TermName(n), TermName(renamer(n))))
      }
    }.map { case (name, rename) => ImportSelector(name, -1, rename, -1) }
    Import(expr, selectors)
  }

  /**
   * Generates the type tree for a given type
   * (This one handles all exceptions for case classes, and etc.)
   */
  def typeToTree(tpe: Type): Tree = {
    def erasedTypeToTree(etpe: Type): Tree =
      createClassFromString(etpe.typeSymbol.fullName)
    tpe.asInstanceOf[TypeRef].args match {
      case Nil => erasedTypeToTree(tpe.erasure)
      case args => AppliedTypeTree(erasedTypeToTree(tpe.erasure), args.map(t => typeToTree(t)))
    }
  }

  // some useful classes and objects
  /**
   * Returns Tuple object tree for given arity
   */
  def tupleObject(arity: Int): Tree = createObject("Tuple" + arity, List("scala"))
  /**
   * Returns Product class tree
   */
  def scalaProductClass: Tree = createClass("Product", List("scala"))
  /**
   * Returns Serializable class tree
   */
  def scalaSerializableClass: Tree = createClass("Serializable", List("scala"))
  /**
   * Returns ForeignKeyAction class tree
   */
  def foreignKeyActionObject: Tree = createObject("ForeignKeyAction", List("scala", "slick", "lifted"))

  // helper for creating a case class
  /**
   * A helper module for dealing with case class tree generation
   */
  object CaseClassCreator {
    val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
    val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]

    /**
     * Creates each field
     */
    def fieldCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    /**
     * Creates each parameter of constructor
     */
    def ctorParamCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    /**
     * Creates constructor tree representation
     */
    //    def ctorCreate(ctorParams: List[ValDef]): DefDef = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
    def ctorCreate(ctorParams: List[ValDef]): DefDef = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(), Literal(Constant(()))))
    /**
     * Creates case class
     */
    def caseClassCreate(name: String, fields: List[ValDef], ctor: DefDef): ClassDef =
      ClassDef(Modifiers(CASE), TypeName(name), Nil, Template(
        List(scalaProductClass, scalaSerializableClass),
        emptyValDef,
        fields :+ ctor))
  }

  // some reflection stuff

  /**
   * Returns names of implicit members of given type
   */
  def implicitMembersNameOfType(tpe: scala.reflect.runtime.universe.Type): List[String] = {
    val members = tpe.members.toList
    val implicitMembers = members.filter(_.isImplicit)
    implicitMembers.map(m => m.name.decoded.trim)
  }

  def implicitMembersName[T <: AnyRef](obj: T)(implicit ttag: TypeTag[obj.type]): List[String] = {
    implicitMembersNameOfType(runtimeUniverse.typeOf[obj.type])
  }

}