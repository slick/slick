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

  def createClass(className: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TypeName(x))(className, packages)

  def createObject(objectName: String, packages: scala.List[String]): Tree =
    createObjectOrClass(x => TermName(x))(objectName, packages)

  def createObjectOrClassFromString(generator: String => Name)(string: String): Tree = {
    val tokens = string.split('.').toList
    tokens match {
      case Nil => throw new SlickException("No class/object name defined")
      case _ => createObjectOrClass(generator)(tokens.last, tokens.dropRight(1))
    }
  }

  def createClassFromString(classString: String): Tree = {
    createObjectOrClassFromString(x => TypeName(x))(classString)
  }

  def createObjectFromString(objectString: String): Tree = {
    createObjectOrClassFromString(x => TermName(x))(objectString)
  }

  def createTuple[T <: Tree](elems: List[T]): Tree =
    Apply(Select(tupleObject(elems.length), TermName("apply")), elems)

  def createTupleOrSingleton[T <: Tree](elems: List[T]): Tree =
    elems match {
      case List(elem) => elem
      case other => createTuple(other)
    }

  def createImport(expr: Tree, selects: List[String], renamer: String => String = (str => str)): Import = {
    val selectors = {
      selects match {
        case Nil => List((nme.WILDCARD, null))
        case l => l.map(n => (TermName(n), TermName(renamer(n))))
      }
    }.map { case (name, rename) => ImportSelector(name, -1, rename, -1) }
    Import(expr, selectors)
  }

  def typeToTree(tpe: Type): Tree =
    tpe.asInstanceOf[TypeRef].args match {
      case Nil => Ident(tpe.typeSymbol)
      case args => AppliedTypeTree(Ident(tpe.typeSymbol), args.map(t => typeToTree(t)))
    }

  // some useful classes and objects
  def tupleObject(arity: Int): Tree = createObject("Tuple" + arity, List("scala"))
  def scalaProductClass: Tree = createClass("Product", List("scala"))
  def scalaSerializableClass: Tree = createClass("Serializable", List("scala"))
  def foreignKeyActionObject: Tree = createObject("ForeignKeyAction", List("scala", "slick", "lifted"))

  // helper for creating a case class
  object CaseClassCreator {
    val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
    val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]

    def fieldCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    def ctorParamCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
    def ctorCreate(ctorParams: List[ValDef]): DefDef = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
    def caseClassCreate(name: String, fields: List[ValDef], ctor: DefDef): ClassDef =
      ClassDef(Modifiers(CASE), TypeName(name), Nil, Template(
        List(scalaProductClass, scalaSerializableClass),
        emptyValDef,
        fields :+ ctor))
  }

  // some reflection stuff

  def implicitMembersNameOfType(tpe: scala.reflect.runtime.universe.Type): List[String] = {
    val members = tpe.members.toList
    val implicitMembers = members.filter(_.isImplicit)
    implicitMembers.map(m => m.name.decoded.trim)
  }

  def implicitMembersName[T <: AnyRef](obj: T)(implicit ttag: TypeTag[obj.type]): List[String] = {
    implicitMembersNameOfType(runtimeUniverse.typeOf[obj.type])
  }

}