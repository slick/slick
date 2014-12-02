package scala.slick.memory

import scala.concurrent.Future
import scala.language.{implicitConversions, existentials}
import scala.collection.mutable.ArrayBuffer
import scala.slick.SlickException
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.lifted._
import scala.slick.relational._
import scala.slick.profile.{BasicDriver, BasicProfile}
import scala.slick.util.??
import TypeUtil._

/** The querying (read-only) part that can be shared between MemoryDriver and DistributedDriver. */
trait MemoryQueryingProfile extends BasicProfile { driver: MemoryQueryingDriver =>

  type ColumnType[T] = ScalaType[T]
  type BaseColumnType[T] = ScalaType[T] with BaseTypedType[T]
  def compileInsert(tree: Node) = insertCompiler.run(tree).tree
  type CompiledInsert = Node

  protected trait CommonImplicits extends super.CommonImplicits with ImplicitColumnTypes
  trait Implicits extends super.Implicits with CommonImplicits
  trait SimpleQL extends super.SimpleQL with Implicits
  trait API extends super.API with CommonImplicits

  val Implicit: Implicits
  val simple: SimpleQL
  val api: API

  trait ImplicitColumnTypes {
    implicit def booleanColumnType = ScalaBaseType.booleanType
    implicit def bigDecimalColumnType = ScalaBaseType.bigDecimalType
    implicit def byteColumnType = ScalaBaseType.byteType
    implicit def charColumnType = ScalaBaseType.charType
    implicit def doubleColumnType = ScalaBaseType.doubleType
    implicit def floatColumnType = ScalaBaseType.floatType
    implicit def intColumnType = ScalaBaseType.intType
    implicit def longColumnType = ScalaBaseType.longType
    implicit def shortColumnType = ScalaBaseType.shortType
    implicit def stringColumnType = ScalaBaseType.stringType
  }
}

trait MemoryQueryingDriver extends BasicDriver with MemoryQueryingProfile { driver =>

  /** The driver-specific representation of types */
  def typeInfoFor(t: Type): ScalaType[Any] = ((t.structural match {
    case t: ScalaType[_] => t
    case t: TypedType[_] => t.scalaType
    case o: OptionType => typeInfoFor(o.elementType).asInstanceOf[ScalaBaseType[_]].optionType
    case t => throw new SlickException("No ScalaType found for type "+t)
  }): ScalaType[_]).asInstanceOf[ScalaType[Any]]

  class MemoryCodeGen extends CodeGen with ResultConverterCompiler[MemoryResultConverterDomain] {
    override def apply(state: CompilerState): CompilerState = state.map(n => retype(apply(n, state)))
    def compileServerSideAndMapping(serverSide: Node, mapping: Option[Node], state: CompilerState) = (serverSide, mapping.map(compileMapping))

    def retype(n: Node): Node = {
      val n2 = transformSimpleGrouping(n)
      val n3 = n2.nodeMapChildren(retype, keepType = true)
      n3.nodeRebuildWithType(trType(n3.nodeType))
    }

    def transformSimpleGrouping(n: Node) = n match {
      case Bind(gen, g: GroupBy, p @ Pure(_: ProductNode, _)) =>
        val p2 = transformCountAll(gen, p)
        if(p2 eq p) n else Bind(gen, g, p2).nodeWithComputedType(typeChildren = true)
      case n => n
    }

    def transformCountAll(gen: Symbol, n: Node): Node = n match {
      case Apply(Library.CountAll, ch @ Seq(Bind(gen2, FwdPath(s :: _), Pure(ProductOfCommonPaths(s2, _), _)))) if s == gen && s2 == gen2 =>
        Apply(Library.Count, ch)(n.nodeType)
      case n => n.nodeMapChildren(ch => transformCountAll(gen, ch), keepType = true)
    }

    def trType(t: Type): Type = t.structural match {
      case t @ (_: StructType | _: ProductType | _: CollectionType | _: MappedScalaType | OptionType.NonPrimitive(_)) => t.mapChildren(trType)
      case t => typeInfoFor(t)
    }

    override def compile(n: Node): ResultConverter[MemoryResultConverterDomain, _] = n match {
      // We actually get a Scala Option value from the interpreter, so the SilentCast is not silent after all
      case Library.SilentCast(sel @ Select(_, ElementSymbol(idx)) :@ OptionType(tpe2)) :@ tpe if !tpe.isInstanceOf[OptionType] =>
        val base = createColumnConverter(sel, idx, None).asInstanceOf[ResultConverter[MemoryResultConverterDomain, Option[Any]]]
        createGetOrElseResultConverter(base, () => throw new SlickException("Read null value for non-nullable column in Option"))
      case n => super.compile(n)
    }

    def createColumnConverter(n: Node, idx: Int, column: Option[FieldSymbol]): ResultConverter[MemoryResultConverterDomain, _] =
      new QueryResultConverter(idx, typeInfoFor(n.nodeType.structural).nullable)

    class QueryResultConverter(ridx: Int, nullable: Boolean) extends ResultConverter[MemoryResultConverterDomain, Any] {
      def read(pr: MemoryResultConverterDomain#Reader) = {
        val v = pr(ridx-1)
        if(!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")

        // TODO: Remove this hack; see comment in ternary logic section of QueryInterpreter
        if(!nullable && v.isInstanceOf[Option[_]]) v.asInstanceOf[Option[_]].get
        else v
      }
      def update(value: Any, pr: MemoryResultConverterDomain#Updater) = ??
      def set(value: Any, pp: MemoryResultConverterDomain#Writer) = ??
      override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"ridx=$ridx, nullable=$nullable")
      def width = 1
    }
  }
}

trait MemoryResultConverterDomain extends ResultConverterDomain {
  type Reader = QueryInterpreter.ProductValue
  type Writer = ArrayBuffer[Any]
}
