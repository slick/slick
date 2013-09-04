package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.lifted._
import scala.slick.profile.{RelationalMappingCompilerComponent, RelationalDriver, StandardParameterizedQueries, RelationalProfile}
import scala.slick.SlickException

/** The querying (read-only) part that can be shared between MemoryDriver and DistributedDriver. */
trait MemoryQueryingProfile extends RelationalProfile with StandardParameterizedQueries { driver: MemoryQueryingDriver =>

  type ColumnType[T] = ScalaType[T]
  type BaseColumnType[T] = ScalaType[T] with BaseTypedType[T]

  def compileParameterizedQuery[P,R](q: Query[_, R]) =
    new ParameterizedQuery[P, R](queryCompiler.run(q.toNode).tree)

  trait Implicits extends super.Implicits with ImplicitColumnTypes

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

trait MemoryQueryingDriver extends RelationalDriver with MemoryQueryingProfile with RelationalMappingCompilerComponent { driver =>

  type RowReader = QueryInterpreter.ProductValue

  /** The driver-specific representation of types */
  type TypeInfo = ScalaType[Any]
  def typeInfoFor(t: Type): TypeInfo = ((t match {
    case t: ScalaType[_] => t
    case t: TypedType[_] => t.scalaType
    case o: OptionType => typeInfoFor(o.elementType).asInstanceOf[ScalaBaseType[_]].optionType
    case t => throw new SlickException("No ScalaType found for type "+t)
  }): ScalaType[_]).asInstanceOf[ScalaType[Any]]

  trait QueryMappingCompiler extends super.MappingCompiler {

    def createColumnConverter(n: Node, path: Node, option: Boolean): ResultConverter = {
      val Select(_, ElementSymbol(ridx)) = path
      val nullable = typeInfoFor(n.nodeType.structural).nullable
      new ResultConverter {
        def read(pr: RowReader) = {
          val v = pr(ridx-1)
          if(!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")
          v
        }
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter) = ???
      }
    }
  }

  class MemoryCodeGen extends CodeGen with QueryMappingCompiler {

    def apply(state: CompilerState): CompilerState = state.map(n => retype(apply(n, state)))

    def apply(node: Node, state: CompilerState): Node =
      ClientSideOp.mapResultSetMapping(node, keepType = true) { rsm =>
        val nmap = CompiledMapping(compileMapping(rsm.map), rsm.map.nodeType)
        rsm.copy(map = nmap).nodeTyped(rsm.nodeType)
      }

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
      case StructType(el) => StructType(el.map { case (s, t) => (s, trType(t)) })
      case ProductType(el) => ProductType(el.map(trType))
      case CollectionType(cons, el) => CollectionType(cons, trType(el))
      case t => typeInfoFor(t)
    }
  }
}
