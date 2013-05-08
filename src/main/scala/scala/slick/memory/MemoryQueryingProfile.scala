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
    new ParameterizedQuery[P, R](queryCompiler.run(Node(q)).tree)

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
    implicit def unitColumnType = ScalaBaseType.unitType
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
      val nullable = typeInfoFor(n.nodeType).nullable
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
      val n2 = n.nodeMapChildrenKeepType(retype)
      n2.nodeRebuildWithType(trType(n2.nodeType))
    }

    def trType(t: Type): Type = t match {
      case StructType(el) => StructType(el.map { case (s, t) => (s, trType(t)) })
      case ProductType(el) => ProductType(el.map(trType))
      case CollectionType(cons, el) => CollectionType(cons, trType(el))
      case t => typeInfoFor(t)
    }
  }
}
