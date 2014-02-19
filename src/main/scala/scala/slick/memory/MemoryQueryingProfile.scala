package scala.slick.memory

import scala.language.{implicitConversions, existentials}
import scala.slick.ast._
import scala.slick.compiler._
import scala.slick.lifted._
import scala.slick.profile.{RelationalMappingCompilerComponent, RelationalDriver, RelationalProfile}
import scala.slick.SlickException
import scala.reflect.ClassTag

/** The querying (read-only) part that can be shared between MemoryDriver and DistributedDriver. */
trait MemoryQueryingProfile extends RelationalProfile { driver: MemoryQueryingDriver =>

  type ColumnType[T] = ScalaType[T]
  type BaseColumnType[T] = ScalaType[T] with BaseTypedType[T]
  type UnshapedQueryExecutor[R] = UnshapedQueryExecutorDef[R]

  val MappedColumnType = new MappedColumnTypeFactory

  def createUnshapedQueryExecutor[M](value: M): UnshapedQueryExecutor[M] = new UnshapedQueryExecutorDef[M](value)

  class MappedColumnTypeFactory extends super.MappedColumnTypeFactory {
    def base[T : ClassTag, U : BaseColumnType](tmap: T => U, tcomap: U => T): BaseColumnType[T] =
      new MappedColumnType(implicitly[BaseColumnType[U]], tmap, tcomap)
  }

  class MappedColumnType[T, U](val baseType: ColumnType[U], toBase: T => U, toMapped: U => T) extends ScalaType[T] with BaseTypedType[T] {
    def nullable: Boolean = baseType.nullable
    def ordered: Boolean = baseType.ordered
    def zero: T = toMapped(baseType.zero)
    def scalaOrderingFor(ord: Ordering): scala.math.Ordering[T] = new scala.math.Ordering[T] {
      val uOrdering = baseType.scalaOrderingFor(ord)
      def compare(x: T, y: T): Int = uOrdering.compare(toBase(x), toBase(y))
    }
  }

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
  def typeInfoFor(t: Type): ScalaType[Any] = ((t match {
    case t: ScalaType[_] => t
    case t: TypedType[_] => t.scalaType
    case o: OptionType => typeInfoFor(o.elementType).asInstanceOf[ScalaBaseType[_]].optionType
    case t => throw new SlickException("No ScalaType found for type "+t)
  }): ScalaType[_]).asInstanceOf[ScalaType[Any]]

  trait QueryMappingCompiler extends super.MappingCompiler {

    def createColumnConverter(n: Node, path: Node, option: Boolean, column: Option[FieldSymbol]): ResultConverter = {
      val Select(_, ElementSymbol(ridx)) = path
      val nullable = typeInfoFor(n.nodeType.structural).nullable
      new ResultConverter {
        def read(pr: RowReader) = {
          val v = pr(ridx-1)
          if(!nullable && (v.asInstanceOf[AnyRef] eq null)) throw new SlickException("Read null value for non-nullable column")
          v
        }
        def update(value: Any, pr: RowUpdater) = ???
        def set(value: Any, pp: RowWriter, forced: Boolean) = ???
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
