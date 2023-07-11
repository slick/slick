package slick.memory

import scala.collection.mutable.ArrayBuffer

import slick.SlickException
import slick.ast.*
import slick.ast.TypeUtil.*
import slick.basic.BasicProfile
import slick.compiler.*
import slick.relational.*
import slick.util.{??, ConstArray}

/** The querying (read-only) part that can be shared between MemoryProfile and DistributedProfile. */
trait MemoryQueryingProfile extends BasicProfile { self: MemoryQueryingProfile =>

  type ColumnType[T] = ScalaType[T]
  type BaseColumnType[T] = ScalaType[T] & BaseTypedType[T]
  def compileInsert(tree: Node) = insertCompiler.run(tree).tree
  type CompiledInsert = Node

  trait MemoryQueryingAPI extends BasicAPI with ImplicitColumnTypes

  val api: MemoryQueryingAPI

  trait ImplicitColumnTypes {
    implicit def booleanColumnType    : ScalaBaseType[Boolean]       = ScalaBaseType.booleanType
    implicit def bigDecimalColumnType : ScalaNumericType[BigDecimal] = ScalaBaseType.bigDecimalType
    implicit def byteColumnType       : ScalaNumericType[Byte]       = ScalaBaseType.byteType
    implicit def charColumnType       : ScalaBaseType[Char]          = ScalaBaseType.charType
    implicit def doubleColumnType     : ScalaNumericType[Double]     = ScalaBaseType.doubleType
    implicit def floatColumnType      : ScalaNumericType[Float]      = ScalaBaseType.floatType
    implicit def intColumnType        : ScalaNumericType[Int]        = ScalaBaseType.intType
    implicit def longColumnType       : ScalaNumericType[Long]       = ScalaBaseType.longType
    implicit def shortColumnType      : ScalaNumericType[Short]      = ScalaBaseType.shortType
    implicit def stringColumnType     : ScalaBaseType[String]        = ScalaBaseType.stringType
  }

  /* internal: */

  /** The profile-specific representation of types */
  def typeInfoFor(t: Type): ScalaType[Any] = ((t.structural match {
    case t: ScalaType[?] => t
    case t: TypedType[?] => t.scalaType
    case o: OptionType   => typeInfoFor(o.elementType).asInstanceOf[ScalaBaseType[?]].optionType
    case t => throw new SlickException("No ScalaType found for type "+t)
  }): ScalaType[?]).asInstanceOf[ScalaType[Any]]

  class MemoryCodeGen extends CodeGen with ResultConverterCompiler[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing] {
    override def apply(state: CompilerState): CompilerState =
      state.map(n => retype(apply(n, state))).withWellTyped(false)

    override def compileServerSideAndMapping(serverSide: Node,
                                             mapping: Option[Node],
                                             state: CompilerState): (Node, Option[CompiledMapping]) =
      (serverSide, mapping.map(compileMapping))

    def retype(n: Node): Node = {
      val n2 = transformSimpleGrouping(n)
      val n3 = n2.mapChildren(retype, keepType = true)
      n3 :@ trType(n3.nodeType)
    }

    def transformSimpleGrouping(n: Node) = n match {
      case Bind(gen, g: GroupBy, p @ Pure(_: ProductNode | _: StructNode, _)) =>
        val p2 = transformCountAll(gen, p)
        if(p2 eq p) n else Bind(gen, g, p2).infer(typeChildren = true)
      case Library.SilentCast(n :@ tpe1) :@ tpe2 if tpe1 == tpe2              => n
      case n => n
    }

    def transformCountAll(gen: TermSymbol, n: Node): Node = n match {
      case Apply(Library.CountAll, ch @ ConstArray(Bind(gen2, FwdPath(s :: _), Pure(ProductOfCommonPaths(s2, _), _))))
        if s == gen && s2 == gen2 =>
        Apply(Library.Count, ch)(n.nodeType)
      case n                      =>
        n.mapChildren(ch => transformCountAll(gen, ch), keepType = true)
    }

    def trType(t: Type): Type = t.structural match {
      case t @ (_: StructType | _: ProductType | _: CollectionType | _: MappedScalaType | OptionType.NonPrimitive(_)) =>
        t.mapChildren(trType)
      case t                                                                                                          =>
        typeInfoFor(t)
    }

    override def compile(n: Node): ResultConverter[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing, ?] = n match {
      // We actually get a Scala Option value from the interpreter, so the SilentCast is not silent after all
      case Library.SilentCast(sel @ Select(_, ElementSymbol(idx)) :@ OptionType(_)) :@ tpe
        if !tpe.isInstanceOf[OptionType] =>
        val base =
          createColumnConverter(sel, idx, None).asInstanceOf[ResultConverter[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing, Option[Any]]]
        createGetOrElseResultConverter(
          base,
          () => throw new SlickException("Read null value for non-nullable column in Option")
        )
      case n =>
        super.compile(n)
    }

    def createColumnConverter(n: Node,
                              idx: Int,
                              column: Option[FieldSymbol]): ResultConverter[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing, ?] =
      new QueryResultConverter(idx, typeInfoFor(n.nodeType.structural).nullable)

    class QueryResultConverter(ridx: Int, nullable: Boolean)
      extends ResultConverter[QueryInterpreter.ProductValue, ArrayBuffer[Any], Nothing, Any] {
      def read(pr: QueryInterpreter.ProductValue) = {
        val v = pr(ridx - 1)
        if (!nullable && (v.asInstanceOf[AnyRef] eq null))
          throw new SlickException("Read null value for non-nullable column")

        // TODO: Remove this hack; see comment in ternary logic section of QueryInterpreter
        if (!nullable && v.isInstanceOf[Option[?]]) v.asInstanceOf[Option[?]].get
        else v
      }
      override def update(value: Any, pr: Updater): Nothing = ??
      override def set(value: Any, pp: Writer, offset: Int): Nothing = ??
      override def getDumpInfo = super.getDumpInfo.copy(mainInfo = s"ridx=$ridx, nullable=$nullable")
      def width = 1
    }
  }

  object ProductOfCommonPaths {
    def unapply(n: ProductNode): Option[(TermSymbol, Vector[List[TermSymbol]])] = if(n.children.isEmpty) None else
      n.children.iterator.foldLeft(null: Option[(TermSymbol, Vector[List[TermSymbol]])]) {
        case (None, _) => None
        case (null, FwdPath(sym :: rest)) => Some((sym, Vector(rest)))
        case (Some((sym0, v)), FwdPath(sym :: rest)) if sym == sym0 => Some((sym, v :+ rest))
        case _ => None
      }
  }
}
