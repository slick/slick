package scala.slick.compiler

import scala.slick.ast._
import scala.slick.lifted.Column
import Util._

/** Create a ResultSetMapping root node, ensure that the top-level server-side
  * node returns a collection, and hoist client-side type conversions into the
  * ResultSetMapping. */
class CreateResultSetMapping extends Phase {
  val name = "createResultSetMapping"

  def apply(state: CompilerState) = state.map { n =>
  // Get the type at the outer layer with TypeMappings in place
    val tpe = n.nodeWithComputedType(new DefaultSymbolScope(Map.empty), false).nodeType
    logger.debug("Client-side result type: "+tpe)
    val n2 = toCollection(removeTypeMapping(n))
    ClientSideOp.mapServerSide(n2) { ch =>
      val gen = new AnonSymbol
      ResultSetMapping(gen, ch, createResult(gen, tpe match {
        case CollectionType(_, el) => el
        case t => t
      }))
    }
  }

  /** Remove TypeMapping nodes and MappedTypes */
  def removeTypeMapping(n: Node): Node = n match {
    case t: TypeMapping => removeTypeMapping(t.child)
    case n =>
      val n2 = n.nodeMapChildren(removeTypeMapping)
      val tpe = n2.nodeType
      val tpe2 = removeMappedType(tpe)
      if(tpe2 eq tpe) n2 else n2.nodeTypedOrCopy(tpe2)
  }

  /** Force collection return type */
  def toCollection(n: Node): Node = n match {
    case _: Column[_] | _: Apply | _: ProductNode => First(Pure(n))
    case n => n
  }

  /** Remove MappedTypes from a Type */
  def removeMappedType(tpe: Type): Type = tpe match {
    case StructType(ch) =>
      mapOrNone(ch.map(_._2))(removeMappedType).fold(tpe)(nch => StructType((ch, nch).zipped.map { case ((s, _), n) => (s, n) }))
    case ProductType(ch) => mapOrNone(ch)(removeMappedType).fold(tpe)(ProductType.apply _)
    case CollectionType(cons, el) =>
      val el2 = removeMappedType(el)
      if(el2 eq el) tpe else CollectionType(cons, el2)
    case m: MappedScalaType => removeMappedType(m.baseType)
    case t => t
  }

  /** Create a structured return value for the client side, based on the
    * result type (which may contain MappedTypes). References are created
    * to linearized columns and not to the real structure. This keeps further
    * compiler phases simple because we don't have to any rewriting here. We
    * just need to be careful not to typecheck the resulting tree or resolve
    * those refs until the server-side tree has been linearized. In the future
    * we may want to create the real refs and rewrite them later in order to
    * gain the ability to optimize the set of columns in the result set. */
  def createResult(sym: Symbol, tpe: Type): Node = {
    var curIdx = 0
    def f(tpe: Type): Node = tpe match {
      case ProductType(ch) =>
        if(ch.length == 1) f(ch(0)) else ProductNode(ch.map(f))
      case StructType(ch) =>
        if(ch.length == 1) f(ch(0)._2) else ProductNode(ch.map { case (_, t) => f(t) })
      case t: MappedScalaType =>
        TypeMapping(f(t.baseType), t.baseType, t.toBase, t.toMapped)
      case t =>
        curIdx += 1
        Select(Ref(sym), ElementSymbol(curIdx))
    }
    f(tpe)
  }
}
