package slick.compiler

import slick.SlickException
import slick.ast._
import TypeUtil._
import slick.util.ConstArray

/** Create a ResultSetMapping root node, ensure that the top-level server-side node returns a
  * collection, and hoist client-side type conversions into the ResultSetMapping. The original
  * result type (which was removed by `removeMappedTypes`) is assigned back to the top level. */
class CreateResultSetMapping extends Phase {
  val name = "createResultSetMapping"

  def apply(state: CompilerState) = state.map { n =>
    val tpe = state.get(Phase.removeMappedTypes).get
    ClientSideOp.mapServerSide(n, keepType = false) { ch =>
      val syms = ch.nodeType.structural match {
        case StructType(defs) => defs.map(_._1)
        case CollectionType(_, Type.Structural(StructType(defs))) => defs.map(_._1)
        case t => throw new SlickException("No StructType found at top level: "+t)
      }
      val gen = new AnonSymbol
      (tpe match {
        case CollectionType(cons, el) =>
          ResultSetMapping(gen, collectionCast(ch, cons).infer(), createResult(Ref(gen) :@ ch.nodeType.asCollectionType.elementType, el, syms))
        case t =>
          ResultSetMapping(gen, ch, createResult(Ref(gen) :@ ch.nodeType.asCollectionType.elementType, t, syms))
      })
    }.infer()
  }

  def collectionCast(ch: Node, cons: CollectionTypeConstructor): Node = ch.nodeType match {
    case CollectionType(c, _) if c == cons => ch
    case _ => CollectionCast(ch, cons).infer()
  }

  /** Create a structured return value for the client side, based on the
    * result type (which may contain MappedTypes). */
  def createResult(ref: Ref, tpe: Type, syms: ConstArray[TermSymbol]): Node = {
    var curIdx = 0
    def f(tpe: Type): Node = {
      logger.debug("Creating mapping from "+tpe)
      tpe.structural match {
        case ProductType(ch) =>
          ProductNode(ch.map(f))
        case StructType(ch) =>
          ProductNode(ch.map { case (_, t) => f(t) })
        case t: MappedScalaType =>
          TypeMapping(f(t.baseType), t.mapper, t.classTag)
        case o @ OptionType(Type.Structural(el)) if !el.isInstanceOf[AtomicType] =>
          val discriminator = Select(ref, syms(curIdx)).infer()
          curIdx += 1
          val data = f(o.elementType)
          RebuildOption(discriminator, data)
        case t =>
          curIdx += 1
          // Assign the original type. Inside a RebuildOption the actual column type will always be
          // Option-lifted but we can still treat it as the base type when the discriminator matches.
          val sel = Select(ref, syms(curIdx-1)).infer()
          val tSel = t.structuralRec
          if(sel.nodeType.structuralRec == tSel) sel else Library.SilentCast.typed(tSel, sel)
      }
    }
    f(tpe)
  }
}

/** Remove all mapped types from the tree and store the original top-level type as the phase state
  * to be used later for building the ResultSetMapping. */
class RemoveMappedTypes extends Phase {
  val name = "removeMappedTypes"
  type State = Type

  def apply(state: CompilerState) =
    if(state.get(Phase.assignUniqueSymbols).map(_.typeMapping).getOrElse(true))
      state.withNode(removeTypeMapping(state.tree)) + (this -> state.tree.nodeType)
    else state + (this -> state.tree.nodeType)

  /** Remove TypeMapping nodes and MappedTypes */
  def removeTypeMapping(n: Node): Node = n match {
    case t: TypeMapping => removeTypeMapping(t.child)
    case n =>
      val n2 = n.mapChildren(removeTypeMapping, keepType = true)
      n2 :@ removeMappedType(n2.nodeType)
  }

  /** Remove MappedTypes from a Type */
  def removeMappedType(tpe: Type): Type = tpe match {
    case m: MappedScalaType => removeMappedType(m.baseType)
    case t => t.mapChildren(removeMappedType)
  }
}
