package scala.slick.compiler

import scala.slick.SlickException
import scala.slick.ast._
import Util._
import TypeUtil._

/** Expand sum types and their catamorphisms to equivalent product type operations. */
class ExpandSums extends Phase {
  val name = "expandSums"

  def apply(state: CompilerState) = state.map(n => ClientSideOp.mapServerSide(n)(tr))

  val one = LiteralNode(ScalaBaseType.intType.optionType, Option(1))

  /** Perform the sum expansion on a Node */
  def tr(tree: Node): Node = {
    val tree2 = tree.nodeMapChildren(tr, keepType = true)
    val tree3 = tree2 match {
      // Primitive OptionFold -> translate to null check
      case OptionFold(from :@ OptionType.Primitive(_), ifEmpty, map, gen) =>
        val pred = Library.==.typed[Boolean](from, LiteralNode(null))
        val n2 = (ifEmpty, map) match {
          case (LiteralNode(true), LiteralNode(false)) => pred
          case (LiteralNode(false), LiteralNode(true)) => Library.Not.typed[Boolean](pred)
          case _ =>
            val ifDefined = map.replace({
              case Ref(s) :@ tpe if s == gen => silentCast(tpe, from)
            }, keepType = true).nodeWithComputedType()
            val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
            IfThenElse(Vector(pred, ifEmpty2, ifDefined))
        }
        n2.nodeWithComputedType()

      // Other OptionFold -> translate to discriminator check
      case OptionFold(from, ifEmpty, map, gen) =>
        val left = from.select(ElementSymbol(1)).nodeWithComputedType()
        val pred = Library.==.typed[Boolean](left, one)
        val n2 = (ifEmpty, map) match {
          case (LiteralNode(true), LiteralNode(false)) => Library.Not.typed[Boolean](pred)
          case (LiteralNode(false), LiteralNode(true)) => pred
          case _ =>
            val ifDefined = map.replace({
              case Ref(s) :@ tpe if s == gen => silentCast(tpe, from.select(ElementSymbol(2)).nodeWithComputedType())
            }, keepType = true).nodeWithComputedType()
            val ifEmpty2 = silentCast(ifDefined.nodeType.structural, ifEmpty)
            if(left == one) ifDefined else IfThenElse(Vector(pred, ifDefined, ifEmpty2))
        }
        n2.nodeWithComputedType()

      // Primitive OptionApply -> leave unchanged
      case n @ OptionApply(_) :@ OptionType.Primitive(_) => n

      // Other OptionApply -> translate to product form
      case n @ OptionApply(ch) => ProductNode(Vector(one, silentCast(toOptionColumns(ch.nodeType), ch))).nodeWithComputedType()

      // Non-primitive GetOrElse
      // (.get is only defined on primitive Options, but this can occur inside of HOFs like .map)
      case g @ GetOrElse(ch :@ tpe, _) =>
        tpe match {
          case OptionType.Primitive(_) => g
          case _ => throw new SlickException(".get may only be called on Options of top-level primitive types")
        }

      case n => n
    }
    tree3.nodeTypedOrCopy(trType(tree3.nodeType))
  }

  def silentCast(tpe: Type, n: Node): Node =
    if(n.nodeType == tpe) n else Library.SilentCast.typed(tpe, n)

  /** Perform the sum expansion on a Type */
  def trType(tpe: Type): Type = tpe.mapChildren(trType) match {
    case t @ OptionType.Primitive(_) => t
    case OptionType(ch) => ProductType(Vector(ScalaBaseType.intType.optionType, toOptionColumns(ch)))
    case t => t
  }

  /** Strip nominal types and convert all atomic types to OptionTypes */
  def toOptionColumns(tpe: Type): Type = tpe.structural match {
    case o @ OptionType(str) if str.children.isEmpty => o
    case str if str.children.isEmpty => OptionType(str)
    case str => str.mapChildren(toOptionColumns)
  }
}
