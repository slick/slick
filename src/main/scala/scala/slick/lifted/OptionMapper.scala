package scala.slick.lifted

import annotation.implicitNotFound
import scala.slick.ast.{Typed, OptionApply, FunctionSymbol, BaseTypedType, Node, TypedType}

trait OptionMapper[BR, R] extends (Rep[BR] => Rep[R]) {
  def lift: Boolean

  def column(fs: FunctionSymbol, ch: Node*)(implicit bt: TypedType[BR]): Rep[R] = {
    implicit val tt = liftedType
    Rep.forNode[R](fs.typed(tt, ch: _*))
  }

  def liftedType(implicit bt: TypedType[BR]): TypedType[R] =
    (if(lift) bt.optionType else bt).asInstanceOf[TypedType[R]]
}

@implicitNotFound("Cannot perform option-mapped operation\n      with type: (${P1}, ${P2}) => ${R}\n  for base type: (${B1}, ${B2}) => ${BR}")
sealed trait OptionMapper2[B1, B2, BR, P1, P2, R] extends OptionMapper[BR, R]

object OptionMapper2 {
  val plain = new OptionMapper2[Any,Any,Any,Any,Any,Any] {
    def apply(n: Rep[Any]): Rep[Any] = n
    def lift = false
  }
  val option = new OptionMapper2[Any,Any,Any,Any,Any,Option[Any]] {
    def apply(n: Rep[Any]): Rep[Option[Any]] = Rep.forNode(OptionApply(n.toNode))(n.asInstanceOf[Typed].tpe.asInstanceOf[TypedType[Any]].optionType)
    def lift = true
  }

  @inline implicit def getOptionMapper2TT[B1, B2 : BaseTypedType, BR] = OptionMapper2.plain .asInstanceOf[OptionMapper2[B1, B2, BR, B1,         B2,         BR]]
  @inline implicit def getOptionMapper2TO[B1, B2 : BaseTypedType, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, B1,         Option[B2], Option[BR]]]
  @inline implicit def getOptionMapper2OT[B1, B2 : BaseTypedType, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], B2,         Option[BR]]]
  @inline implicit def getOptionMapper2OO[B1, B2 : BaseTypedType, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], Option[B2], Option[BR]]]
}

@implicitNotFound("Cannot perform option-mapped operation\n      with type: (${P1}, ${P2}, ${P3}) => ${R}\n  for base type: (${B1}, ${B2}, ${B3}) => ${BR}")
sealed trait OptionMapper3[B1, B2, B3, BR, P1, P2, P3, R] extends OptionMapper[BR, R]

object OptionMapper3 {
  val plain = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Any] {
    def apply(n: Rep[Any]): Rep[Any] = n
    def lift = false
  }
  val option = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Option[Any]] {
    def apply(n: Rep[Any]): Rep[Option[Any]] = Rep.forNode(OptionApply(n.toNode))(n.asInstanceOf[Typed].tpe.asInstanceOf[TypedType[Any]].optionType)
    def lift = true
  }

  @inline implicit def getOptionMapper3TTT[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.plain .asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         B3,         BR]]
  @inline implicit def getOptionMapper3TTO[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3TOT[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], B3,         Option[BR]]]
  @inline implicit def getOptionMapper3TOO[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3OTT[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         B3,         Option[BR]]]
  @inline implicit def getOptionMapper3OTO[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3OOT[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], B3,         Option[BR]]]
  @inline implicit def getOptionMapper3OOO[B1, B2 : BaseTypedType, B3 : BaseTypedType, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], Option[B3], Option[BR]]]
}

object OptionMapperDSL {
  type arg[B1, P1] = {
    type to[BR, PR] = OptionMapper2[B1, B1, BR, P1, P1, PR]
    type toSame = OptionMapper2[B1, B1, B1, P1, P1, P1]
    type arg[B2, P2] = {
      type to[BR, PR] = OptionMapper2[B1, B2, BR, P1, P2, PR]
      type arg[B3, P3] = {
        type to[BR, PR] = OptionMapper3[B1, B2, B3, BR, P1, P2, P3, PR]
      }
    }
  }
}
