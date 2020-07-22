package slick.lifted

import annotation.implicitNotFound
import slick.ast.{OptionType, OptionApply, FunctionSymbol, BaseTypedType, Node, TypedType}

trait OptionMapper[BR, R] extends (Rep[BR] => Rep[R]) {
  def lift: Boolean

  def column(fs: FunctionSymbol, ch: Node*)(implicit bt: TypedType[BR]): Rep[R] = {
    implicit val tt: TypedType[R] = liftedType
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
    override def toString = "OptionMapper2.plain"
  }
  val option = new OptionMapper2[Any,Any,Any,Any,Any,Option[Any]] {
    def apply(n: Rep[Any]): Rep[Option[Any]] = Rep.forNode(OptionApply(n.toNode))(n.asInstanceOf[Rep.TypedRep[Any]].tpe.optionType)
    def lift = true
    override def toString = "OptionMapper2.option"
  }

  @inline implicit def getOptionMapper2TT[B1, B2 : BaseTypedType, P2 <: B2, BR]: OptionMapper2[B1, B2, BR, B1,         P2,         BR]         = OptionMapper2.plain .asInstanceOf[OptionMapper2[B1, B2, BR, B1,         P2,         BR]]
  @inline implicit def getOptionMapper2TO[B1, B2 : BaseTypedType, P2 <: B2, BR]: OptionMapper2[B1, B2, BR, B1,         Option[P2], Option[BR]] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, B1,         Option[P2], Option[BR]]]
  @inline implicit def getOptionMapper2OT[B1, B2 : BaseTypedType, P2 <: B2, BR]: OptionMapper2[B1, B2, BR, Option[B1], P2,         Option[BR]] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], P2,         Option[BR]]]
  @inline implicit def getOptionMapper2OO[B1, B2 : BaseTypedType, P2 <: B2, BR]: OptionMapper2[B1, B2, BR, Option[B1], Option[P2], Option[BR]] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], Option[P2], Option[BR]]]
}

@implicitNotFound("Cannot perform option-mapped operation\n      with type: (${P1}, ${P2}, ${P3}) => ${R}\n  for base type: (${B1}, ${B2}, ${B3}) => ${BR}")
sealed trait OptionMapper3[B1, B2, B3, BR, P1, P2, P3, R] extends OptionMapper[BR, R]

object OptionMapper3 {
  val plain = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Any] {
    def apply(n: Rep[Any]): Rep[Any] = n
    def lift = false
    override def toString = "OptionMapper3.plain"
  }
  val option = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Option[Any]] {
    def apply(n: Rep[Any]): Rep[Option[Any]] = Rep.forNode(OptionApply(n.toNode))(n.asInstanceOf[Rep.TypedRep[Any]].tpe.optionType)
    def lift = true
    override def toString = "OptionMapper3.option"
  }

  @inline implicit def getOptionMapper3TTT[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, B1,         P2,         P3,         BR]         = OptionMapper3.plain .asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         P2,         P3,         BR]]
  @inline implicit def getOptionMapper3TTO[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, B1,         P2,         Option[P3], Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         P2,         Option[P3], Option[BR]]]
  @inline implicit def getOptionMapper3TOT[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, B1,         Option[P2], P3,         Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[P2], P3,         Option[BR]]]
  @inline implicit def getOptionMapper3TOO[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, B1,         Option[P2], Option[P3], Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[P2], Option[P3], Option[BR]]]
  @inline implicit def getOptionMapper3OTT[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, Option[B1], P2,         P3,         Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], P2,         P3,         Option[BR]]]
  @inline implicit def getOptionMapper3OTO[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, Option[B1], P2,         Option[P3], Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], P2,         Option[P3], Option[BR]]]
  @inline implicit def getOptionMapper3OOT[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, Option[B1], Option[P2], P3,         Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[P2], P3,         Option[BR]]]
  @inline implicit def getOptionMapper3OOO[B1, B2 : BaseTypedType, P2 <: B2, B3 : BaseTypedType, P3 <: B3, BR]: OptionMapper3[B1, B2, B3, BR, Option[B1], Option[P2], Option[P3], Option[BR]] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[P2], Option[P3], Option[BR]]]
}

object OptionMapperDSL {
  type arg[B1, P1] = {
    type to[BR, PR] = OptionMapper2[Boolean, B1, BR, Boolean, P1, PR]
    type arg[B2, P2] = {
      type to[BR, PR] = OptionMapper2[B1, B2, BR, P1, P2, PR]
      type arg[B3, P3] = {
        type to[BR, PR] = OptionMapper3[B1, B2, B3, BR, P1, P2, P3, PR]
      }
    }
  }
}

/** A typeclass that lifts a mixed type to the packed Option type. */
sealed trait OptionLift[M, O] {
  def lift(v: M): O
}

object OptionLift extends OptionLiftLowPriority {
  final implicit def repOptionLift[M <: Rep[_], P](implicit shape: Shape[_ <: FlatShapeLevel, M, _, Rep[P]]): OptionLift[M, Rep[Option[P]]] = new OptionLift[M, Rep[Option[P]]] {
    def lift(v: M): Rep[Option[P]] = {
      val n = OptionApply(v.toNode)
      val packed = shape.pack(v)
      packed match {
        case r: Rep.TypedRep[_] if !r.tpe.isInstanceOf[OptionType] /* An primitive column */ =>
          Rep.forNode[Option[P]](n)(r.tpe.asInstanceOf[TypedType[P]].optionType)
        case _ =>
          RepOption[P](ShapedValue(packed, shape.packedShape), n)
      }
    }
    override def toString = s"RepOptionLift($shape)"
  }
}

sealed trait OptionLiftLowPriority {
  final implicit def anyOptionLift[M, P](implicit shape: Shape[_ <: FlatShapeLevel, M, _, P]): OptionLift[M, Rep[Option[P]]] = new OptionLift[M, Rep[Option[P]]] {
    def lift(v: M): Rep[Option[P]] =
      RepOption[P](ShapedValue(shape.pack(v), shape.packedShape), OptionApply(shape.toNode(v)))
    override def toString = s"AnyOptionLift($shape)"
  }

  /** Get a suitably typed base value for a `Rep[Option[_]]` */
  def baseValue[M, O](v: O, path: Node): M = v match {
    case RepOption(base, _) => base.asInstanceOf[ShapedValue[M, _]].encodeRef(path).value
    case r: Rep.TypedRep[_] /* An Option column */ =>
      Rep.columnPlaceholder[Any](r.tpe.asInstanceOf[OptionType].elementType.asInstanceOf[TypedType[Any]]).encodeRef(path).asInstanceOf[M]
  }
}
