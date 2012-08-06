package scala.slick.lifted

import annotation.implicitNotFound

@implicitNotFound("Cannot perform option-mapped operation\n      with type: (${P1}, ${P2}) => ${R}\n  for base type: (${B1}, ${B2}) => ${BR}")
sealed trait OptionMapper2[B1, B2, BR, P1, P2, R] extends (Column[BR] => Column[R])

object OptionMapper2 {
  val plain = new OptionMapper2[Any,Any,Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper2[Any,Any,Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }

  @inline implicit def getOptionMapper2TT[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.plain .asInstanceOf[OptionMapper2[B1, B2, BR, B1,         B2,         BR]]
  @inline implicit def getOptionMapper2TO[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, B1,         Option[B2], Option[BR]]]
  @inline implicit def getOptionMapper2OT[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], B2,         Option[BR]]]
  @inline implicit def getOptionMapper2OO[B1, B2 : BaseTypeMapper, BR] = OptionMapper2.option.asInstanceOf[OptionMapper2[B1, B2, BR, Option[B1], Option[B2], Option[BR]]]
}

@implicitNotFound("Cannot perform option-mapped operation\n      with type: (${P1}, ${P2}, ${P3}) => ${R}\n  for base type: (${B1}, ${B2}, ${B3}) => ${BR}")
sealed trait OptionMapper3[B1, B2, B3, BR, P1, P2, P3, R] extends (Column[BR] => Column[R])

object OptionMapper3 {
  val plain = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }

  @inline implicit def getOptionMapper3TTT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.plain .asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         B3,         BR]]
  @inline implicit def getOptionMapper3TTO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         B2,         Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3TOT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], B3,         Option[BR]]]
  @inline implicit def getOptionMapper3TOO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, B1,         Option[B2], Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3OTT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         B3,         Option[BR]]]
  @inline implicit def getOptionMapper3OTO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], B2,         Option[B3], Option[BR]]]
  @inline implicit def getOptionMapper3OOT[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], B3,         Option[BR]]]
  @inline implicit def getOptionMapper3OOO[B1, B2 : BaseTypeMapper, B3 : BaseTypeMapper, BR] = OptionMapper3.option.asInstanceOf[OptionMapper3[B1, B2, B3, BR, Option[B1], Option[B2], Option[B3], Option[BR]]]
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
