package slick.lifted

import annotation.implicitNotFound
import slick.ast.OptionApply



@implicitNotFound("Cannot perform option-mapped operation\n      with type: ${R}\n  for base type: ${BR}")
trait TupleOptionMapper[BR, R] extends OptionMapper[BR, R]

object TupleOptionMapper {

  val plain = new TupleOptionMapper[Any,Any] {
    def apply(n: Rep[Any]): Rep[Any] = n
    def lift = false
    override def toString = "TupleOptionMapper.plain"
  }
  val option = new TupleOptionMapper[Any,Option[Any]] {
    def apply(n: Rep[Any]): Rep[Option[Any]] = Rep.forNode(OptionApply(n.toNode))(n.asInstanceOf[Rep.TypedRep[Any]].tpe.optionType)
    def lift = true
    override def toString = "TupleOptionMapper.option"
  }

  @inline implicit def getPlain[BR] = plain.asInstanceOf[TupleOptionMapper[BR, BR]]
  @inline implicit def getOption[BR] = option.asInstanceOf[TupleOptionMapper[BR, Option[BR]]]
}
