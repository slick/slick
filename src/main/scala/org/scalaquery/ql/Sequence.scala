package org.scalaquery.ql

import org.scalaquery.ql.basic.BasicProfile
import org.scalaquery.util.UnaryNode

class Sequence[T] private[Sequence] (val name: String,
    val _minValue: Option[T],
    val _maxValue: Option[T],
    val _increment: Option[T],
    val _start: Option[T],
    val _cycle: Boolean)(implicit val typeMapper: TypeMapper[T], val integral: Integral[T]) { seq =>

  def min(v: T) = new Sequence[T](name, Some(v), _maxValue, _increment, _start, _cycle)
  def max(v: T) = new Sequence[T](name, _minValue, Some(v), _increment, _start, _cycle)
  def inc(v: T) = new Sequence[T](name, _minValue, _maxValue, Some(v), _start, _cycle)
  def start(v: T) = new Sequence[T](name, _minValue, _maxValue, _increment, Some(v), _cycle)
  def cycle = new Sequence[T](name, _minValue, _maxValue, _increment, _start, true)

  final def next = Sequence.Nextval(this)

  final def curr = Sequence.Currval(this)

  def ddl(implicit profile: BasicProfile): DDL = profile.buildSequenceDDL(this)
}

object Sequence {
  def apply[T : TypeMapper : Integral](name: String) = new Sequence[T](name, None, None, None, None, false)

  final case class Nextval[T : TypeMapper](seq: Sequence[T]) extends OperatorColumn[T] with SimpleFunction with UnaryNode {
    val name = "nextval"
    val child = ConstColumn(seq.name)(TypeMapper.StringTypeMapper)
  }

  final case class Currval[T : TypeMapper](seq: Sequence[T]) extends OperatorColumn[T] with SimpleFunction with UnaryNode {
    val name = "currval"
    val child = ConstColumn(seq.name)(TypeMapper.StringTypeMapper)
  }
}
