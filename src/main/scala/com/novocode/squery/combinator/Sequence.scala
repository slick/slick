package com.novocode.squery.combinator

class Sequence[T] private[Sequence] (tm: TypeMapper[_,T],
    val name: String,
    val _minValue: Option[T],
    val _maxValue: Option[T],
    val _increment: Option[T],
    val _start: Option[T],
    val _cycle: Boolean) { seq =>

  def min(v: T) = new Sequence[T](tm, name, Some(v), _maxValue, _increment, _start, _cycle)
  def max(v: T) = new Sequence[T](tm, name, _minValue, Some(v), _increment, _start, _cycle)
  def inc(v: T) = new Sequence[T](tm, name, _minValue, _maxValue, Some(v), _start, _cycle)
  def start(v: T) = new Sequence[T](tm, name, _minValue, _maxValue, _increment, Some(v), _cycle)
  def cycle = new Sequence[T](tm, name, _minValue, _maxValue, _increment, _start, true)

  final object next extends OperatorColumn[T]()(tm) with SimpleFunction with UnaryNode {
    val name = "nextval"
    val child = ConstColumn(seq.name)(TypeMapper.StringTypeMapper)
  }

  final object curr extends OperatorColumn[T]()(tm) with SimpleFunction with UnaryNode {
    val name = "currval"
    val child = ConstColumn(seq.name)(TypeMapper.StringTypeMapper)
  }
}

object Sequence {
  def apply[T](name: String)(implicit tm: TypeMapper[_,T]) =
    new Sequence[T](tm, name, None, None, None, None, false)
}
