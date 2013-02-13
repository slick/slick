package scala.slick.lifted

import scala.slick.driver.JdbcProfile
import scala.slick.ast._
import scala.Some
import scala.slick.ast.SequenceNode
import FunctionSymbolExtensionMethods._

class Sequence[T] private[Sequence] (val name: String,
    val _minValue: Option[T],
    val _maxValue: Option[T],
    val _increment: Option[T],
    val _start: Option[T],
    val _cycle: Boolean)(implicit val tpe: TypedType[T], val integral: Integral[T])
  extends NodeGenerator with Typed { seq =>

  def min(v: T) = new Sequence[T](name, Some(v), _maxValue, _increment, _start, _cycle)
  def max(v: T) = new Sequence[T](name, _minValue, Some(v), _increment, _start, _cycle)
  def inc(v: T) = new Sequence[T](name, _minValue, _maxValue, Some(v), _start, _cycle)
  def start(v: T) = new Sequence[T](name, _minValue, _maxValue, _increment, Some(v), _cycle)
  def cycle = new Sequence[T](name, _minValue, _maxValue, _increment, _start, true)

  final def next = Library.NextValue.column[T](Node(this))
  final def curr = Library.CurrentValue.column[T](Node(this))

  def nodeDelegate = SequenceNode(name)(_increment.map(integral.toLong).getOrElse(1))

  def ddl(implicit profile: JdbcProfile): DDL = profile.buildSequenceDDL(this)
}

object Sequence {
  def apply[T : TypedType : Integral](name: String) = new Sequence[T](name, None, None, None, None, false)
}
