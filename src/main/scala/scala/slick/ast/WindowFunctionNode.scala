package scala.slick.ast

import scala.slick.SlickException
import scala.slick.lifted.Column

object FrameStartEndType extends Enumeration {
  val unboundedPreceding = Value
  val unboundedFollowing = Value
  val precedingN = Value
  val followingN = Value
  val currentRow = Value
}

object FrameType extends Enumeration {
  val rowFrame = Value
  val rangeFrame = Value
}

trait FrameStartEnd {
  val frameStartEndType: FrameStartEndType.Value
}

object UnboundedPreceding extends FrameStartEnd {
  override val frameStartEndType: FrameStartEndType.Value = FrameStartEndType.unboundedPreceding
}

object UnboundedFollowing extends FrameStartEnd {
  override val frameStartEndType: FrameStartEndType.Value = FrameStartEndType.unboundedFollowing
}

trait ValueHolder extends FrameStartEnd {
  val value: Column[Int]
}

case class PrecedingN(val value: Column[Int]) extends ValueHolder {
  override val frameStartEndType: FrameStartEndType.Value = FrameStartEndType.precedingN
}

case class FollowingN(val value: Column[Int]) extends ValueHolder {
  override val frameStartEndType: FrameStartEndType.Value = FrameStartEndType.followingN
}

object CurrentRow extends FrameStartEnd {
  override val frameStartEndType: FrameStartEndType.Value = FrameStartEndType.currentRow
}

trait AbstractWindowFrame {
  val frameType: FrameType.Value
  val start: FrameStartEnd
  val end: FrameStartEnd
  val allowedType4Start = Seq(FrameStartEndType.currentRow, FrameStartEndType.unboundedPreceding)
  val allowedType4End = Seq(FrameStartEndType.currentRow, FrameStartEndType.unboundedFollowing)
}

case class RowFrame(override val start: FrameStartEnd = UnboundedPreceding, override val end: FrameStartEnd = CurrentRow) extends AbstractWindowFrame {
  if (!allowedType4Start.++(Seq(FrameStartEndType.precedingN)).exists(_ == start.frameStartEndType))
    throw new SlickException("Invalid Start Frame type.")
  if (!allowedType4End.++(Seq(FrameStartEndType.followingN)).exists(_ == end.frameStartEndType))
    throw new SlickException("Invalid End Frame type.")
  override val frameType: FrameType.Value = FrameType.rowFrame
}

case class RangeFrame(override val start: FrameStartEnd = UnboundedPreceding, override val end: FrameStartEnd = CurrentRow) extends AbstractWindowFrame {
  if (!allowedType4Start.exists(_ == start.frameStartEndType))
    throw new SlickException("Invalid Start Frame type.")
  if (!allowedType4End.exists(_ == end.frameStartEndType))
    throw new SlickException("Invalid End Frame type.")
  override val frameType: FrameType.Value = FrameType.rangeFrame
}

final case class WindowFunctionNode(from: Node, val frameType: FrameType.Value, val frameStartType: FrameStartEndType.Value, val precedingValue: Option[Node], val frameEndType: FrameStartEndType.Value, val followingValue: Option[Node]) extends SimplyTypedNode {
  type Self = WindowFunctionNode

  override def nodeChildNames = Seq("from") ++ precedingValue.map(_ => "precedingN") ++ followingValue.map(_ => "followingN")

  override protected def buildType: Type = from.nodeType

  override def nodeChildren: Seq[Node] = Seq(from) ++ precedingValue ++ followingValue

  override protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
    val iterator = ch.iterator
    copy(
      iterator.next(),
      frameType,
      frameStartType,
      precedingValue.map(_ => iterator.next()),
      frameEndType,
      followingValue.map(_ => iterator.next()))
  }
}


