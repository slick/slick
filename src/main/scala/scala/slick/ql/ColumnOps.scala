package scala.slick.ql

import scala.slick.ast._

final class ColumnOps[B1, P1](val leftOperand: Node) extends AnyVal {
  import ColumnOps._
  type BaseTM = BaseTypeMapper[B1]
  type Num = BaseTM with NumericTypeMapper

  import OptionMapperDSL._
  type o = arg[B1, P1]

  def is[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.==.column(leftOperand, Node(e)))
  def === [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.==.column(leftOperand, Node(e)))
  def isNot[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](leftOperand, Node(e))))
  @deprecated("Use =!= instead", "0.9.0")
  def != [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](leftOperand, Node(e))))
  def =!= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.==.typed[Boolean](leftOperand, Node(e))))

  def < [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.<.column(leftOperand, Node(e)))
  def <= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.<=.column(leftOperand, Node(e)))
  def > [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.>.column(leftOperand, Node(e)))
  def >= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.>=.column(leftOperand, Node(e)))

  def in[P2, R](e: Query[Column[P2], _])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.In.column(leftOperand, Node(e)))
  def notIn[P2, R](e: Query[Column[P2], _])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Library.Not.column(Library.In.typed[Boolean](leftOperand, Node(e))))
  def inSet[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R], tm: BaseTM) = om(
    if(seq.isEmpty) ConstColumn.FALSE
    else Library.In.column(leftOperand, ProductNode(seq.map(LiteralNode.apply _).toSeq)))
  def inSetBind[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R], tm: BaseTM) = om(
    if(seq.isEmpty) ConstColumn.FALSE
    else Library.In.column(leftOperand, ProductNode(seq.map(v => BindColumn(v)(tm)).toSeq)))

  def between[P2, P3, R](start: Column[P2], end: Column[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om(Library.Between.column(leftOperand, Node(start), Node(end)))
  def ifNull[B2, P2, R](e: Column[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Column[P2] =
    Library.IfNull.column[P2](leftOperand, Node(e))(e.typeMapper)
  def min(implicit om: toOption[B1], tm: BaseTM) =
    om(Library.Min.column[B1](leftOperand))
  def max(implicit om: toOption[B1], tm: BaseTM) =
    om(Library.Max.column[B1](leftOperand))

  // NumericTypeMapper only
  def + [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Library.+.column[B1](leftOperand, Node(e)))
  def - [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Library.-.column[B1](leftOperand, Node(e)))
  def * [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Library.*.column[B1](leftOperand, Node(e)))
  def / [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Library./.column[B1](leftOperand, Node(e)))
  def % [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Library.%.column[B1](leftOperand, Node(e)))
  def abs(implicit om: o#toSame, tm: Num) =
    om(Library.Abs.column[B1](leftOperand))
  def ceil(implicit om: o#toSame, tm: Num) =
    om(Library.Ceiling.column[B1](leftOperand))
  def floor(implicit om: o#toSame, tm: Num) =
    om(Library.Floor.column[B1](leftOperand))
  def sign[R](implicit om: o#to[Int, R], tm: Num) =
    om(Library.Sign.column[Int](leftOperand))
  def toDegrees(implicit om: o#toSame, tm: Num) =
    om(Library.Degrees.column[B1](leftOperand))
  def toRadians(implicit om: o#toSame, tm: Num) =
    om(Library.Radians.column[B1](leftOperand))
  def avg(implicit om: toOption[B1], tm: Num) =
    om(Library.Avg.column[B1](leftOperand))
  def sum(implicit om: toOption[B1], tm: Num) =
    om(Library.Sum.column[B1](leftOperand))

  // Boolean only
  def &&[P2, R](b: Column[P2])(implicit om: arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]) =
    om(Library.And.column(leftOperand, Node(b)))
  def ||[P2, R](b: Column[P2])(implicit om: arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]) =
    om(Library.Or.column(leftOperand, Node(b)))
  def unary_!(implicit ev: B1 <:< Boolean) =
    Library.Not.column[Boolean](leftOperand)

  // String only
  def length[R](implicit om: arg[String, P1]#to[Int, R]) =
    om(Library.Length.column[Int](leftOperand))
  def like[P2, R](e: Column[P2], esc: Char = '\0')(implicit om: arg[String, P1]#arg[String, P2]#to[Boolean, R]) = om(
    if(esc == '\0') Library.Like.column(leftOperand, Node(e))
    else Library.Like.column(leftOperand, Node(e), LiteralNode(esc)))
  def ++[P2, R](e: Column[P2])(implicit om: arg[String, P1]#arg[String, P2]#to[String, R]) =
    om(Library.Concat.column[String](leftOperand, Node(e)))
  def startsWith[R](s: String)(implicit om: arg[String, P1]#to[Boolean, R]) =
    om(Library.StartsWith.column(leftOperand, LiteralNode(s)))
  def endsWith[R](s: String)(implicit om: arg[String, P1]#to[Boolean, R]) =
    om(Library.EndsWith.column(leftOperand, LiteralNode(s)))
  def toUpperCase[R](implicit om: o#to[String, R], ev: B1 <:< String) =
    om(Library.UCase.column[String](leftOperand))
  def toLowerCase[R](implicit om: o#to[String, R], ev: B1 <:< String) =
    om(Library.LCase.column[String](leftOperand))
  def ltrim[R](implicit om: o#to[String, R], ev: B1 <:< String) =
    om(Library.LTrim.column[String](leftOperand))
  def rtrim[R](implicit om: o#to[String, R], ev: B1 <:< String) =
    om(Library.RTrim.column[String](leftOperand))
  def trim[R](implicit om: o#to[String, R], ev: B1 <:< String) =
    om(Library.Trim.column[String](leftOperand))
}

object ColumnOps {
  //TODO remove when not used by SlickBackend anymore
  final case class Relational(name: String, left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
}
