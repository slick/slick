package scala.slick.ql

import TypeMapper._
import scala.slick.ast.{SimpleNode, Node, UnaryNode, BinaryNode}

final class ColumnOps[B1, P1](val leftOperand: Node) extends AnyVal {
  import ColumnOps._
  type BaseTM = BaseTypeMapper[B1]
  type Num = BaseTM with NumericTypeMapper

  import OptionMapperDSL._
  type o = arg[B1, P1]

  def is[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Is(leftOperand, Node(e)))
  def === [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Is(leftOperand, Node(e)))
  def isNot[P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Not(Is(leftOperand, Node(e))))
  @deprecated("Use =!= instead", "0.9.0")
  def != [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Not(Is(leftOperand, Node(e))))
  def =!= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Not(Is(leftOperand, Node(e))))
  def < [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Relational("<", leftOperand, Node(e)))
  def <= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Relational("<=", leftOperand, Node(e)))
  def > [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Relational(">", leftOperand, Node(e)))
  def >= [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[Boolean, R]) =
    om(Relational(">=", leftOperand, Node(e)))
  def inSet[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R], tm: BaseTM) =
    om(InSet(leftOperand, seq, false)(tm))
  def inSetBind[R](seq: Traversable[B1])(implicit om: o#to[Boolean, R], tm: BaseTM) =
    om(InSet(leftOperand, seq, true)(tm))
  def between[P2, P3, R](start: Column[P2], end: Column[P3])(implicit om: o#arg[B1, P2]#arg[B1, P3]#to[Boolean, R]) =
    om(Between(leftOperand, Node(start), Node(end)))
  def ifNull[B2, P2, R](e: Column[P2])(implicit om: o#arg[B2, P2]#to[Boolean, R]): Column[P2] =
    EscFunction[P2]("ifnull", leftOperand, Node(e))(e.typeMapper)
  def min(implicit om: toOption[B1], tm: BaseTM) =
    om(StdFunction[B1]("min", leftOperand))
  def max(implicit om: toOption[B1], tm: BaseTM) =
    om(StdFunction[B1]("max", leftOperand))

  // NumericTypeMapper only
  def + [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Arith[B1]("+", leftOperand, Node(e)))
  def - [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Arith[B1]("-", leftOperand, Node(e)))
  def * [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Arith[B1]("*", leftOperand, Node(e)))
  def / [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(Arith[B1]("/", leftOperand, Node(e)))
  def % [P2, R](e: Column[P2])(implicit om: o#arg[B1, P2]#to[B1, R], tm: Num) =
    om(EscFunction[B1]("mod", leftOperand, Node(e)))
  def abs(implicit tm: Num) =
    EscFunction[B1]("abs", leftOperand)
  def ceil(implicit tm: Num) =
    EscFunction[B1]("ceiling", leftOperand)
  def floor(implicit tm: Num) =
    EscFunction[B1]("floor", leftOperand)
  def sign[R](implicit om: o#to[Int, R], tm: Num) =
    om(EscFunction[Int]("sign", leftOperand))
  def toDegrees(implicit tm: Num) =
    EscFunction[B1]("degrees", leftOperand)
  def toRadians(implicit tm: Num) =
    EscFunction[B1]("radians", leftOperand)
  def avg(implicit om: toOption[B1], tm: Num) =
    om(StdFunction[B1]("avg", leftOperand))
  def sum(implicit om: toOption[B1], tm: Num) =
    om(StdFunction[B1]("sum", leftOperand))

  // Boolean only
  def &&[P2, R](b: Column[P2])(implicit om: arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]) =
    om(And(leftOperand, Node(b)))
  def ||[P2, R](b: Column[P2])(implicit om: arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]) =
    om(Or(leftOperand, Node(b)))
  def unary_!(implicit ev: B1 <:< Boolean) =
    Not(leftOperand)

  // String only
  def length[R](implicit om: arg[String, P1]#to[Int, R]) =
    om(EscFunction[Int]("length", leftOperand))
  def like[P2, R](e: Column[P2])(implicit om: arg[String, P1]#arg[String, P2]#to[Boolean, R]) =
    om(Like(leftOperand, Node(e), None))
  def like[P2, R](e: Column[P2], esc: Char)(implicit om: arg[String, P1]#arg[String, P2]#to[Boolean, R]) =
    om(Like(leftOperand, Node(e), Some(esc)))
  def ++[P2, R](e: Column[P2])(implicit om: arg[String, P1]#arg[String, P2]#to[String, R]) =
    om(EscFunction[String]("concat", leftOperand, Node(e)))
  def startsWith[R](s: String)(implicit om: arg[String, P1]#to[Boolean, R]) =
    om(new StartsWith(leftOperand, s))
  def endsWith[R](s: String)(implicit om: arg[String, P1]#to[Boolean, R]) =
    om(new EndsWith(leftOperand, s))
  def toUpperCase(implicit ev: B1 <:< String) =
    EscFunction[String]("ucase", leftOperand)
  def toLowerCase(implicit ev: B1 <:< String) =
    EscFunction[String]("lcase", leftOperand)
  def ltrim(implicit ev: B1 <:< String) =
    EscFunction[String]("ltrim", leftOperand)
  def rtrim(implicit ev: B1 <:< String) =
    EscFunction[String]("rtrim", leftOperand)
  def trim(implicit ev: B1 <:< String) =
    EscFunction[String]("ltrim", EscFunction[String]("rtrim", leftOperand))
}

object ColumnOps {
  final case class In(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    val name = "in"
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
  final case class CountAll(child: Node) extends OperatorColumn[Int] with UnaryNode {
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  }

  final case class Relational(name: String, left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
  final case class Arith[T : TypeMapper](name: String, left: Node, right: Node) extends OperatorColumn[T] with SimpleBinaryOperator {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy[T](left = left, right = right)
  }

  final case class Is(left: Node, right: Node) extends OperatorColumn[Boolean] with BinaryNode {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
  final case class CountDistinct(child: Node) extends OperatorColumn[Int] with UnaryNode {
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  }
  final case class InSet[T](child: Node, seq: Traversable[T], bind: Boolean)(val tm: TypeMapper[T]) extends OperatorColumn[Boolean] with UnaryNode {
    protected[this] def nodeRebuild(child: Node): Node = copy[T](child = child)()
  }

  final case class Between(left: Node, start: Node, end: Node) extends OperatorColumn[Boolean] with SimpleNode {
    protected[this] def nodeChildGenerators = Seq(left, start, end)
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Node = Between(ch(0), ch(1), ch(2))
  }

  final case class AsColumnOf[T : TypeMapper](child: Node, typeName: Option[String]) extends Column[T] with UnaryNode {
    protected[this] def nodeRebuild(child: Node): Node = copy[T](child = child)
  }

  // Boolean
  final case class And(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    val name = "and"
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
  final case class Or(left: Node, right: Node) extends OperatorColumn[Boolean] with SimpleBinaryOperator {
    val name = "or"
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
  }
  final case class Not(child: Node) extends OperatorColumn[Boolean] with UnaryNode {
    protected[this] def nodeRebuild(child: Node): Node = copy(child = child)
  }

  // String
  sealed case class Like(left: Node, right: Node, esc: Option[Char]) extends OperatorColumn[Boolean] with BinaryNode {
    protected[this] def nodeRebuild(left: Node, right: Node): Node = copy(left = left, right = right)
    override def equals(o: Any) = getClass == o.asInstanceOf[AnyRef].getClass && super.equals(o)
  }
  final class StartsWith(n: Node, s: String) extends Like(n, ConstColumn(likeEncode(s)+'%'), Some('^')) {
    protected[this] override def nodeRebuild(left: Node, right: Node): Node = new StartsWith(left, s)
  }
  final class EndsWith(n: Node, s: String) extends Like(n, ConstColumn('%'+likeEncode(s)), Some('^')) {
    protected[this] override def nodeRebuild(left: Node, right: Node): Node = new EndsWith(left, s)
  }

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for(c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }
}
