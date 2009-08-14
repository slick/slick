package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator.{Column, StringColumnOps, OptionMapper, Operator, Node, ConstColumn}
import com.novocode.squery.combinator.basic.{BasicProfile, BasicImplicitConversions}
import com.novocode.squery.session.TypeMapper
import com.novocode.squery.session.TypeMapper._

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_]
}

trait ExtendedImplicitConversions[DriverType <: BasicProfile] extends BasicImplicitConversions[DriverType] {
  override implicit def columnOfStringToStringColumnOps(c: ColumnBase[String]): ExtendedStringColumnOps[String] = c match {
    case o: ExtendedStringColumnOps[_] => o.asInstanceOf[ExtendedStringColumnOps[String]]
    case _ => new ExtendedStringColumnOps[String] { protected[this] val leftOperand = Node(c) }
  }

  override implicit def columnOfStringOptionToStringColumnOps(c: ColumnBase[Option[String]]): ExtendedStringColumnOps[Option[String]] = c match {
    case o: ExtendedStringColumnOps[_] => o.asInstanceOf[ExtendedStringColumnOps[Option[String]]]
    case _ => new ExtendedStringColumnOps[Option[String]] { protected[this] val leftOperand = Node(c) }
  }
}

trait ExtendedStringColumnOps[P1] extends StringColumnOps[P1] {
  def ++[P2, R](e: Column[P2])(implicit om: OptionMapper[String, String, String, P1, P2, R]): Column[R] =
    om(ExtendedOperator.Concat(leftOperand, Node(e)))
  def startsWith[P2, R](e: Column[P2])(implicit om: OptionMapper[String, String, Boolean, P1, P2, R]): Column[R] =
    om(ExtendedOperator.StartsEndsWith(leftOperand, Node(e), false))
  def endsWith[P2, R](e: Column[P2])(implicit om: OptionMapper[String, String, Boolean, P1, P2, R]): Column[R] =
    om(ExtendedOperator.StartsEndsWith(leftOperand, Node(e), true))
}

object ExtendedOperator {
  final case class Concat(left: Node, right: Node) extends OperatorColumn[String] with SimpleBinaryOperator with ExtendedStringColumnOps[Boolean] { val name = "||" }
  final case class StartsEndsWith(left: Node, part: Node, ends: Boolean) extends OperatorColumn[Boolean] with SimpleBinaryOperator with BooleanColumnOps[Boolean] {
    val name = "like"
    //TODO: Escape patterns in StartsWith and EndsWith
    lazy val right = part match {
      case c @ ConstColumn(v: String) =>
        ConstColumn(if(ends) "%" + v else v + "%")
      case _ =>
        if(ends) Concat(ConstColumn("%"), part) else Concat(part, ConstColumn("%"))
    }
  }
}
