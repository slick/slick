package com.novocode.squery.combinator

import java.io.PrintWriter


trait Operator { self: Column[_] =>
  def children: List[Column[_]]

  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
    val n = getClass.getName.replaceFirst(".*\\.", "")
    out.println(prefix+name+"Operators."+n)
    def f(l: List[Column[_]], i: Int): Unit = l match {
      case Nil => ()
      case x :: xs =>
        dumper(out, prefix+"  ", "["+i+"]: ", x)
        f(xs, i+1)
    }
    f(children, 0)
  }
}

object Operator {
  final case class Is(left: Column[_], right: Column[_]) extends BooleanColumn with BinaryOperator[Column[_], Column[_]]
  final case class In(left: Column[_], right: Column[_]) extends BooleanColumn with BinaryOperator[Column[_], Column[_]]
  final case class And(left: BooleanColumn, right: BooleanColumn) extends BooleanColumn with BinaryOperator[BooleanColumn, BooleanColumn]
  final case class Or(left: BooleanColumn, right: BooleanColumn) extends BooleanColumn with BinaryOperator[BooleanColumn, BooleanColumn]
  final case class Count(e: Column[_]) extends IntColumn with UnaryOperator[Column[_]]
  final case class Max(e: Column[_]) extends IntColumn with UnaryOperator[Column[_]]
  final case class Not(e: BooleanColumn) extends BooleanColumn with UnaryOperator[BooleanColumn]
}

trait BinaryOperator[L <: Column[_], R <: Column[_]] extends Operator { self: Column[_] =>
  val left: L
  val right: R
  def children = left :: right :: Nil
}

trait UnaryOperator[E <: Column[_]] extends Operator { self: Column[_] =>
  val e: E
  def children = e :: Nil
}
