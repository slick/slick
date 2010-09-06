package org.scalaquery.ql

sealed trait OptionMapper2[B1, B2, BR, P1, P2, R] extends (Column[BR] => Column[R])

object OptionMapper2 {
  val plain = new OptionMapper2[Any,Any,Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper2[Any,Any,Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }
}

sealed trait OptionMapper3[B1, B2, B3, BR, P1, P2, P3, R] extends (Column[BR] => Column[R])

object OptionMapper3 {
  val plain = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Any] { def apply(n: Column[Any]): Column[Any] = n }
  val option = new OptionMapper3[Any,Any,Any,Any,Any,Any,Any,Option[Any]] { def apply(n: Column[Any]): Column[Option[Any]] = n.? }
}
