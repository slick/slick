package com.novocode.squery.combinator.extended

import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.basic.{BasicProfile, BasicImplicitConversions}

trait ExtendedProfile extends BasicProfile {
  type ImplicitT <: ExtendedImplicitConversions[_ <: ExtendedProfile]
}

trait ExtendedImplicitConversions[DriverType <: ExtendedProfile] extends BasicImplicitConversions[DriverType] {
  implicit def queryToExtendedQueryOps[E](q: Query[E]) = new ExtendedQueryOps(q)
}

class ExtendedQueryOps[E](q: Query[E]) {
  import ExtendedQueryOps._

  def take(num: Column[Int]) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(t, d)) => TakeDrop(Some(Node(num)), d)
    case None => TakeDrop(Some(Node(num)), None)
  }
  def drop(num: Column[Int]) = q.createOrReplaceSingularModifier[TakeDrop] {
    case Some(TakeDrop(t, d)) => TakeDrop(t, Some(Node(num)))
    case None => TakeDrop(None, Some(Node(num)))
  }
}

object ExtendedQueryOps {
  final case class TakeDrop(take: Option[Node], drop: Option[Node]) extends QueryModifier {
    def nodeChildren = take.toList ::: drop.toList
    override def nodeNamedChildren = take.map((_,"take")).toList ::: drop.map((_,"drop")).toList
  }
}
