package com.novocode.squery.meta

import com.novocode.squery.session.PositionedResult

/**
 * A qualified name with an optional catalog and schema.
 */
case class MQName(catalog: Option[String], schema: Option[String], name: String) {
  override def toString = "MQName(" + catalog.map(_ + ".").getOrElse("") + schema.map(_ + ".").getOrElse("") + name + ")"
}

object MQName {
  private[meta] def from(r: PositionedResult) =
    MQName(r.nextStringOption, r.nextStringOption, r.nextString)

  private[meta] def optionalFrom(r: PositionedResult) = {
    val cat = r.nextStringOption
    val schema = r.nextStringOption
    r.nextStringOption map (MQName(cat, schema, _))
  }

  def local(name: String) = MQName(Some(""), Some(""), name)
}
