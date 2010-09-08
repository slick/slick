package org.scalaquery.meta

import org.scalaquery.session.PositionedResult

/**
 * A qualified name with an optional catalog and schema.
 */
case class MQName(catalog: Option[String], schema: Option[String], name: String) {
  override def toString = "MQName(" + catalog.map(_ + ".").getOrElse("") + schema.map(_ + ".").getOrElse("") + name + ")"

  def catalog_? = catalog.orNull
  def schema_? = schema.orNull
}

object MQName {
  private[meta] def from(r: PositionedResult) = MQName(r<<, r<<, r<<)

  private[meta] def optionalFrom(r: PositionedResult) = {
    val cat = r.nextStringOption
    val schema = r.nextStringOption
    r.nextStringOption map (MQName(cat, schema, _))
  }

  def local(name: String) = MQName(Some(""), Some(""), name)
}
