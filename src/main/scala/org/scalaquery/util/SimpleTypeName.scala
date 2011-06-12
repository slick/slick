package org.scalaquery.util

/**
 * Get a simple type name for a value.
 */

object SimpleTypeName {
  def forVal(v: Any) = v match {
    case null => "Null"
    case _: Boolean => "Boolean"
    case _: Byte => "Byte"
    case _: Char => "Char"
    case _: Double => "Double"
    case _: Float => "Float"
    case _: Int => "Int"
    case _: Long => "Long"
    case _: Short => "Short"
    case _: Unit => "Unit"
    case _ => v.asInstanceOf[AnyRef].getClass.getName
  }
}
