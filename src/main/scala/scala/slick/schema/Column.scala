package scala.slick.schema

import scala.slick.schema.naming.Naming
import scala.reflect.api.Universe

/**
 * A meta-model for each column of relation
 */
case class Column(name: QualifiedName, tpe: Universe#Type, moduleFieldName: String, caseFieldName: String) {
  override def toString = s"$name: $tpe"
}
