package scala.slick.schema

import scala.slick.jdbc.meta.MQName
import scala.collection.mutable.ArrayBuffer
import scala.slick.SlickException

/**
 * A meta-model for representing name of column, table, schema and catalog
 */
trait Name {
  val name: String
  val kind: NameType
  override def toString(): String = name
}

/**
 * Represents each part of a name
 */
case class NamePart(override val name: String, override val kind: NameType) extends Name

/**
 * Type for each part of a name
 */
sealed trait NameType
case object TableName extends NameType
case object ColumnName extends NameType
case object SchemaName extends NameType
case object CatalogName extends NameType

/**
 * Qualified name for a meta-model of database
 */
case class QualifiedName(parts: List[NamePart]) extends Name {
  override val name = parts.mkString(".")
  override val kind = parts.last.kind

  @inline def getPartName(kind: NameType): String = {
    getPart(kind).map(_.name) getOrElse {
      throw new SlickException(s"This name '$name' was not for '$kind'")
    }
  }

  @inline def getPart(kind: NameType): Option[NamePart] =
    parts find (part => part.kind equals kind)

  def lastPart: String =
    parts.last.name
}

object QualifiedName {
  def apply(mqName: MQName): QualifiedName = {
    import NamePart._
    val parts = new ArrayBuffer[NamePart]
    parts ++=
      mqName.catalog.map(c => NamePart(c, CatalogName)).toList
    parts ++=
      mqName.schema.map(c => NamePart(c, SchemaName)).toList
    parts +=
      NamePart(mqName.name, TableName)
    QualifiedName(parts.toList)
  }

  def tableName(tableName: String): QualifiedName = {
    QualifiedName(List(NamePart(tableName, TableName)))
  }

  def columnName(tableName: QualifiedName, columnName: String): QualifiedName = {
    val parts = tableName.parts :+ NamePart(columnName, ColumnName)
    QualifiedName(parts)
  }
}