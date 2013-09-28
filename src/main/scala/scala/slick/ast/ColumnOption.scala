package scala.slick.ast

import scala.slick.lifted.Column

abstract class ColumnOption[+T]

object ColumnOption {
  case object NotNull extends ColumnOption[Nothing]
  case object Nullable extends ColumnOption[Nothing]
  case object PrimaryKey extends ColumnOption[Nothing]  
  case class DBType(val dbType: String) extends ColumnOption[Nothing]
  case class Default[T](val defaultValue: Column[T]) extends ColumnOption[T]
  case object AutoInc extends ColumnOption[Nothing]
}
