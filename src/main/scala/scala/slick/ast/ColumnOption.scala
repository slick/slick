package scala.slick.ast

abstract class ColumnOption[+T]

object ColumnOption {
  case object NotNull extends ColumnOption[Nothing]
  case object Nullable extends ColumnOption[Nothing]
  case object PrimaryKey extends ColumnOption[Nothing]
  /** Default value for the column. Needs to wrap an Option for nullable Columns. */
  case class Default[T](val defaultValue: T) extends ColumnOption[T]
  /** Type as expected by the DBMS, e.g. VARCHAR or VARCHAR(254).
    * Note that Slick's model omits the optional length ascription for string columns here
    * and carries the length in the separate ColumnOption Length instead.
    * A length ascription for string column is allowed though and can be used in a 
    * Slick Table subclass to pass it to the DBMS.
    *
    * As this is the type of the underlying DBMS it may not be portable to other DBMS.
    *
    * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String
    * columns if neither ColumnOption DBType nor Length are given.
    *
    */
  case class DBType(val dbType: String) extends ColumnOption[Nothing]
  case object AutoInc extends ColumnOption[Nothing]
  /** Number of unicode characters for string-like types
    *
    * Unlike DBType this is portable between different DBMS.
    *
    * Note that for DDL Slick currently uses picks type CHAR
    * when varying=false and VARCHAR when varying=true.
    *
    * Note that Slick uses VARCHAR or VARCHAR(254) in DDL for String
    * columns if neither ColumnOption DBType nor Length are given.
    *
    * @param varying indicates wether this is just the maximum length of a varying
    */
  case class Length[T <: String](length: Int, varying: Boolean) extends ColumnOption[T]
}
