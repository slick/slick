package slick.ast

abstract class ColumnOption[+T]

/** The basic column options that influence profile-independent parts of query compilation.
  * Different profile levels define additional options. */
object ColumnOption {

  /** Mark a column as auto-incrementing / automatically generated. This option is usually combined
    * with `PrimaryKey`. */
  case object AutoInc extends ColumnOption[Nothing]

  /** Mark a column as the table's primary key. For named and composite primary keys you have to
    * use the Table's `primaryKey` method instead. */
  case object PrimaryKey extends ColumnOption[Nothing]

  case object Unique extends ColumnOption[Nothing]
}
