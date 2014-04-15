package scala.slick.driver

import scala.slick.profile.SqlTableComponent
import scala.slick.ast.ColumnOption

/** The part of the driver cake that handles JDBC-specific table definitions.
  * It provides new column options for the standard relational table definitions
  * from `RelationalTableComponen`. */
trait JdbcTableComponent extends SqlTableComponent { driver: JdbcDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}
