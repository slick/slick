package scala.slick.driver

import scala.slick.profile.SqlTableComponent
import scala.slick.ast.ColumnOption

trait JdbcTableComponent extends SqlTableComponent { driver: JdbcDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}
