package scala.slick.driver

import scala.slick.profile.RelationalTableComponent
import scala.slick.ast.ColumnOption

trait JdbcTableComponent extends RelationalTableComponent { driver: JdbcDriver =>

  trait ColumnOptions extends super.ColumnOptions {
    val NotNull = ColumnOption.NotNull
    val Nullable = ColumnOption.Nullable
    def DBType(dbType: String) = ColumnOption.DBType(dbType)
  }

  override val columnOptions: ColumnOptions = new AnyRef with ColumnOptions
}
