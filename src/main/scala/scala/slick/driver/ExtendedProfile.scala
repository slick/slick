package scala.slick.driver

import scala.slick.ql.ColumnOption

trait ExtendedProfile extends BasicProfile with ExtendedTableComponent { driver: ExtendedDriver => }

trait ExtendedDriver extends ExtendedProfile with BasicDriver {
  override val profile: ExtendedProfile = this
}

trait ExtendedTableComponent extends BasicTableComponent { driver: ExtendedDriver =>
  override val columnOptions: ExtendedColumnOptions = new ExtendedColumnOptions

  class ExtendedColumnOptions extends BasicColumnOptions {
    val AutoInc = ColumnOption.AutoInc
  }
}
