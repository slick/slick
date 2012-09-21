package scala.slick.driver

trait ExtendedProfile extends JdbcProfile with ExtendedTableComponent { driver: ExtendedDriver => }

trait ExtendedDriver extends ExtendedProfile with JdbcDriver {
  override val profile: ExtendedProfile = this
}

trait ExtendedTableComponent extends JdbcTableComponent { driver: ExtendedDriver =>
  override val columnOptions: ExtendedColumnOptions = new ExtendedColumnOptions

  class ExtendedColumnOptions extends JdbcColumnOptions
}
