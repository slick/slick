package scala.slick.driver

trait ExtendedProfile extends BasicProfile with ExtendedTableComponent { driver: ExtendedDriver => }

trait ExtendedDriver extends ExtendedProfile with BasicDriver {
  override val profile: ExtendedProfile = this
}

trait ExtendedTableComponent extends BasicTableComponent { driver: ExtendedDriver =>
  override val columnOptions: ExtendedColumnOptions = new ExtendedColumnOptions

  class ExtendedColumnOptions extends BasicColumnOptions
}
