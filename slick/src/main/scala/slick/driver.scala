package slick

package object driver {
  @deprecated("Use type `slick.jdbc.JdbcProfile` instead of `slick.driver.JdbcDriver`", "3.2")
  type JdbcDriver = slick.jdbc.JdbcProfile

  @deprecated("Use type `slick.jdbc.JdbcProfile` instead of `slick.driver.JdbcProfile`", "3.2")
  type JdbcProfile = slick.jdbc.JdbcProfile
  object JdbcProfile {
    @deprecated("Use object `slick.jdbc.JdbcCapabilities` instead of `slick.driver.JdbcProfile.capabilities`", "3.2")
    val capabilities = slick.jdbc.JdbcCapabilities
  }

  @deprecated("Use type `slick.jdbc.DerbyProfile` instead of `slick.driver.DerbyDriver`", "3.2")
  type DerbyDriver = slick.jdbc.DerbyProfile
  @deprecated("Use object `slick.jdbc.DerbyProfile` instead of `slick.driver.DerbyDriver`", "3.2")
  val DerbyDriver = slick.jdbc.DerbyProfile

  @deprecated("Use type `slick.jdbc.H2Profile` instead of `slick.driver.H2Driver`", "3.2")
  type H2Driver = slick.jdbc.H2Profile
  @deprecated("Use object `slick.jdbc.H2Profile` instead of `slick.driver.H2Driver`", "3.2")
  val H2Driver = slick.jdbc.H2Profile

  @deprecated("Use type `slick.jdbc.HsqldbProfile` instead of `slick.driver.HsqldbDriver`", "3.2")
  type HsqldbDriver = slick.jdbc.HsqldbProfile
  @deprecated("Use object `slick.jdbc.HsqldbProfile` instead of `slick.driver.HsqldbDriver`", "3.2")
  val HsqldbDriver = slick.jdbc.HsqldbProfile

  @deprecated("Use type `slick.jdbc.MySQLProfile` instead of `slick.driver.MySQLDriver`", "3.2")
  type MySQLDriver = slick.jdbc.MySQLProfile
  @deprecated("Use object `slick.jdbc.MySQLProfile` instead of `slick.driver.MySQLDriver`", "3.2")
  val MySQLDriver = slick.jdbc.MySQLProfile

  @deprecated("Use type `slick.jdbc.PostgresProfile` instead of `slick.driver.PostgresDriver`", "3.2")
  type PostgresDriver = slick.jdbc.PostgresProfile
  @deprecated("Use object `slick.jdbc.PostgresProfile` instead of `slick.driver.PostgresDriver`", "3.2")
  val PostgresDriver = slick.jdbc.PostgresProfile

  @deprecated("Use type `slick.jdbc.SQLiteProfile` instead of `slick.driver.SQLiteDriver`", "3.2")
  type SQLiteDriver = slick.jdbc.SQLiteProfile
  @deprecated("Use object `slick.jdbc.SQLiteProfile` instead of `slick.driver.SQLiteDriver`", "3.2")
  val SQLiteDriver = slick.jdbc.SQLiteProfile
}
