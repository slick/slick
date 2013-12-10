package scala.slick
/** Specific database drivers */
package object driver {

  @deprecated("Use JdbcProfile instead of ExtendedProfile", "1.1")
  type ExtendedProfile = JdbcProfile

  @deprecated("Use JdbcDriver instead of ExtendedDriver", "1.1")
  type ExtendedDriver = JdbcDriver
}
