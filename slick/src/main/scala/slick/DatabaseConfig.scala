package slick

import _root_.cats.effect.Async
import slick.basic.BasicBackend

/** Common interface for database configuration objects that can open a database connection.
  *
  * Both [[slick.basic.BasicDatabaseConfig]] (Typesafe Config-based) and
  * [[slick.jdbc.JdbcDatabaseConfig]] (programmatic JDBC) implement this trait,
  * allowing integration facades to accept either with a single method.
  */
trait DatabaseConfig {

  /** The [[ControlsConfig]] governing connection pool sizing, queue limits, and timeouts
    * for databases opened from this config. */
  def controls: ControlsConfig

  /** Return a copy of this config with the given [[ControlsConfig]] attached.
    *
    * @see [[ControlsConfig]]
    */
  def withControls(c: ControlsConfig): DatabaseConfig

  def makeDatabase[F[_]: Async](): F[BasicBackend#BasicDatabaseDef[F]]
}
