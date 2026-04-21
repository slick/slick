package slick.jdbc

import cats.effect.Async
import slick.ControlsConfig

final class JdbcDatabaseConfig[P <: JdbcProfile](
  val profile: P,
  val source: JdbcDataSource,
  override val controls: ControlsConfig = ControlsConfig()
) extends slick.DatabaseConfig {

  def profileName: String = profile.getClass.getName.stripSuffix("$")

  def profileIsObject: Boolean = profile.getClass.getName.endsWith("$")

  override def withControls(c: ControlsConfig): JdbcDatabaseConfig[P] =
    new JdbcDatabaseConfig(profile, source, c)

  override def makeDatabase[F[_]: Async](): F[slick.basic.BasicBackend#BasicDatabaseDef[F]] =
    // profile.backend.Database[F] <: BasicBackend#BasicDatabaseDef[F] is guaranteed by the
    // type bounds, but F's invariance prevents the compiler from accepting the upcast directly.
    profile.backend.makeDatabase[F](this).asInstanceOf[F[slick.basic.BasicBackend#BasicDatabaseDef[F]]]
}
