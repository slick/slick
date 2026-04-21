package slick.basic

import cats.effect.Async
import com.typesafe.config.Config
import slick.ControlsConfig

final class BasicDatabaseConfig[P <: BasicProfile](
  val profile: P,
  val config: Config,
  val path: String,
  val rootConfig: Config,
  val classLoader: ClassLoader,
  val loadedProfileName: String,
  override val controls: ControlsConfig = ControlsConfig()
) extends slick.DatabaseConfig {

  def profileName: String = if (profileIsObject) loadedProfileName.substring(0, loadedProfileName.length - 1) else loadedProfileName

  def profileIsObject: Boolean = loadedProfileName.endsWith("$")

  override def withControls(c: ControlsConfig): BasicDatabaseConfig[P] =
    new BasicDatabaseConfig(profile, config, path, rootConfig, classLoader, loadedProfileName, c)

  override def makeDatabase[F[_]: Async](): F[BasicBackend#BasicDatabaseDef[F]] =
    // profile.backend.Database[F] <: BasicBackend#BasicDatabaseDef[F] is guaranteed by the
    // type bounds, but F's invariance prevents the compiler from accepting the upcast directly.
    profile.backend.makeDatabase[F](this).asInstanceOf[F[BasicBackend#BasicDatabaseDef[F]]]
}
