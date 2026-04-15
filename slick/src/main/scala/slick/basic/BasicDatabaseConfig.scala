package slick.basic

import com.typesafe.config.Config

final class BasicDatabaseConfig[P <: BasicProfile](
  val profile: P,
  val config: Config,
  val path: String,
  val rootConfig: Config,
  val classLoader: ClassLoader,
  val loadedProfileName: String
) {

  def profileName: String = if (profileIsObject) loadedProfileName.substring(0, loadedProfileName.length - 1) else loadedProfileName

  def profileIsObject: Boolean = loadedProfileName.endsWith("$")
}
