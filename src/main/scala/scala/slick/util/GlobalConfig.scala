package scala.slick.util

import com.typesafe.config.{Config, ConfigFactory}

/** Singleton object with Slick's configuration, loaded from the application config.
  * This includes configuration for the global driver objects and settings for debug logging. */
object GlobalConfig {
  private[this] val config = ConfigFactory.load()

  /** Dump individual `Select` and `Ref` nodes instead of a single `Path` */
  val dumpPaths = config.getBoolean("slick.dumpPaths")

  /** Use ANSI color sequences in tree dumps */
  val ansiDump = config.getBoolean("slick.ansiDump")

  /** Get a `Config` object for a Slick driver */
  def driverConfig(name: String): Config = {
    val path = "slick.driver." + name
    if(config.hasPath(path)) config.getConfig(path) else ConfigFactory.empty()
  }
}
