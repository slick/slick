import sbt._

object Osgi {
  lazy val osgiBundleFiles =
    taskKey[Seq[File]]("osgi-bundles that our tests rely on using.")

  /** Create an OSGi version range for standard Scala / Typesafe versioning
    * schemes that describes binary compatible versions. */
  def osgiVersionRange(version: String, requireMicro: Boolean = false): String =
    if(version contains '-') "${@}" // M, RC or SNAPSHOT -> exact version
    else if(requireMicro) "${range;[===,=+)}" // At least the same micro version
    else "${range;[==,=+)}" // Any binary compatible version

  /** Create an OSGi Import-Package version specification. */
  def osgiImport(pattern: String, version: String, requireMicro: Boolean = false): String =
    pattern + ";version=\"" + osgiVersionRange(version, requireMicro) + "\""
}
