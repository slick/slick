import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.site.SphinxSupport._
import sbt.Keys._
import sbt._

object SphinxDoc extends AutoPlugin {

  override def projectSettings = site.settings ++ site.sphinxSupport() ++ Seq(
    sourceDirectory in Sphinx := baseDirectory.value / "rst",
    watchSources <++= (sourceDirectory in Sphinx, excludeFilter in Global) map { (source, excl) =>
      source.descendantsExcept("*.rst", excl).get
    },
    sphinxEnv in Sphinx ++= Map(
      "version" -> version.value.replaceFirst( """(\d*.\d*).*""", """$1"""),
      "release" -> version.value
    ),
    sphinxProperties in Sphinx := Map.empty,
    site.addMappingsToSiteDir(mappings in packageDoc in Compile in LocalProject(SlickBuild.slick.id), "api"),
    site.addMappingsToSiteDir(mappings in packageDoc in Compile in LocalProject(SlickBuild.slickDirect.id), "direct-api"),
    site.addMappingsToSiteDir(mappings in packageDoc in Compile in LocalProject(SlickBuild.slickCodegen.id), "codegen-api"),
    site.addMappingsToSiteDir(mappings in packageDoc in Compile in LocalProject(SlickBuild.slickTestkit.id), "testkit-api")
  )
}
