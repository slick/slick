import sbt._
import Keys._
import com.typesafe.sbt.sdlc.Plugin.{sdlcSettings, sdlcBase, sdlcCheckDir, sdlc}
import com.novocode.ornate.sbtplugin.OrnatePlugin.autoImport.{ornateBaseDir, ornateSourceDir, ornateTargetDir, ornateResourceDir, ornateSettings, ornate}

object Docs {
  val docDir = SettingKey[File]("docDir", "Base directory for documentation")

  val buildCapabilitiesTable = TaskKey[Unit]("Build the capabilities.csv table for the documentation")

  def versionTag(v: String) = "v" + v // get the tag for a version

  def docSettings = Seq (
    ornateBaseDir := Some(docDir.value),
    ornateSourceDir := Some(docDir.value / "src"),
    ornateTargetDir := Some(docDir.value / "target"),
    // TODO: when moving to slick 1.x start using the `cleanFiles` setting instead of `cleanFIlesTask`
    cleanFilesTask += docDir.value / "target",
    ornateResourceDir := Some(docDir.value / "resources"),
    ornateSettings := Map(
      "version" -> version.value,
      "shortVersion" -> version.value.replaceFirst("""(\d+\.\d+)\.\d+.*""", """$1.x"""),
      "tag" -> versionTag(version.value), // for snippet links
      "branch" -> "master", // for "Edit page" links
      "scalaVersion" -> scalaVersion.value // for "scalaapi:" links
    ),
    ornate :=
      (ornate dependsOn (buildCapabilitiesTable in LocalProject("testkit"))).value,
    aggregate in doc := true,
    aggregate in sdlc := true,
    doc := ornate.value
  )

  def scaladocSettings = sdlcSettings ++ Seq(
    sdlcBase := apiName(projectID.value.name),
    sdlcCheckDir := docDir.value / "target",
    target in doc in Compile := docDir.value / "target" / sdlcBase.value,
    sdlc := (sdlc dependsOn (doc in Compile)).value
  )

  def apiName(projectId: String) = (projectId + "-api/").replaceFirst("^slick-", "")
}
