import sbt._
import Keys._
import com.typesafe.sbt.sdlc.SDLCPlugin.autoImport.{sdlc, sdlcBase, sdlcCheckDir}
import com.novocode.ornate.sbtplugin.OrnatePlugin.autoImport.{ornateBaseDir, ornateSourceDir, ornateTargetDir, ornateResourceDir, ornateSettings, ornate}

object Docs {
  val docDir = settingKey[File]("Base directory for documentation")

  val buildCapabilitiesTable = taskKey[Unit]("Build the capabilities.csv table for the documentation")

  def versionTag(v: String) = "v" + v // get the tag for a version

  def docSettings = Seq (
    ornateBaseDir := Some(docDir.value),
    ornateSourceDir := Some(docDir.value / "src"),
    ornateTargetDir := Some(docDir.value / "target"),
    // TODO: when moving to slick 1.x start using the `cleanFiles` setting instead of `cleanFIlesTask`
    cleanFiles += docDir.value / "target",
    ornateResourceDir := Some(docDir.value / "resources"),
    ornateSettings := Map(
      "version" -> version.value,
      "shortVersion" -> version.value.replaceFirst("""(\d+\.\d+)\.\d+.*""", """$1.x"""),
      "tag" -> versionTag(version.value), // for snippet links
      "branch" -> "master", // for "Edit page" links
      "scalaVersion" -> scalaVersion.value // for "scalaapi:" links
    ),
    ornate :=
      (ornate dependsOn (LocalProject("testkit") / buildCapabilitiesTable)).value,
    doc / aggregate := true,
    sdlc / aggregate := true,
    doc := ornate.value
  )

  def scaladocSettings = Seq(
    sdlcBase := apiName(projectID.value.name),
    sdlcCheckDir := docDir.value / "target",
    Compile / doc / target := docDir.value / "target" / sdlcBase.value,
    sdlc :=
      (sdlc dependsOn (Compile / doc)).value
  )

  def apiName(projectId: String) = (projectId + "-api/").replaceFirst("^slick-", "")
}
