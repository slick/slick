import sbt.Keys._
import sbt._

object Publish extends AutoPlugin {
  override val trigger = allRequirements

  override val projectSettings = Seq(
    publishTo := {
      if(isSnapshot.value) Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
      else Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) },
    pomExtra := pomExtraXml
  )

  private val pomExtraXml = {
    <scm>
      <url>git@github.com:slick/slick.git</url>
      <connection>scm:git:git@github.com:slick/slick.git</connection>
    </scm> ++ makeDevelopersXml(
      ("szeiger", "Stefan Zeiger", "http://szeiger.de"),
      ("cvogt", "Jan Christopher Vogt", "https://github.com/cvogt")
    )
  }

  private def makeDevelopersXml(developers: (String, String, String)*) = {
    <developers>
      {
      for ((id, name, url) <- developers) yield
        <developer>
          <id>{id}</id>
          <name>{name}</name>
          <url>{url}</url>
        </developer>
      }
    </developers>
  }
}
