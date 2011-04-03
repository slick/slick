import sbt._
import scala.collection.Set
import scala.xml._
import java.io.{File, FileOutputStream}
import java.nio.channels.Channels

class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info)
{
  /*********** Options ***********/
  override def compileOptions = Deprecation :: super.compileOptions.toList
  override def documentOptions: Seq[ScaladocOption] = documentTitle("ScalaQuery " + version) :: Nil
  override def testOptions = super.testOptions ++ Seq(TestArgument(TestFrameworks.JUnit, "-q", "-v"))

  val useJDBC4 = try { classOf[java.sql.DatabaseMetaData].getMethod("getClientInfoProperties"); true }
    catch { case _:NoSuchMethodException => false }

  /*********** Dependencies ***********/
  //val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val h2 = "com.h2database" % "h2" % "1.2.140" % "test->default"
  val sqlite = "org.xerial" % "sqlite-jdbc" % "3.6.20" % "test->default"
  val derby = "org.apache.derby" % "derby" % "10.6.1.0" % "test->default"
  val hsqldb = "org.hsqldb" % "hsqldb" % "2.0.0" % "test->default"
  val postgresql = "postgresql" % "postgresql" % (if(useJDBC4) "8.4-701.jdbc4" else "8.4-701.jdbc3") % "test->default"
  val mysql = "mysql" % "mysql-connector-java" % "5.1.13" % "test->default"
  val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test->default"
  val fmppDep = "net.sourceforge.fmpp" % "fmpp" % "0.9.13" % "fmpp"
  val fmppConf = config("fmpp") hide

  /*********** Run FMPP before compiling ***********/
  lazy val fmpp =
    runTask(Some("fmpp.tools.CommandLine"), configurationClasspath(fmppConf),
      "--expert" :: "-q" :: "-S" :: "src/main/scala" :: "-O" :: "src/main/scala" ::
        "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil)
  override def compileAction = super.compileAction dependsOn(fmpp)

  /*********** Publishing ***********/
  val publishTo = Resolver.file("ScalaQuery Test Repo", new File("d:/temp/repo/"))
  //val publishTo = "Scala Tools Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  //val publishTo = "Scala Tools Releases" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  //def specificSnapshotRepo =
  //  Resolver.url("scala-nightly").
  //  artifacts("http://scala-tools.org/repo-snapshots/[organization]/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]").
  //  mavenStyle()
  //val nightlyScala = ModuleConfiguration("org.scala-lang", "*", "2.8.0-.*", specificSnapshotRepo)
  //override def deliverScalaDependencies = Nil
  override def managedStyle = ManagedStyle.Maven
  override def packageDocsJar = defaultJarPath("-javadocs.jar")
  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)
  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadocs"), Nil, None)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  /*********** Extra meta-data for the POM ***********/
  override def pomExtra =
    (<name>ScalaQuery</name>
    <url>http://scalaquery.org/</url>
    <inceptionYear>2008</inceptionYear>
    <description>A type-safe database API for Scala</description>
    <licenses>
      <license>
        <name>Two-clause BSD-style license</name>
        <url>http://github.com/szeiger/scala-query/blob/master/LICENSE.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>szeiger</id>
        <name>Stefan Zeiger</name>
        <timezone>+1</timezone>
        <email>szeiger [at] novocode.com</email>
      </developer>
    </developers>
    <scm>
      <url>http://github.com/szeiger/scala-query/</url>
    </scm>)
}
