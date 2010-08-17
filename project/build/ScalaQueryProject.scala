import sbt._
import scala.collection.Set
import scala.xml._
import java.io.{File, FileOutputStream}
import java.nio.channels.Channels

class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info)
{
  /*********** Options ***********/
  override def compileOptions = Deprecation :: super.compileOptions.toList
  override def documentOptions: Seq[ScaladocOption] =
    /* LinkSource ::
    documentTitle(name + " " + version + " API") :: 
    windowTitle(name + " " + version + " API") :: */ // Not supported in scaladoc2
    Nil

  /*********** Dependencies ***********/
  //val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  val h2 = "com.h2database" % "h2" % "1.2.140" % "test->default"
  val sqlite = "org.xerial" % "sqlite-jdbc" % "3.6.20" % "test->default"
  val junitInterface = "com.novocode" % "junit-interface" % "0.4" % "test->default"
  val fmppDep = "net.sourceforge.fmpp" % "fmpp" % "0.9.13" % "fmpp"
  val fmppConf = config("fmpp") hide

  /*********** Run FMPP before compiling ***********/
  lazy val fmpp =
    runTask(Some("fmpp.tools.CommandLine"), configurationClasspath(fmppConf),
      "--expert" :: "-q" :: "-S" :: "src/main/scala" :: "-O" :: "src/main/scala" ::
        "--replace-extensions=fm, scala" :: "-M" :: "execute(**/*.fm), ignore(**/*)" :: Nil)
  override def compileAction = super.compileAction dependsOn(fmpp)

  /*********** Publishing ***********/
  val publishTo = Resolver.file("ScalaQuery Test Repo", new File("e:/temp/repo/"))
  //val publishTo = "Scala Tools Snapshots" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  //val publishTo = "Scala Tools Releases" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  def specificSnapshotRepo =
    Resolver.url("scala-nightly").
    artifacts("http://scala-tools.org/repo-snapshots/[organization]/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]").
    mavenStyle()
  val nightlyScala = ModuleConfiguration("org.scala-lang", "*", "2.8.0-.*", specificSnapshotRepo)
  //override def deliverScalaDependencies = Nil
  override def managedStyle = ManagedStyle.Maven
  override def packageDocsJar = defaultJarPath("-javadocs.jar")
  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)
  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadocs"), Nil, None)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  /*********** Extra meta-data for the POM ***********/
  override def makePomAction = enrichPom dependsOn superMakePom
  lazy val superMakePom = super.makePomAction
  lazy val enrichPom = task {
    val in = XML.loadFile(pomPath.asFile)
    val out = <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      {in \ "modelVersion"}
      {in \ "groupId"}
      {in \ "artifactId"}
      {in \ "packaging"}
      <name>ScalaQuery</name>
      {in \ "version"}
      <url>http://github.com/szeiger/scala-query/</url>
      <inceptionYear>2008</inceptionYear>
      <description>A type-safe database query DSL for Scala</description>
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
      </scm>
      {in \ "dependencies"}
    </project>
    val fos = new FileOutputStream(pomPath.asFile)
    try {
      val w = Channels.newWriter(fos.getChannel(), "UTF-8")
      try {
        w.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n")
        w.write(new PrettyPrinter(java.lang.Integer.MAX_VALUE, 2).format(out, TopScope))
      } finally { w.close() }
    } finally { fos.close() }
    None
  }
}
