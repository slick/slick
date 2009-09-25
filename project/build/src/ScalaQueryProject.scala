import sbt._
import scala.collection.Set
import java.io.File

class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = Deprecation :: super.compileOptions.toList
  //val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
  //val specs = "org.scala-tools.testing" % "specs" % "1.4.4"
  //val scalaCheck = "org.scalacheck" % "scalacheck" % "1.5"
  val h2 = "com.h2database" % "h2" % "1.1.118" % "test->default"
  val junit = "junit" % "junit" % "4.7" % "test->default"
  //val scalatest = "org.scalatest" % "scalatest" % "1.0-for-scala-2.8.0-SNAPSHOT" % "test->default"

  val forkedCompilerJar = property[File]
  val forkedLibraryJar = property[File]
  val doFork = propertyOptional[Boolean](false)
  override def fork = if(doFork.get.get) Some(new ForkScalaCompiler with ForkScalaRun {
    override def workingDirectory = Some(info.projectPath.asFile)
    override def scalaJars: Iterable[File] = List(forkedCompilerJar.get.get, forkedLibraryJar.get.get)
  }) else super.fork
}
