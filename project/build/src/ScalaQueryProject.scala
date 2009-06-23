import sbt._
import scala.collection.Set

class ScalaQueryProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = Deprecation :: super.compileOptions.toList
  val specs = "org.scala-tools.testing" % "specs" % "1.4.4"
  val cglib = "cglib" % "cglib" % "2.1_3"
  val objensis = "org.objenesis" % "objenesis" % "1.1"
  val hamcrest = "org.hamcrest" % "hamcrest-all" % "1.1"
  val asm = "asm" % "asm" % "1.5.3"
  val scalaCheck = "org.scalacheck" % "scalacheck" % "1.5"
  val junit = "junit" % "junit" % "4.5"
  val jmock = "org.jmock" % "jmock" % "2.4.0"
}
