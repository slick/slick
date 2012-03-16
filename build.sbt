name := "ScalaQuery"

organizationName := "ScalaQuery"

organization := "org.scalaquery"

version := "0.10.0-M1"

scalaVersion := "2.9.1-1"

crossScalaVersions ++= "2.9.1-1" :: "2.9.1" :: "2.9.0-1" :: "2.9.0" :: Nil

scalacOptions += "-deprecation"

libraryDependencies <++= (useJDBC4) { u => Seq(
  "com.h2database" % "h2" % "1.2.147" % "test",
  "org.xerial" % "sqlite-jdbc" % "3.6.20" % "test",
  "org.apache.derby" % "derby" % "10.6.1.0" % "test",
  "org.hsqldb" % "hsqldb" % "2.0.0" % "test",
  "postgresql" % "postgresql" % (if(u) "8.4-701.jdbc4" else "8.4-701.jdbc3") % "test",
  "mysql" % "mysql-connector-java" % "5.1.13" % "test",
  "net.sourceforge.jtds" % "jtds" % "1.2.4" % "test",
  "com.novocode" % "junit-interface" % "0.7" % "test"
)}

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

publishTo <<= (repoKind)(r => Some(Resolver.file("test", file("c:/temp/repo/"+r))))
/*publishTo <<= (repoKind){
  case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}*/

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

description := "A type-safe database API for Scala"

homepage := Some(url("http://scalaquery.org/"))

startYear := Some(2008)

licenses += ("Two-clause BSD-style license", url("http://github.com/szeiger/scala-query/blob/master/LICENSE.txt")) 

pomExtra :=
  <developers>
    <developer>
      <id>szeiger</id>
      <name>Stefan Zeiger</name>
      <timezone>+1</timezone>
      <url>http://szeiger.de</url>
    </developer>
  </developers>
  <scm>
    <url>git@github.com:szeiger/scala-query.git</url>
    <connection>scm:git:git@github.com:szeiger/scala-query.git</connection>
  </scm>

// Work around scaladoc problem
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
