name := "SLICK"

organizationName := "Typesafe"

organization := "com.typesafe"

version := "1.0.0-SNAPSHOT"

//scalaVersion := "2.10.0-unknown-unknown"
//scalaVersion := "2.10.0-SNAPSHOT"
scalaVersion := "2.10.0-M6"

scalaBinaryVersion in Global := "2.10.0-M6"

resolvers += Resolver.sonatypeRepo("snapshots")

//crossScalaVersions ++= "2.10.0-M4" :: Nil
//crossVersion := CrossVersion.Disabled

//scalaHome := Some(file("C:/Users/szeiger/code/scala/build/pack"))

//autoScalaLibrary := false

scalacOptions ++= List("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.3.166" % "test",
  "org.xerial" % "sqlite-jdbc" % "3.6.20" % "test",
  "org.apache.derby" % "derby" % "10.9.1.0" % "test",
  "org.hsqldb" % "hsqldb" % "2.2.8" % "test",
  "postgresql" % "postgresql" % "9.1-901.jdbc4" % "test",
  "mysql" % "mysql-connector-java" % "5.1.13" % "test",
  "net.sourceforge.jtds" % "jtds" % "1.2.4" % "test",
  "com.novocode" % "junit-interface" % "0.9-RC2" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "ch.qos.logback" % "logback-classic" % "0.9.28" % "test"
)

// Add scala-compiler dependency for scala.reflect.internal
libraryDependencies <+= scalaVersion(
  "org.scala-lang" % "scala-compiler" % _
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a")

publishTo <<= (repoKind)(r => Some(Resolver.file("test", file("c:/temp/repo/"+r))))
/*publishTo <<= (repoKind){
  case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}*/

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

description := "A type-safe database API for Scala"

homepage := Some(url("https://github.com/slick/slick/wiki"))

startYear := Some(2008)

licenses += ("Two-clause BSD-style license", url("http://github.com/slick/slick/blob/master/LICENSE.txt"))

pomExtra :=
  <developers>
    <developer>
      <id>szeiger</id>
      <name>Stefan Zeiger</name>
      <timezone>+1</timezone>
      <url>http://szeiger.de</url>
    </developer>
    <developer>
      <id>cvogt</id>
      <name>Jan Christopher Vogt</name>
      <timezone>+1</timezone>
      <url>https://github.com/cvogt/</url>
    </developer>
  </developers>
  <scm>
    <url>git@github.com:slick/slick.git</url>
    <connection>scm:git:git@github.com:slick/slick.git</connection>
  </scm>

// Work around scaladoc problem
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))
