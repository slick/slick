import sbt._

/** Dependencies for reuse in different parts of the build */
object Dependencies {

  // NOTE: remember to change the version numbers in the sample projects
  // when changing them here

  val scalaVersions = Seq("2.11.12", "2.12.12", "2.13.4") // When updating these also update .travis.yml and appveyor.yml

  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.26"
  val typesafeConfig = "com.typesafe" % "config" % "1.4.1"
  val reactiveStreamsVersion = "1.0.2"
  val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
  val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.4.1"

  def mainDependencies = Seq(slf4j, typesafeConfig, reactiveStreams, scalaCollectionCompat)

  val junit = Seq(
    "junit" % "junit-dep" % "4.11",
    "com.novocode" % "junit-interface" % "0.11"
  )
  def scalaTestFor(scalaVersion: String) = {
    val v = "3.0.8"
    "org.scalatest" %% "scalatest" % v
  }
  val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
  val hikariCP = "com.zaxxer" % "HikariCP" % "3.3.1"

  val h2 = "com.h2database" % "h2" % "1.4.199"
  val sqlServer = {
    val javaVersion = System.getProperty("java.version")
    val jreVersionToUse = if (javaVersion.startsWith("11") || javaVersion.startsWith("12")) {
      "11"
    } else "8"
    "com.microsoft.sqlserver" % "mssql-jdbc" % s"7.2.2.jre$jreVersionToUse"
  }

  val testDBs = Seq(
    h2,
    sqlServer,
    "org.apache.derby" % "derby" % "10.14.2.0",
    "org.xerial" % "sqlite-jdbc" % "3.27.2.1",
    "org.hsqldb" % "hsqldb" % "2.4.1",
    "org.postgresql" % "postgresql" % "42.2.5",
    "mysql" % "mysql-connector-java" % "5.1.46",
    "net.sourceforge.jtds" % "jtds" % "1.3.1"
  )

  val paxExamVersion = "4.13.1"
  val paxExam = Seq(
    "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
    "org.apache.felix"       % "org.apache.felix.framework" % "6.0.2"
  )
}
