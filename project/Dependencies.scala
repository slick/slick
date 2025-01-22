import sbt.*
import sbt.Keys.scalaVersion

/** Dependencies for reuse in different parts of the build */
object Dependencies {
  val scala212 = "2.12.20"
  val scala213 = "2.13.15"
  val scala3 = "3.3.4"

  val scalaVersions = Seq(scala212, scala213, scala3) // When updating these also update ci.yml and .mergify.yml

  val slf4j = "org.slf4j" % "slf4j-api" % "2.0.16"
  val typesafeConfig = "com.typesafe" % "config" % "1.4.3"
  val reactiveStreamsVersion = "1.0.4"
  val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
  val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion

  val scalaCollectionCompat = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n == 12 =>
        Seq("org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0")
      case _ =>
        Seq.empty
    }
  }

  def mainDependencies = Seq(slf4j, typesafeConfig, reactiveStreams)

  val junit = Seq(
    "junit" % "junit-dep" % "4.11",
    "com.github.sbt" % "junit-interface" % "0.13.3"
  )
  val logback = "ch.qos.logback" % "logback-classic" % "1.5.16"
  val hikariCP = "com.zaxxer" % "HikariCP" % "6.2.1"

  val h2 = "com.h2database" % "h2" % "1.4.200"
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
    "org.xerial" % "sqlite-jdbc" % "3.41.0.0",
    "org.hsqldb" % "hsqldb" % "2.7.4",
    "org.postgresql" % "postgresql" % "42.7.4",
    "com.mysql" % "mysql-connector-j" % "9.2.0",
    "com.oracle.database.jdbc.debug" % "ojdbc8_g" % "21.16.0.0",
    "com.ibm.db2.jcc" % "db2jcc" % "db2jcc4"
  )

  val paxExamVersion = "4.14.0"
  val paxExam = Seq(
    "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
    "org.apache.felix"       % "org.apache.felix.framework" % "7.0.5"
  )
}
