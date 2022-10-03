import sbt._

/** Dependencies for reuse in different parts of the build */
object Dependencies {

  // NOTE: remember to change the version numbers in the sample projects
  // when changing them here

  val scalaVersions = Seq("2.12.17", "2.13.9") // When updating these also update ci.yml and appveyor.yml

  val slf4j = "org.slf4j" % "slf4j-api" % "2.0.3"
  val typesafeConfig = "com.typesafe" % "config" % "1.4.2"
  val reactiveStreamsVersion = "1.0.4"
  val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
  val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1"

  def mainDependencies = Seq(slf4j, typesafeConfig, reactiveStreams, scalaCollectionCompat)

  val junit = Seq(
    "junit" % "junit-dep" % "4.11",
    "com.github.sbt" % "junit-interface" % "0.13.3"
  )
  val logback = "ch.qos.logback" % "logback-classic" % "1.4.3"
  val hikariCP = "com.zaxxer" % "HikariCP" % "5.0.1"

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
    "org.xerial" % "sqlite-jdbc" % "3.39.3.0",
    "org.hsqldb" % "hsqldb" % "2.5.2",
    "org.postgresql" % "postgresql" % "42.5.0",
    "mysql" % "mysql-connector-java" % "8.0.30",
    "net.sourceforge.jtds" % "jtds" % "1.3.1",
    "com.oracle.database.jdbc.debug" % "ojdbc8_g" % "21.7.0.0",
    "com.ibm.db2.jcc" % "db2jcc" % "db2jcc4"
  )

  val paxExamVersion = "4.13.5"
  val paxExam = Seq(
    "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
    "org.apache.felix"       % "org.apache.felix.framework" % "7.0.5"
  )
}
