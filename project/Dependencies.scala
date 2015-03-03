import sbt._

object Dependencies {

  object Versions {
    val crossScala = Seq("2.11.5", "2.10.4")
    val scalaVersion = crossScala.head
  }

  val testngExtras = Seq(
    "com.google.inject" % "guice" % "2.0"
  )
  val paxExamVersion = "2.6.0"
  val paxExam = Seq(
    "org.ops4j.pax.exam"     % "pax-exam-container-native"  % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-junit4"            % paxExamVersion,
    "org.ops4j.pax.exam"     % "pax-exam-link-assembly"     % paxExamVersion,
    "org.ops4j.pax.url"      % "pax-url-aether"             % "1.6.0",
    "org.ops4j.pax.swissbox" % "pax-swissbox-framework"     % "1.5.1",
    "org.apache.felix"       % "org.apache.felix.framework" % "3.2.2"
  )
}

object Library {

  object DB {
    val h2 = "com.h2database" % "h2" % "1.3.170"
    val mysql = "mysql" % "mysql-connector-java" % "5.1.23"
    val postgres = "postgresql" % "postgresql" % "9.1-901.jdbc4"
    val sqlite = "org.xerial" % "sqlite-jdbc" % "3.8.7"
    val derby = "org.apache.derby" % "derby" % "10.9.1.0"
    val hsqldb = "org.hsqldb" % "hsqldb" % "2.2.8"

    val all = Seq(h2, mysql, postgres, sqlite, derby, hsqldb)
  }

  val slf4j = "org.slf4j" % "slf4j-api" % "1.6.4"
  val logback = "ch.qos.logback" % "logback-classic" % "0.9.28"
  val typesafeConfig = "com.typesafe" % "config" % "1.2.1"

  val hikariCP = "com.zaxxer" % "HikariCP-java6" % "2.0.1"

  val junit = "junit" % "junit-dep" % "4.10"
  val junitInterface = "com.novocode" % "junit-interface" % "0.11"

  private val reactiveStreamsVersion = "1.0.0.RC3"
  val reactiveStreams = "org.reactivestreams" % "reactive-streams" % reactiveStreamsVersion
  val reactiveStreamsTCK = "org.reactivestreams" % "reactive-streams-tck" % reactiveStreamsVersion

}
