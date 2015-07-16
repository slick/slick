package com.typesafe.slick.testkit.util

import slick.profile.{SqlProfile, RelationalProfile, BasicProfile}
import slick.driver.JdbcProfile
import java.io.{PrintWriter, OutputStreamWriter, BufferedWriter, FileOutputStream, FileWriter}

/** Build a table of supported capability flags for the user manual. */
object BuildCapabilitiesTable extends App {
  // testkit/runMain com.typesafe.slick.testkit.util.BuildCapabilitiesTable ../src/sphinx/capabilities.csv
  if(args.length < 1 || args.length > 2) {
    println("Syntax: com.typesafe.slick.testkit.util.BuildCapabilitiesTable OUTPUTFILE [DRIVERLIST]")
    System.exit(1)
  }

  val driverNames = if(args.length > 1) args(1).split(",") else Array(
    "slick.driver.DerbyDriver",
    "slick.driver.H2Driver",
    "slick.driver.HsqldbDriver",
    "slick.driver.MySQLDriver",
    "slick.driver.PostgresDriver",
    "slick.driver.SQLiteDriver"
  )

  val drivers = driverNames.map { n =>
    Class.forName(n + "$").getField("MODULE$").get(null).asInstanceOf[BasicProfile]
  }

  val profiles = Vector(
    RelationalProfile.capabilities.all -> "slick.profile.RelationalProfile$$capabilities$@",
    SqlProfile.capabilities.all -> "slick.profile.SqlProfile$$capabilities$@",
    JdbcProfile.capabilities.all -> "slick.driver.JdbcProfile$$capabilities$@"
  )

  val capabilities = for {
    (caps, linkBase) <- profiles
    cap <- caps.toVector.sortBy(c => if(c.toString.endsWith(".other")) "" else c.toString)
  } yield (cap, linkBase + cap.toString.replaceFirst(".*\\.", "") + ":slick.profile.Capability")

  val out = new FileOutputStream(args(0))
  try {
    val wr = new PrintWriter(new BufferedWriter(new OutputStreamWriter(out, "UTF-8")))
    wr.println("Capability," + driverNames.map(n => s":api:`$n`").mkString(","))
    for((cap, link) <- capabilities) {
      val flags = drivers.map(d => d.capabilities.contains(cap))
      wr.println(s":api:`$cap <$link>`," + flags.map(b => if(b) "Yes" else "").mkString(","))
    }
    wr.flush()
  } finally out.close()
}
