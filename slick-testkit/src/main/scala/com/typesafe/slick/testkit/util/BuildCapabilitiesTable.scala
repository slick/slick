package com.typesafe.slick.testkit.util

import java.io.*

import slick.basic.BasicProfile
import slick.jdbc.JdbcCapabilities
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities


/** Build a table of supported capability flags for the user manual. */
object BuildCapabilitiesTable {
  def main(args: Array[String]): Unit = {
    // testkit/runMain com.typesafe.slick.testkit.util.BuildCapabilitiesTable ../src/sphinx/capabilities.csv
    if (args.length < 1 || args.length > 2) {
      println("Syntax: com.typesafe.slick.testkit.util.BuildCapabilitiesTable OUTPUTFILE [PROFILELIST]")
      System.exit(1)
    }

    val profileNames = if (args.length > 1) args(1).split(",") else Array(
      "slick.jdbc.DB2Profile",
      "slick.jdbc.DerbyProfile",
      "slick.jdbc.H2Profile",
      "slick.jdbc.HsqldbProfile",
      "slick.jdbc.MySQLProfile",
      "slick.jdbc.OracleProfile",
      "slick.jdbc.PostgresProfile",
      "slick.jdbc.SQLiteProfile",
      "slick.jdbc.SQLServerProfile"
    )

    val profiles = profileNames.map { n =>
      Class.forName(n + "$").getField("MODULE$").get(null).asInstanceOf[BasicProfile]
    }

    val profileCapabilities = Vector(
      RelationalCapabilities.all -> "slick.relational.RelationalCapabilities$#",
      SqlCapabilities.all -> "slick.sql.SqlCapabilities$#",
      JdbcCapabilities.all -> "slick.jdbc.JdbcCapabilities$#"
    )

    val capabilities = for {
      (caps, linkBase) <- profileCapabilities
      cap <- caps.toVector.sortBy(c => if (c.toString.endsWith(".other")) "" else c.toString)
    } yield (cap, linkBase + cap.toString.replaceFirst(".*\\.", "") + ":slick.basic.Capability")

    val file = new File(args(0))
    file.getParentFile.mkdirs()
    val out = new FileOutputStream(file)
    try {
      val wr = new PrintWriter(new BufferedWriter(new OutputStreamWriter(out, "UTF-8")))
      wr.println("| Capability |" + profileNames.map(n => s" @scaladoc[${n.replace("slick.jdbc.", "").replace("Profile", "")}]($n) |").mkString)
      wr.println("| :--- |" + profileNames.map(n => " :---: |").mkString)
      for ((cap, link) <- capabilities) {
        val flags = profiles.map(d => d.capabilities.contains(cap))
        wr.println(s"| @scaladoc[$cap]($link) |" + flags.map(b => if (b) " ✅ |" else " |").mkString)
      }
      wr.flush()
    } finally out.close()
  }
}
