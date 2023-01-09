import java.nio.file.{Files, Paths}


val filename = "slick-testkit/src/test/scala/slick/test/profile/ProfileTest.scala"
val res =
  "(?<=class )\\S+".r
    .findAllIn(os.read(os.pwd / os.RelPath(filename)))
    .toSeq
    .groupBy {
      case "DB2Test"        => "db2"
      case "MySQLTest"      => "mysql"
      case "OracleTest"     => "oracle"
      case "PostgresTest"   => "postgres"
      case s"SQLServer${_}" => "mssql"
      case _                => "other"
    }
    .view.mapValues(_.map("slick.test.profile." + _).mkString(" ")).toMap

Files.writeString(
  Paths.get(sys.env.getOrElse("GITHUB_OUTPUT", "/dev/stdout")),
  "tests=" + upickle.default.write(res)
)
