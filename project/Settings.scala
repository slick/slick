import sbt._
import Keys._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.core.{DirectMissingMethodProblem, IncompatibleMethTypeProblem, IncompatibleResultTypeProblem, MissingClassProblem, MissingTypesProblem, ProblemFilters, ReversedMissingMethodProblem}
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaBinaryIssueFilters, mimaPreviousArtifacts}
import com.typesafe.sbt.osgi.SbtOsgi.{OsgiKeys, osgiSettings}
import com.typesafe.sbt.pgp.PgpKeys

object Settings {
//  val slickVersion = 

  val testSamples = TaskKey[Unit]("testSamples", "Run tests in the sample apps")

  val repoKind =
    SettingKey[String]("repo-kind",
                       """"Maven repository kind ("snapshots" or "releases")""")

  val binaryCompatSlickVersion = SettingKey[Option[String]]("binaryCompatSlickVersion",
                                                            "The slick version this build should be compatible with, if any")

  /* Test Configuration for running tests on doc sources */
  def DocTest = config("doctest") extend(Test)

  def slickProjectSettings = (
    slickGeneralSettings ++
      compilerDependencySetting("macro") ++
      inConfig(config("macro"))(Defaults.configSettings) ++
      FMPP.preprocessorSettings ++
      mimaDefaultSettings ++
      extTarget("slick") ++
      Docs.scaladocSettings ++
      osgiSettings ++
      Seq(
        name := "Slick",
        description := "Scala Language-Integrated Connection Kit",
        libraryDependencies ++= Dependencies.mainDependencies,
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
          "-doc-root-content", "scaladoc-root.txt"
        ),
        test := (), testOnly :=  (), // suppress test status output
        mimaPreviousArtifacts := binaryCompatSlickVersion.value.toSet.map { v: String =>
          "com.typesafe.slick" % ("slick_" + scalaBinaryVersion.value) % v
        },
        mimaBinaryIssueFilters ++= Seq(
          ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl$"),
          ProblemFilters.exclude[MissingClassProblem]("slick.util.MacroSupportInterpolationImpl"),
          // #1997 added new method ColumnExtensionMethods.in
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.in"),
          // #1958 changes for scala 2.13 support (Iterable return type instead of Traversable)
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.util.ConstArray.from"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.util.SQLBuilder.sep"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.BaseColumnExtensionMethods.inSetBind"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.BaseColumnExtensionMethods.inSet"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.OptionColumnExtensionMethods.inSetBind"),
          ProblemFilters.exclude[IncompatibleMethTypeProblem]("slick.lifted.OptionColumnExtensionMethods.inSet"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSetBind"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.inSet"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.lifted.ColumnExtensionMethods.in"),
          // #2025 default parameters for AsyncExecutor.apply have been removed and replaced by overloads
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$5"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$6"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.util.AsyncExecutor.apply$default$7"),
          // TODO see of some of these can be prevented
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.H2Profile#JdbcTypes.instantType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLiteProfile$JdbcTypes$LocalDateJdbcType"),
          ProblemFilters.exclude[MissingTypesProblem]("slick.jdbc.OracleProfile$JdbcTypes$LocalTimeJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.DerbyProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes.localDateType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.timestampJdbcType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.offsetDateTimeType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.localDateType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.instantType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.zonedDateType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.localTimeType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#JdbcTypes.offsetTimeType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.charColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.longColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.clobColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.blobColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.instantColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.intColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.booleanColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.shortColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.doubleColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timestampColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetTimeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.bigDecimalColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.zonedDateTimeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.floatColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.uuidColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localTimeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.dateColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateTimeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetDateTimeColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteArrayColumnType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.stringColumnType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLServerProfile$JdbcTypes$LocalTimeJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.HsqldbProfile#JdbcTypes.localTimeType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLServerProfile$JdbcTypes$LocalDateTimeJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.JdbcTypesComponent$JdbcTypes$ZonedDateTimeJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.JdbcTypesComponent$JdbcTypes$LocalDateJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.MySQLProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.MySQLProfile#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.DerbyProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.HsqldbProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#OffsetTimeJdbcType.serializeFiniteTime"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#OffsetTimeJdbcType.parseFiniteTime"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.OracleProfile$JdbcTypes$LocalDateTimeJdbcType"),
          ProblemFilters.exclude[MissingTypesProblem]("slick.jdbc.OracleProfile$JdbcTypes$ZonedDateTimeJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.JdbcTypesComponent$JdbcTypes$LocalDateTimeJdbcType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.OracleProfile#JdbcTypes.offsetDateTimeType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.OracleProfile#JdbcTypes.localDateType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.OracleProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.OracleProfile#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLiteProfile$JdbcTypes$LocalDateTimeJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.SQLiteProfile#JdbcTypes.localDateType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.SQLiteProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.SQLiteProfile#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.PostgresProfile$JdbcTypes$LocalDateJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.DB2Profile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLiteProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.OracleProfile$JdbcTypes$OffsetDateTimeJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.PostgresProfile$JdbcTypes$LocalDateTimeJdbcType"),
          ProblemFilters.exclude[MissingTypesProblem]("slick.jdbc.PostgresProfile$JdbcTypes$LocalTimeJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.max"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.serializeTime"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.parseTime"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.min"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.serializeFiniteTime"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#LocalTimeJdbcType.parseFiniteTime"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.OracleProfile$JdbcTypes$LocalDateJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.SQLServerProfile#JdbcTypes.instantType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.SQLServerProfile#JdbcTypes.localDateTimeType"),
          ProblemFilters.exclude[IncompatibleResultTypeProblem]("slick.jdbc.SQLServerProfile#JdbcTypes.localTimeType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.OracleProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.DB2Profile#JdbcTypes.instantType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.PostgresProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.JdbcTypesComponent$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#PostgreTimeJdbcType.serializeFiniteTime"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#PostgreTimeJdbcType.parseFiniteTime"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#PostgreTimeJdbcType.serializeFiniteTime"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.PostgresProfile#JdbcTypes#PostgreTimeJdbcType.parseFiniteTime"),
          ProblemFilters.exclude[MissingClassProblem]("slick.jdbc.SQLServerProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[MissingTypesProblem]("slick.jdbc.HsqldbProfile$JdbcTypes$InstantJdbcType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes#InstantJdbcType.setValue"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes#InstantJdbcType.getValue"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes#InstantJdbcType.hasLiteralForm"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes#InstantJdbcType.sqlType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.HsqldbProfile#JdbcTypes#InstantJdbcType.updateValue"),
          // Note the following part is actually for scala 2.11 only
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.charColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.longColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.clobColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.blobColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.instantColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.intColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.booleanColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.shortColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.doubleColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timestampColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetTimeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.floatColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.bigDecimalColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.zonedDateTimeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.uuidColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localTimeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.dateColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateTimeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetDateTimeColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteArrayColumnType"),
          ProblemFilters.exclude[DirectMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.stringColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.charColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.longColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.clobColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.blobColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.instantColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.intColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.booleanColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.shortColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.doubleColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.timestampColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetTimeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.floatColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.bigDecimalColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.zonedDateTimeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.uuidColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localTimeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.dateColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.localDateTimeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.offsetDateTimeColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.byteArrayColumnType"),
          ProblemFilters.exclude[ReversedMissingMethodProblem]("slick.jdbc.JdbcTypesComponent#ImplicitColumnTypes.stringColumnType")

        ),
        ivyConfigurations += config("macro").hide.extend(Compile),
        unmanagedClasspath in Compile ++= (products in config("macro")).value,
        libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        mappings in (Compile, packageSrc) ++= (mappings in (config("macro"), packageSrc)).value,
        mappings in (Compile, packageBin) ++= (mappings in (config("macro"), packageBin)).value,
        OsgiKeys.exportPackage := Seq("slick", "slick.*", "scala.slick", "scala.slick.*"),
        OsgiKeys.importPackage := Seq(Osgi.osgiImport("scala*", scalaVersion.value), "*"),
        OsgiKeys.privatePackage := Nil
      )
  )

  def slickTestkitProjectSettings = (
    slickGeneralSettings ++
      compilerDependencySetting("provided") ++
      inConfig(DocTest)(Defaults.testSettings) ++
      Docs.scaladocSettings ++
      TypeProviders.codegenSettings ++
      extTarget("testkit") ++
      Seq(
        name := "Slick-TestKit",
        description := "Test Kit for Slick (Scala Language-Integrated Connection Kit)",
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-testkit/src/main€{FILE_PATH}.scala"
        ),
        testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a", "-Djava.awt.headless=true"),
        //scalacOptions in Compile += "-Yreify-copypaste",
        libraryDependencies ++=
          Dependencies.junit ++:
          (Dependencies.reactiveStreamsTCK % "test") +:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % "test") ++:
          (Dependencies.logback +: Dependencies.testDBs).map(_ % "codegen"),
        parallelExecution in Test := false,
        fork in run := true,
        //connectInput in run := true,
        //javaOptions in run += "-agentpath:/Applications/YourKit_Java_Profiler_2015_build_15072.app/Contents/Resources/bin/mac/libyjpagent.jnilib",
        javaOptions in run += "-Dslick.ansiDump=true",
        //javaOptions in run += "-verbose:gc",
        compile in Test ~= { a =>
          // Delete classes in "compile" packages after compiling. (Currently only slick.test.compile.NestedShapeTest)
          // These are used for compile-time tests and should be recompiled every time.
          val products = a.relations.allProducts.toSeq ** new SimpleFileFilter(_.getParentFile.getName == "compile")
          IO.delete(products.get)
          a
        },
        Docs.buildCapabilitiesTable := {
          val logger = ConsoleLogger()
          Run.run( "com.typesafe.slick.testkit.util.BuildCapabilitiesTable",
                   (fullClasspath in Compile).value.map(_.data),
                   Seq(Docs.docDir.value / "capabilities.md") map (_.toString),
                   logger)(runner.value)
        },
        unmanagedSourceDirectories in DocTest += Docs.docDir.value / "code",
        unmanagedResourceDirectories in DocTest += Docs.docDir.value / "code"
      )
  )

  def slickCodegenProjectSettings = (
    slickGeneralSettings ++
      extTarget("codegen") ++
      Docs.scaladocSettings ++
      Seq(
        name := "Slick-CodeGen",
        description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)",
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-codegen/src/main€{FILE_PATH}.scala"
        ),
        test := (), testOnly := (), // suppress test status output
        commonTestResourcesSetting
      )
  )

  def slickHikariCPProjectSettings = (
    slickGeneralSettings ++
      extTarget("hikaricp") ++
      Docs.scaladocSettings ++
      osgiSettings ++
      Seq(
        name := "Slick-HikariCP",
        description := "HikariCP integration for Slick (Scala Language-Integrated Connection Kit)",
        scalacOptions in (Compile, doc) ++= Seq(
          "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick-hikaricp/src/main€{FILE_PATH}.scala"
        ),
        test := (), testOnly := (), // suppress test status output
        libraryDependencies += Dependencies.hikariCP,
        OsgiKeys.exportPackage := Seq("slick.jdbc.hikaricp"),
        OsgiKeys.importPackage := Seq(
          Osgi.osgiImport("slick*", (version in ThisBuild).value),
          Osgi.osgiImport("scala*", scalaVersion.value),
          "*"
        ),
        OsgiKeys.privatePackage := Nil
      )
  )

  def aRootProjectSettings = (
    slickGeneralSettings ++
      extTarget("root") ++
      Docs.docSettings ++
      Seq(
        sourceDirectory := file(target.value + "/root-src"),
        publishArtifact := false,
        publish := {},
        publishLocal := {},
        PgpKeys.publishSigned := {},
        PgpKeys.publishLocalSigned := {},
        test := (), testOnly := () // suppress test status output
      )
  )

  def reactiveStreamsTestProjectSettings = (
    slickGeneralSettings ++ Seq(
      name := "Slick-ReactiveStreamsTests",
      resolvers += Resolver.sbtPluginRepo("releases"),
      libraryDependencies += Dependencies.scalaTestFor(scalaVersion.value),
      libraryDependencies ++=
        (Dependencies.logback +: Dependencies.testDBs).map(_ % "test"),
      libraryDependencies += Dependencies.reactiveStreamsTCK,
      parallelExecution in Test := false,
      commonTestResourcesSetting
    )
  )

  def osgiTestProjectSettings = slickGeneralSettings ++ Seq(
    name := "Slick-OsgiTests",
    libraryDependencies ++= (
      Dependencies.h2 +: Dependencies.logback +: Dependencies.reactiveStreams +:
        Dependencies.junit ++: Dependencies.paxExam
    ).map(_ % "test"),
    fork in Test := true,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
    javaOptions in Test ++= Seq(
      // Use '@' as a seperator that shouldn't appear in any filepaths or names
      "-Dslick.osgi.bundlepath=" + Osgi.osgiBundleFiles.value.map(_.getCanonicalPath).mkString("@"),
      "-Dorg.ops4j.pax.logging.DefaultServiceLog.level=WARN"
    ),
    Osgi.osgiBundleFiles := Seq((OsgiKeys.bundle in LocalProject("slick")).value),
    Osgi.osgiBundleFiles ++=
      (dependencyClasspath in Compile in LocalProject("slick")).value.
      map(_.data).filterNot(_.isDirectory),
    Osgi.osgiBundleFiles ++=
      (dependencyClasspath in Test).value.map(_.data).
      filter(f => f.name.contains("logback-") || f.name.contains("h2")),
    publishArtifact := false,
    commonTestResourcesSetting
  )

  def slickGeneralSettings =
    slickPublishSettings ++ slickScalacSettings ++ slickScalaSettings ++ Seq(
      logBuffered := false
    )

  def commonTestResourcesSetting = (
    unmanagedResourceDirectories in Test +=
      (baseDirectory in LocalProject("root")).value / "common-test-resources"
  )

  def slickScalaSettings = {
    sys.props("scala.home.local") match {
      case null => publishedScalaSettings
      case path =>
        scala.Console.err.println("Using local scala at " + path)
        localScalaSettings(path)
    }
  }

  def slickPublishSettings = Seq(
    organizationName := "Typesafe",
    organization := "com.typesafe.slick",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    repoKind := (if (version.value.trim.endsWith("SNAPSHOT")) "snapshots" else "releases"),
    publishTo := (
      repoKind.value match {
        case "snapshots" => Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
        case "releases" =>  Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
      }
    ),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    makePomConfiguration ~= { _.copy(configurations = Some(Seq(Compile, Runtime, Optional))) },
    homepage := Some(url("http://slick.typesafe.com")),
    startYear := Some(2008),
    licenses += ("Two-clause BSD-style license", url("http://github.com/slick/slick/blob/master/LICENSE.txt")),
    pomExtra := pomExtraXml
  )

  def pomExtraXml = (
    <developers>
      <developer>
        <id>szeiger</id>
        <name>Stefan Zeiger</name>
        <timezone>+1</timezone>
        <url>http://szeiger.de</url>
      </developer>
      <developer>
        <id>hvesalai</id>
        <name>Heikki Vesalainen</name>
        <timezone>+2</timezone>
        <url>https://github.com/hvesalai/</url>
      </developer>
    </developers>
    <scm>
      <url>git@github.com:slick/slick.git</url>
      <connection>scm:git:git@github.com:slick/slick.git</connection>
    </scm>
  )

  def slickScalacSettings = Seq(
    scalacOptions ++= List("-deprecation", "-feature", "-unchecked", "-Xfuture"),
    scalacOptions in (Compile, doc) ++= Seq(
      "-doc-title", name.value,
      "-doc-version", version.value,
      "-doc-footer", "Slick is developed by Typesafe and EPFL Lausanne.",
      "-sourcepath", (sourceDirectory in Compile).value.getPath, // needed for scaladoc to strip the location of the linked source path
      "-doc-source-url", s"https://github.com/slick/slick/blob/${Docs.versionTag(version.value)}/slick/src/main€{FILE_PATH}.scala",
      "-implicits",
      "-diagrams", // requires graphviz
      "-groups"
    )
  )

  // set the scala-compiler dependency unless a local scala is in use
  def compilerDependencySetting(config: String) =
    if (sys.props("scala.home.local") != null) Nil else Seq(
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % config
    )

  def publishedScalaSettings = Seq(
    scalaVersion := Dependencies.scalaVersions.head,
    crossScalaVersions := Dependencies.scalaVersions
  )

  def localScalaSettings(path: String): Seq[Setting[_]] = Seq(
    scalaVersion := "2.10.0-unknown",
    scalaBinaryVersion := "2.10.0-unknown",
    crossVersion := CrossVersion.Disabled,
    scalaHome := Some(file(path)),
    autoScalaLibrary := false,
    unmanagedJars := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("compile") := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("test") := scalaInstance.map( _.jars.classpath).value,
    unmanagedJars in config("macro") := scalaInstance.map( _.jars.classpath).value
  )

  def sampleProject(s: String): Project = Project(id = "sample-"+s, base = file("samples/"+s))
    .addSbtFiles(file("../override.sbt"))

  def extTarget(extName: String): Seq[Setting[File]] = {
    sys.props("slick.build.target") match {
      case null => Seq.empty
      case path => Seq(target := file(path + "/" + extName))
    }
  }

}
