import sbt._
import Keys._

import scala.util.{Failure, Success}

object TypeProviders {

  /** Slick type provider code gen  */
  val typeProviders = taskKey[Seq[File]]("Type provider code generation")
  val TypeProvidersConfig = config("codegen").hide
  val CompileConfig = config("compile")
  def codegenSettings = {
    inConfig(TypeProvidersConfig)(Defaults.configSettings) ++
    Seq(
      Test / sourceGenerators += typeProviders.taskValue,
      typeProviders := typeProvidersTask.value,
      ivyConfigurations += TypeProvidersConfig.extend(Compile),
      (Test / compile) := ((Test / compile) dependsOn (TypeProvidersConfig / compile)).value,
      TypeProvidersConfig / unmanagedClasspath ++= (CompileConfig / fullClasspath).value,
      TypeProvidersConfig / unmanagedClasspath ++= (LocalProject("codegen") / Test / fullClasspath).value,
      Test / unmanagedClasspath ++= (TypeProvidersConfig / fullClasspath).value,
      Test / packageSrc / mappings ++= {
        val src = (Test / sourceDirectory).value / "codegen"
        val inFiles = src ** "*.scala"
        ((Test / managedSources).value.pair(Path.relativeTo((Test / sourceManaged).value) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(src) | Path.flat)) // Add *.fm files to sources JAR
      }
    )
  }
  def typeProvidersTask = Def.task {
    val cp = (TypeProvidersConfig / fullClasspath).value
    val r = (typeProviders / runner).value
    val output = (Test / sourceManaged).value
    val s = streams.value
    val srcDir = (Compile / sourceDirectory).value
    val slickSrc = (LocalProject("slick") / sourceDirectory).value
    val src = srcDir / "codegen"
    val outDir = (output/"slick-codegen").getPath
    val inFiles = (src ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/codegen" ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/jdbc/meta" ** "*.scala").get.toSet
    val cachedFun = FileFunction.cached(s.cacheDirectory / "type-providers", inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { (in: Set[File]) =>
      IO.delete((output ** "*.scala").get)

      val errorsMain = r.run("slick.test.codegen.GenerateMainSources", cp.files, Array(outDir), s.log)
      val errorsRoundtrip = r.run("slick.test.codegen.GenerateRoundtripSources", cp.files, Array(outDir), s.log)

      (errorsMain, errorsRoundtrip) match {
        case (Success(_), Success(_)) =>
          (output ** "*.scala").get.toSet
        case (Failure(failedMain), Failure(failedRoundtrip)) =>
          sys.error(failedMain.getMessage + System.lineSeparator() + failedRoundtrip)
        case (failedMain, failedRoundtrip) =>
          failedMain.fold(e => sys.error(e.getMessage), _ => sys.error(failedRoundtrip.failed.get.getMessage))
      }
    }
    cachedFun(inFiles).toSeq
  }
}
