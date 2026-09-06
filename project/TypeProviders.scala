import sbt.*
import Keys.*
import xsbti.{FileConverter, HashedVirtualFileRef}

import scala.util.{Failure, Success}

object TypeProviders {

  /** Slick type provider code gen  */
  val typeProviders = taskKey[Seq[File]]("Type provider code generation")
  val TypeProvidersConfig = config("codegen").hide.extend(Compile)
  private val Test = sbt.Test.extend(TypeProvidersConfig)
  def codegenSettings = {
    inConfig(TypeProvidersConfig)(Defaults.configSettings) ++
    inConfig(Test)(Defaults.configSettings) ++
    Seq(
      Test / sourceGenerators += typeProviders.taskValue,
      typeProviders := Def.uncached(typeProvidersTask.value),
      ivyConfigurations += TypeProvidersConfig,
      ivyConfigurations += Test,
      // Add the codegen sources to the sources JAR. The generated sources are managed sources, which sbt 2 already
      // includes in the default packageSrc mappings.
      Test / packageSrc / mappings ++= {
        val conv = fileConverter.value
        val src = (Test / sourceDirectory).value / "codegen"
        val codegenSources = (src ** "*.scala").pair(Path.relativeTo(src) | Path.flat)
        codegenSources.map { case (f, path) => (conv.toVirtualFile(f.toPath): HashedVirtualFileRef) -> path }
      }
    )
  }
  def typeProvidersTask = Def.task {
    given FileConverter = fileConverter.value
    val cp = (TypeProvidersConfig / fullClasspath).value.files
    val r = (typeProviders / runner).value
    val output = (Test / sourceManaged).value
    val s = streams.value
    val srcDir = (Compile / sourceDirectory).value
    val slickSrc = (LocalProject("slick") / sourceDirectory).value
    val src = srcDir / "codegen"
    val outDir = (output/"slick-codegen").getPath
    val inFiles = (src ** "*.scala").get().toSet ++ (slickSrc / "main/scala/slick/codegen" ** "*.scala").get().toSet ++ (slickSrc / "main/scala/slick/jdbc/meta" ** "*.scala").get().toSet
    val cachedFun = FileFunction.cached(s.cacheDirectory / "type-providers", inStyle = FilesInfo.lastModified, outStyle = FilesInfo.exists) { (in: Set[File]) =>
      IO.delete((output ** "*.scala").get())

      val errorsMain = r.run("slick.test.codegen.GenerateMainSources", cp, Seq(outDir), s.log)
      val errorsRoundtrip = r.run("slick.test.codegen.GenerateRoundtripSources", cp, Seq(outDir), s.log)

      (errorsMain, errorsRoundtrip) match {
        case (Success(_), Success(_)) =>
          (output ** "*.scala").get().toSet
        case (Failure(failedMain), Failure(failedRoundtrip)) =>
          sys.error(failedMain.getMessage + System.lineSeparator() + failedRoundtrip)
        case (failedMain, failedRoundtrip) =>
          failedMain.fold(e => sys.error(e.getMessage), _ => sys.error(failedRoundtrip.failed.get.getMessage))
      }
    }
    cachedFun(inFiles).toSeq
  }
}
