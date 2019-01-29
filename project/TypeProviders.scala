import sbt._
import Keys._

object TypeProviders {

  /** Slick type provider code gen  */
  val typeProviders = taskKey[Seq[File]]("Type provider code generation")
  def typeProvidersConfig = config("codegen").hide
  def codegenSettings = {
    inConfig(typeProvidersConfig)(Defaults.configSettings) ++
    Seq(
      sourceGenerators in Test += typeProviders.taskValue,
      typeProviders := typeProvidersTask.value,
      ivyConfigurations += typeProvidersConfig.extend(Compile),
      (compile in Test) := ((compile in Test) dependsOn (compile in typeProvidersConfig)).value,
      unmanagedClasspath in typeProvidersConfig ++= (fullClasspath in config("compile")).value,
      unmanagedClasspath in typeProvidersConfig ++= (fullClasspath in (LocalProject("codegen"), Test)).value,
      unmanagedClasspath in Test ++= (fullClasspath in typeProvidersConfig).value,
      mappings in (Test, packageSrc) ++= {
        val src = (sourceDirectory in Test).value / "codegen"
        val inFiles = src ** "*.scala"
        ((managedSources in Test).value.pair(Path.relativeTo((sourceManaged in Test).value) | Path.flat)) ++ // Add generated sources to sources JAR
          (inFiles pair (Path.relativeTo(src) | Path.flat)) // Add *.fm files to sources JAR
      }
    )
  }
  def typeProvidersTask = Def.task {
    val cp = (fullClasspath in typeProvidersConfig).value
    val r = (runner in typeProviders).value
    val output = (sourceManaged in Test).value
    val s = streams.value
    val srcDir = sourceDirectory.value
    val slickSrc = (sourceDirectory in LocalProject("slick")).value
    val src = srcDir / "codegen"
    val outDir = (output/"slick-codegen").getPath
    val inFiles = (src ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/codegen" ** "*.scala").get.toSet ++ (slickSrc / "main/scala/slick/jdbc/meta" ** "*.scala").get.toSet
    val cachedFun = FileFunction.cached(s.cacheDirectory / "type-providers", outStyle = FilesInfo.exists) { (in: Set[File]) =>
      IO.delete((output ** "*.scala").get)

      val errors = {
        r.run("slick.test.codegen.GenerateMainSources", cp.files, Array(outDir), s.log)
      } orElse {
        r.run("slick.test.codegen.GenerateRoundtripSources", cp.files, Array(outDir), s.log)
      }

      errors foreach sys.error

      (output ** "*.scala").get.toSet
    }
    cachedFun(inFiles).toSeq
  }
}
