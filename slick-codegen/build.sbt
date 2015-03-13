name := "Slick-CodeGen"
description := "Code Generator for Slick (Scala Language-Integrated Connection Kit)"
scalacOptions in(Compile, doc) ++= Seq(
  "-doc-source-url", "https://github.com/slick/slick/blob/" + version.value + "/slick-codegen/src/main€{FILE_PATH}.scala"
)
unmanagedResourceDirectories in Test += (baseDirectory in root).value / "common-test-resources"
test :=()
testOnly :=()
//sdlcBase := "codegen-api/"
//sdlcCheckDir := (target in(slickProject, com.typesafe.sbt.SbtSite.SiteKeys.makeSite)).value
//sdlc <<= sdlc dependsOn(doc in Compile, com.typesafe.sbt.SbtSite.SiteKeys.makeSite in slickDocs)
