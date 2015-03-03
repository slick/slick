name := "Slick-Docs"
description := "Slick Documentation"
unmanagedSourceDirectories in Compile += baseDirectory.value / "rst" / "code"
unmanagedResourceDirectories in Compile += baseDirectory.value / "rst" / "resources"
libraryDependencies ++= Seq(
  Library.hikariCP % Test,
  Library.logback % Test,
  Library.junit % Test,
  Library.junitInterface % Test
)
libraryDependencies ++= Library.DB.all.map(_ % Test)
enablePlugins(SphinxDoc)
