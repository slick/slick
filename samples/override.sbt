// Override settings of sample projects when they are used as subprojects of the main Slick build.
//
// The sample projects are packaged as standalone sbt projects via the Example Code Service
// (https://example.lightbend.com) so they have to be complete and usable on their own and they must
// not contain any unnecessary code that is only needed when they are run as part of the main build.
// The settings here (which are loaded after build.sbt) ensure that the Scala version and the Slick
// version match the ones of the main build.

crossScalaVersions := (crossScalaVersions in LocalProject("slick")).value

scalaVersion := (scalaVersion in LocalProject("slick")).value

libraryDependencies := libraryDependencies.value.map { m =>
  if(m.organization == (organization in LocalProject("slick")).value) m.copy(revision = slickVersion)
  else m
}

unmanagedClasspath in Compile :=
  Attributed.blank(baseDirectory.value.getParentFile / "resources") +: (unmanagedClasspath in Compile).value

unmanagedClasspath in Compile ++= (products in config("macro") in LocalProject("slick")).value
