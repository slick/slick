name := "Slick-ReactiveStreamsTests"
unmanagedResourceDirectories in Test += (baseDirectory in root).value / "common-test-resources"
libraryDependencies ++= Seq(
  Library.reactiveStreamsTCK,
  Library.logback % Test
)
libraryDependencies ++= Dependencies.testngExtras
libraryDependencies ++= Library.DB.all.map(_ % Test)
testNGSuites := Seq("reactive-streams-tests/src/test/resources/testng.xml")
