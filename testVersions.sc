val testFilename = "test-version.txt"

def commandSucceeds(cwd: os.Path, command: os.Shellable*) =
  os.proc(command: _*).call(cwd = cwd, check = false, stderr = os.Pipe).exitCode == 0

assert(!commandSucceeds(os.pwd, "git", "cat-file", "-e", s"HEAD:$testFilename"))

val dir = os.temp.dir()
val testPath = dir / testFilename

def createTag(name: String) = {
  println(fansi.Color.Yellow(s"Creating tag $name"))
  os.proc("git", "tag", name).call(cwd = dir, stdout = os.Inherit)
}

def commitIsTagged() = commandSucceeds(dir, "git", "describe", "--tags", "--exact-match")

def stageChanges(filename: String = testFilename) = {
  println(fansi.Color.Yellow(s"Staging changes to $filename"))
  os.proc("git", "add", filename).call(cwd = dir, stdout = os.Inherit)
}

def createCommit(message: String) = {
  println(fansi.Color.Yellow("Committing changes"))
  os.proc("git", "commit", "-m", message).call(cwd = dir, stdout = os.Inherit)
}

val sbtCommand = List("sbt", "--batch", "--info")

def assertVersions(epoch: String, major: String, minor: String): Unit = {
  println(fansi.Color.Yellow(s"Expecting version bumps to be $epoch / $major / $minor"))

  def assertExpr(expect: String, mode: String) =
    s"""  Versioning.shortVersionFor(Versioning.Bump$mode) match {
       |    case Some("$expect") =>
       |      System.out.println("Good, $mode bump would produce $expect")
       |    case other =>
       |      assert(false, "$mode bump would produce " + other + ", not $expect as expected")
       |  }
       |  """.stripMargin

  os.proc(
    sbtCommand,
    s"""eval {
       |${assertExpr(epoch, "Epoch")}
       |${assertExpr(major, "Major")}
       |${assertExpr(minor, "Minor")}
       |}
       |""".stripMargin
  ).call(cwd = dir, check = true, stdout = os.Inherit)
}

try {
  os.proc("git", "clone", os.pwd, dir).call(stdout = os.Inherit)

  os.write(
    dir / "silent.sbt",
    """
      |Global      / lintUnusedKeysOnLoad := false
      |Global      / onLoadMessage        := ""
      |ThisProject / traceLevel           := -1
      |ThisBuild   / showSuccess          := false
      |""".stripMargin
  )

  val filesToCopy =
    List(os.sub / "build.sbt", os.sub / "project" / "Versioning.scala", os.sub / "project" / "Docs.scala")
      .filter(p => !os.exists(dir / p) || os.read(dir / p) != os.read(os.pwd / p))
  filesToCopy.foreach { p =>
    os.copy.over(os.pwd / p, dir / p, replaceExisting = false)
    stageChanges(p.toString)
  }
  if (filesToCopy.nonEmpty)
    createCommit(s"Local changes to ${filesToCopy.mkString(", ")}")

  createTag("v999.9.9")

  assert(commitIsTagged())

  println(fansi.Color.Yellow("Making a change"))

  os.write(testPath, "Testing")

  assertVersions("999.9.9", "999.9.9", "999.9.9")

  stageChanges()

  assertVersions("1000.0.0-SNAPSHOT", "999.10.0-SNAPSHOT", "999.9.10-SNAPSHOT")

  createCommit(s"Added $testFilename")

  assert(!commitIsTagged())

  assertVersions("1000.0.0-SNAPSHOT", "999.10.0-SNAPSHOT", "999.9.10-SNAPSHOT")

  createTag("v1000.0.0-M1")

  assertVersions("1000.0.0-M1", "1000.0.0-M1", "1000.0.0-M1")

  println(fansi.Color.Yellow("Making a change"))
  os.write.over(testPath, "Testing changes after prerelease")
  stageChanges()
  createCommit(s"Updated $testFilename")

  assertVersions("1000.0.0-SNAPSHOT", "1000.0.0-SNAPSHOT", "1000.0.0-SNAPSHOT")
}
