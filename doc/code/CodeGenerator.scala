package com.typesafe.slick.docs

//#imports
import slick.jdbc.H2Profile
//#imports

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

import cats.effect.unsafe.implicits.global
import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.jdbc.H2Profile.api._
import slick.codegen.SourceCodeGenerator

object CodeGenerator {
  // The same in-memory database as slick.db.default in application.conf
  private val url = ExampleDatabases.h2Url("codegen")

  def main(args: Array[String]): Unit =
    try runExamples()
    finally ExampleDatabases.shutdown(url)

  private def runExamples(): Unit = {
    val uri = "#slick.db.default"
    val profile = "slick.jdbc.H2Profile"
    val jdbcDriver = "org.h2.Driver"
    val outputFolder = Files.createTempDirectory("slick-codegen-doc").toString
    val pkg = "demo"
    val user = ""
    val password = ""

    val dc = DatabaseConfig.forURL(H2Profile, url, driver = jdbcDriver)
    val wideColumns = (1 to 23).map(i => s"C$i int").mkString(", ")
    Database.resource(dc).use { db =>
      db.run(DBIO.seq(
        sqlu"create table COFFEES (COF_NAME varchar(255) primary key, PRICE double)",
        sqlu"create table SPECIALS (ID int primary key, SOME_SPECIAL_COLUMN_NAME int)",
        sqlu"create table WIDES (#$wideColumns)"
      ))
    }.unsafeRunSync()

    // Each runner should write the generated code to outputFolder/demo/Tables.scala
    val generatedFile = Paths.get(outputFolder, pkg, "Tables.scala")
    def checkGenerated(runner: String): Unit = {
      assert(Files.exists(generatedFile), s"The $runner runner did not write $generatedFile")
      val code = new String(Files.readAllBytes(generatedFile), "UTF-8")
      assert(code.contains("lazy val Coffees"), s"The $runner runner's code has no table for COFFEES:\n$code")
      Files.delete(generatedFile)
    }

    Database.resource(dc).use { db =>
        //#default-runner-uri
        slick.codegen.SourceCodeGenerator.main(
          Array(uri, outputFolder)
        )
        //#default-runner-uri
        checkGenerated("config URI")
        //#default-runner
        slick.codegen.SourceCodeGenerator.main(
          Array(profile, jdbcDriver, url, outputFolder, pkg)
        )
        //#default-runner
        checkGenerated("default")
        //#default-runner-with-auth
        slick.codegen.SourceCodeGenerator.main(
          Array(profile, jdbcDriver, url, outputFolder, pkg, user, password)
        )
        //#default-runner-with-auth
        checkGenerated("with-auth")
        //#customization
        // fetch data model
        val modelAction = H2Profile.createModel(Some(H2Profile.defaultTables)) // you can filter specific tables here
        // customize code generator
        db.run(modelAction).map { model =>
          val codegen = new SourceCodeGenerator(model) {
            // override mapped table and class name
            override def entityName =
              dbTableName => dbTableName.dropRight(1).toLowerCase.toCamelCase
            override def tableName =
              dbTableName => dbTableName.toLowerCase.toCamelCase

            // add some custom import
            override def code = "import foo.{MyCustomType,MyCustomTypeMapper}" + "\n" + super.code

            // override table generator
            override def Table = new Table(_){
              // disable entity class generation for tables with more than 22 columns
              override def hugeClassEnabled = false

              // override contained column generator
              override def Column = new Column(_){
                // use the data model member of this column to change the Scala type,
                // e.g. to a custom enum or anything else
                override def rawType =
                  if(this.model.name == "SOME_SPECIAL_COLUMN_NAME") "MyCustomType" else super.rawType
              }
            }
          }
          codegen.writeToFile(
            "slick.jdbc.H2Profile","some/folder/","some.packag","Tables","Tables.scala"
          )
        }
        //#customization
    }.unsafeRunSync()

    // The customization example writes to some/folder/ in the current directory. Delete only that file and the
    // directories it created, so that nothing else under some/ is touched.
    val customFile = Paths.get("some/folder/some/packag/Tables.scala")
    val customCode = new String(Files.readAllBytes(customFile), "UTF-8")
    Files.delete(customFile)
    Iterator.iterate(customFile.getParent)(_.getParent).takeWhile(_ != null)
      .takeWhile(isEmptyDirectory)
      .foreach(Files.delete)
    deleteRecursively(Paths.get(outputFolder))

    def checkCustom(claim: String, holds: Boolean): Unit =
      assert(holds, s"The customized generator's code does not show that $claim:\n$customCode")
    checkCustom("it adds the custom import", customCode.contains("import foo.{MyCustomType,MyCustomTypeMapper}"))
    checkCustom("entityName drops the last letter (COFFEES -> Coffee)", customCode.contains("case class Coffee("))
    checkCustom("tableName camel-cases the table name (COFFEES -> Coffees)", customCode.contains("class Coffees("))
    checkCustom(
      "rawType maps SOME_SPECIAL_COLUMN_NAME to MyCustomType",
      customCode.contains("""column[Option[MyCustomType]]("SOME_SPECIAL_COLUMN_NAME")""")
    )
    checkCustom(
      "hugeClassEnabled = false generates an HList type instead of a case class for a table with more than 22 columns",
      customCode.contains("type Wide = HCons[") && !customCode.contains("case class Wide(")
    )
  }


  private def isEmptyDirectory(dir: Path): Boolean = {
    val entries = Files.list(dir)
    try !entries.iterator().hasNext
    finally entries.close()
  }

  private def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      val paths = Files.walk(path)
      try paths.iterator().asScala.toList.reverse.foreach(Files.delete)
      finally paths.close()
    }
}
