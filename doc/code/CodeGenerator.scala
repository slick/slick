package com.typesafe.slick.docs

//#imports
import slick.jdbc.H2Profile
//#imports

import slick.cats.Database
import slick.jdbc.DatabaseConfig
import slick.codegen.SourceCodeGenerator

object CodeGenerator {
  def main(args: Array[String]): Unit = {
    val uri = "#slick.db.default"
    val profile = "slick.jdbc.H2Profile"
    val jdbcDriver = "org.h2.Driver"
    val url = "jdbc:postgresql://localhost/test"
    val outputFolder = ""
    val pkg = "demo"
    val user = ""
    val password = ""
    if(false){
      implicit val runtime: _root_.cats.effect.unsafe.IORuntime = _root_.cats.effect.unsafe.IORuntime.global
      val dc = DatabaseConfig.forURL(H2Profile, "jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
      Database.resource(dc).use { db =>
        //#default-runner-uri
        slick.codegen.SourceCodeGenerator.main(
          Array(uri, outputFolder)
        )
        //#default-runner-uri
        //#default-runner
        slick.codegen.SourceCodeGenerator.main(
          Array(profile, jdbcDriver, url, outputFolder, pkg)
        )
        //#default-runner
        //#default-runner-with-auth
        slick.codegen.SourceCodeGenerator.main(
          Array(profile, jdbcDriver, url, outputFolder, pkg, user, password)
        )
        //#default-runner-with-auth
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
    }
  }
}
