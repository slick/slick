package com.typesafe.slick.docs

//#imports
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

import slick.jdbc.H2Profile
import slick.jdbc.H2Profile.api.*
//#imports

object CodeGenerator extends App {
  val uri = "#slick.db.default"
  val profile = "slick.jdbc.H2Profile"
  val jdbcDriver = "org.h2.Driver"
  val url = "jdbc:postgresql://localhost/test"
  val outputFolder = ""
  val pkg = "demo"
  val user = ""
  val password = ""
  if(false){
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
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
    import slick.codegen.SourceCodeGenerator
    // fetch data model
    val modelAction = H2Profile.createModel(Some(H2Profile.defaultTables)) // you can filter specific tables here
    val modelFuture = db.run(modelAction)
    // customize code generator
    val codegenFuture = modelFuture.map(model => new SourceCodeGenerator(model) {
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
    })
    codegenFuture.onComplete {
      case Success(codegen) =>
        codegen.writeToFile(
          "slick.jdbc.H2Profile","some/folder/","some.packag","Tables","Tables.scala"
        )
      case Failure(_) =>
    }
    //#customization
  }
}
