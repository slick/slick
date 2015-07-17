package com.typesafe.slick.docs

import scala.concurrent.ExecutionContext.Implicits.global
import slick.driver.H2Driver.api._
import slick.driver.H2Driver

object CodeGenerator extends App {
  val slickDriver = "slick.driver.H2Driver"
  val jdbcDriver = "org.h2.Driver"
  val url = "jdbc:postgresql://localhost/test"
  val outputFolder = ""
  val pkg = "demo"
  val user = ""
  val password = ""
  if(false){
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
    //#default-runner
    slick.codegen.SourceCodeGenerator.main(
      Array(slickDriver, jdbcDriver, url, outputFolder, pkg)
    )
    //#default-runner
    //#default-runner-with-auth
    slick.codegen.SourceCodeGenerator.main(
      Array(slickDriver, jdbcDriver, url, outputFolder, pkg, user, password)
    )
    //#default-runner-with-auth
    //#customization
    import slick.codegen.SourceCodeGenerator
    // fetch data model
    val modelAction = H2Driver.createModel(Some(H2Driver.defaultTables)) // you can filter specific tables here
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
        // disable entity class generation and mapping
        override def EntityType = new EntityType{
          override def classEnabled = false
        }

         // override contained column generator
        override def Column = new Column(_){
          // use the data model member of this column to change the Scala type,
          // e.g. to a custom enum or anything else
          override def rawType =
            if(model.name == "SOME_SPECIAL_COLUMN_NAME") "MyCustomType" else super.rawType
        }
      }
    })
    codegenFuture.onSuccess { case codegen =>
      codegen.writeToFile(
        "slick.driver.H2Driver","some/folder/","some.packag","Tables","Tables.scala"
      )
    }
    //#customization
  }
}
