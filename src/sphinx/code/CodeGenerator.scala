package com.typesafe.slick.docs
import scala.slick.driver.H2Driver.simple._
import scala.slick.driver.H2Driver

object CodeGenerator extends App {
  val slickDriver = "scala.slick.driver.H2Driver"
  val jdbcDriver = "org.h2.Driver"
  val url = "jdbc:postgresql://localhost/test"
  val outputFolder = ""
  val pkg = "demo"
  if(false){
    val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
    //#default-runner
    scala.slick.model.codegen.SourceCodeGenerator.main(
      Array(slickDriver, jdbcDriver, url, outputFolder, pkg)
    )
    //#default-runner
    //#customization
    import scala.slick.jdbc.meta.createModel
    import scala.slick.model.codegen.SourceCodeGenerator
    // fetch data model
    val model = db.withSession{ implicit session =>
      createModel(H2Driver.getTables.list,H2Driver) // you can filter specific tables here
    }
    // customize code generator
    val codegen = new SourceCodeGenerator(model){
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
    }
    codegen.writeToFile(
      "scala.slick.driver.H2Driver","some/folder/","some.packag","Tables","Tables.scala"
    )
    //#customization
  }
}
