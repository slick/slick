package scala.slick.test.meta.codegen

import scala.slick.meta.codegen.SourceCodeGenerator
import scala.slick.driver._
import scala.slick.jdbc.JdbcBackend
import scala.slick.jdbc.meta.{MTable,createMetaModel}
import scala.slick.meta.Model

/** Generates files for GeneratedCodeTest */
object CodeGeneratorTest {
  val testdbLocation = "slick-testkit/src/codegen/resources/dbs/"
  def main(args: Array[String]) {
    for( config <- configurations ){
      import config._
      val db = slickDriverObject.simple.Database.forURL(url=url,driver=jdbcDriver,user="",password="")
      db.withSession{ implicit session =>
        generator(config)(session).writeToFile(driver=slickDriver, folder=args(0), pkg="scala.slick.test.meta.codegen.generated", objectName, fileName=objectName+".scala" )
      }
    }
  }

  lazy val configurations = Seq(
    new H2Config("CG1", Seq("create.sql","populate.sql")),
    Config(
      "CG2",
      "jdbc:hsqldb:"+testdbLocation+"hsql/supp;shutdown=true",
      HsqldbDriver, "scala.slick.driver.HsqldbDriver", "org.hsqldb.jdbcDriver",
      config => session => new MySourceCodeGenerator(HsqldbDriver.metaModel(session),config)
    ),
    Config("CG3", "jdbc:sqlite:"+testdbLocation+"sqlite/sqlite-supp.db",
      SQLiteDriver, "scala.slick.driver.SQLiteDriver", "org.sqlite.JDBC",
      config => session => new MySourceCodeGenerator(SQLiteDriver.metaModel(session),config)
    ),
    new H2Config("CG4", Seq("create-fk-1.sql")),
    new H2Config("CG5", Seq("create-fk-2.sql")),
    // CG5b tests that foreign keys to not included tables are removed
    new H2Config("CG5b", Seq("create-fk-2.sql"),
      config => session => new MySourceCodeGenerator(
        createMetaModel(
          H2Driver.getTables.list()(session).filter(_.name.name == "a"),
          H2Driver
        )(session),
        config
      )
    ),
    new H2Config("CG6", Seq("create-ainc.sql")),
    new H2Config("CG7", Seq("create.sql","populate.sql"),
      config => session => new MySourceCodeGenerator(H2Driver.metaModel(session),config){
        override def entityName = {
          case "COFFEES" => "Coff"
          case other => super.entityName(other)
        }
        override def tableName = {
          case "COFFEES" => "Coffs"
          case "SUPPLIERS" => "Supps"
          case other => super.tableName(other)
        }
      }
    ),
    new H2Config("CG8", Seq("create-simple.sql"),
      config => session => new MySourceCodeGenerator(H2Driver.metaModel(session),config){
        override def Table = new Table(_){
          override def entityClassEnabled = false
          override def mappingEnabled     = true
          override def code = {
            if(meta.name.table == "SIMPLE_AS"){
              Seq("""
import scala.slick.test.meta.codegen.CustomTyping._
import scala.slick.test.meta.codegen.CustomTyping
type SimpleA = CustomTyping.SimpleA
val  SimpleA = CustomTyping.SimpleA
                """.trim) ++ super.code
            } else super.code
          }
          override def Column = new Column(_){
            override def tpe = meta.name match {
              case "A1" => "Bool"
              case _ => super.tpe
            }
          }
        }
      }
    )
  )
  class MySourceCodeGenerator(meta:Model, config: Config) extends SourceCodeGenerator(meta){
    override def entityName = sqlName => {
      val baseName = super.entityName(sqlName)
      if(baseName.dropRight(3).last == 's') baseName.dropRight(4)
      else baseName
    }
    override def code = {
      import config._
      s"""
val driver = $slickDriver
val database = Database.forURL(url=""\"$url""\",driver="$jdbcDriver",user="",password="")
      """.trim() + "\n" + super.code
    }
  }
  case class Config(
    objectName: String,
    url: String,
    slickDriverObject: JdbcDriver,
    slickDriver: String,
    jdbcDriver: String,
    generator: Config => JdbcBackend#Session => SourceCodeGenerator
  )
  class H2Config(
    objectName: String,
    inits: Seq[String],
    generator: Config => JdbcBackend#Session => SourceCodeGenerator
      = config => session => new MySourceCodeGenerator(H2Driver.metaModel(session),config)
  ) extends Config(
    objectName,
    "jdbc:h2:mem:test3;INIT="+inits.map("runscript from '"+testdbLocation+"h2mem/"+_+"'").mkString("\\;"),
    H2Driver,
    "scala.slick.driver.H2Driver",
    "org.h2.Driver",
    generator
  )
}
