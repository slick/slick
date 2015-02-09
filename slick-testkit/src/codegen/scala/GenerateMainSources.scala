package scala.slick.test.codegen

import java.io.File

import com.typesafe.slick.testkit.util.{InternalJdbcTestDB, StandardTestDBs, JdbcTestDB}

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Codec, Source}
import scala.slick.dbio.DBIO
import scala.slick.codegen.{OutputHelpers, SourceCodeGenerator}
import scala.slick.driver._
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver
import scala.slick.jdbc.meta.MTable
import scala.slick.model.Model

/** Generates files for GeneratedCodeTest */
object GenerateMainSources {
  lazy val configurations = Seq(
    new Config("CG1", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2.sql")),
    new Config("CG2", StandardTestDBs.HsqldbMem, "HsqldbMem", Seq("/dbs/hsqldb.sql")),
    new Config("CG3", StandardTestDBs.SQLiteMem, "SQLiteMem", Seq("/dbs/sqlite.sql")),
    new Config("CG7", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2.sql")) {
      override def generator = tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def entityName = {
          case "COFFEES" => "Coff"
          case other => super.entityName(other)
        }
        override def tableName = {
          case "COFFEES" => "Coffs"
          case "SUPPLIERS" => "Supps"
          case other => super.tableName(other)
        }
        override def code = "trait AA; trait BB\n" + super.code
        override def Table = new Table(_){
          override def EntityType = new EntityType{
            override def parents = Seq("AA","BB")
          }
          override def TableClass = new TableClass{
            override def parents = Seq("AA","BB")
          }
        }
      })
    },
    new Config("CG8", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2-simple.sql")) {
      override def generator = tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def Table = new Table(_){
          override def EntityType = new EntityType{
            override def enabled = false
          }
          override def mappingEnabled = true
          override def code = {
            if(model.name.table == "SIMPLE_AS"){
              Seq("""
import scala.slick.test.codegen.CustomTyping._
import scala.slick.test.codegen.CustomTyping
type SimpleA = CustomTyping.SimpleA
val  SimpleA = CustomTyping.SimpleA
                  """.trim) ++ super.code
            } else super.code
          }
          override def Column = new Column(_){
            override def rawType = model.name match {
              case "A1" => "Bool"
              case _ => super.rawType
            }
          }
        }
      })
    },
    new Config("CG9", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2.sql")) {
      override def generator = tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def Table = new Table(_){
          override def autoIncLastAsOption = true
        }
      })
    }
  )

  def packageName = "scala.slick.test.codegen.generated"

  def main(args: Array[String]): Unit = {
    val clns = configurations.flatMap(_.generate(args(0)).toSeq)
    new OutputHelpers {
      def indent(code: String): String = code
      def code: String = ""
    }.writeStringToFile(
      s"""
         |package $packageName
         |object AllTests {
         |  val clns = Seq(${clns.map("\"" + _ + "\"").mkString(", ")})
         |}
       """.stripMargin, args(0), packageName, "AllTests.scala"
      )
  }

  class Config(val objectName: String, val tdb: JdbcTestDB, tdbName: String, initScripts: Seq[String]) { self =>

    def slickDriver = tdb.driver.getClass.getName.replaceAll("\\$", "")

    def fullTdbName = StandardTestDBs.getClass.getName.replaceAll("\\$", "") + "." + tdbName

    def generate(dir: String): Option[String] = if(tdb.isEnabled || tdb.isInstanceOf[InternalJdbcTestDB]) {
      tdb.cleanUpBefore()
      try {
        var init: DBIO[Any] = DBIO.successful(())
        var current: String = null
        initScripts.foreach { initScript =>
          import tdb.driver.api._
          Source.fromURL(self.getClass.getResource(initScript))(Codec.UTF8).getLines().foreach { s =>
            if(current eq null) current = s else current = current + "\n" + s
            if(s.trim.endsWith(";")) {
              init = init >> sqlu"#$current"
              current = null
            }
          }
          if(current ne null) init = init >> sqlu"#$current"
        }
        val db = tdb.createDB()
        try {
          val m = Await.result(db.run((init >> generator).withPinnedSession), Duration.Inf)
          m.writeToFile(profile=slickDriver, folder=dir, pkg=packageName, objectName, fileName=objectName+".scala" )
        } finally db.close
      }
      finally tdb.cleanUpAfter()
      Some(s"$packageName.$objectName")
    } else None

    def generator: DBIO[SourceCodeGenerator] =
      tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_))

    def testCode: String = "scala.slick.test.codegen.GeneratedCodeTest.test" + objectName

    class MyGen(model:Model) extends SourceCodeGenerator(model) {
      override def entityName = sqlName => {
        val baseName = super.entityName(sqlName)
        if(baseName.dropRight(3).last == 's') baseName.dropRight(4)
        else baseName
      }
      override def parentType = Some("scala.slick.test.codegen.GeneratedCodeTest.TestCase")
      override def code = {
        s"""
           |lazy val tdb = $fullTdbName
           |def test = {
           |  import org.junit.Assert._
           |  import scala.concurrent.ExecutionContext.Implicits.global
           |  $testCode
           |}
         """.stripMargin + super.code
      }
    }
  }
}
