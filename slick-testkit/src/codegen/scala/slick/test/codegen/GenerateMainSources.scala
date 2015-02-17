package slick.test.codegen

import java.io.File
import java.sql.Blob

import com.typesafe.slick.testkit.util.{InternalJdbcTestDB, StandardTestDBs, JdbcTestDB}

import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Codec, Source}
import slick.dbio.DBIO
import slick.codegen.{OutputHelpers, SourceCodeGenerator}
import slick.driver._
import slick.jdbc.JdbcBackend
import slick.driver.JdbcDriver
import slick.jdbc.meta.MTable
import slick.model.Model

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
import slick.test.codegen.CustomTyping._
import slick.test.codegen.CustomTyping
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
    },
    new UUIDConfig("CG10", StandardTestDBs.Postgres, "Postgres", Seq("/dbs/uuid.sql")),
    new Config("Postgres1", StandardTestDBs.Postgres, "Postgres", Nil) {
      import tdb.driver.api._
      class A(tag: Tag) extends Table[(Int, Array[Byte], Blob)](tag, "a") {
        def id = column[Int]("id")
        def ba = column[Array[Byte]]("ba")
        def blob = column[Blob]("blob")
        def * = (id, ba, blob)
      }
      override def generator =
        TableQuery[A].schema.create >>
        tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_))
      override def testCode =
        """
          |  import java.sql.Blob
          |  import javax.sql.rowset.serial.SerialBlob
          |  val a1 = ARow(1, Array[Byte](1,2,3), new SerialBlob(Array[Byte](4,5,6)))
          |  DBIO.seq(
          |    schema.create,
          |    A += a1,
          |    A.result.map { case Seq(ARow(id, ba, blob)) => assertEquals("1123", ""+id+ba.mkString) }
          |  ).transactionally
        """.stripMargin
    },
    new UUIDConfig("Postgres2", StandardTestDBs.Postgres, "Postgres", Seq("/dbs/uuid.sql"))
  )

  def packageName = "slick.test.codegen.generated"

  def main(args: Array[String]): Unit = try {
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
  } catch { case ex: Throwable =>
    ex.printStackTrace(System.err)
    System.exit(1)
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

    def testCode: String = "slick.test.codegen.GeneratedCodeTest.test" + objectName

    class MyGen(model:Model) extends SourceCodeGenerator(model) {
      override def entityName = sqlName => {
        val baseName = super.entityName(sqlName)
        if(baseName.dropRight(3).last == 's') baseName.dropRight(4)
        else baseName
      }
      override def parentType = Some("slick.test.codegen.GeneratedCodeTest.TestCase")
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

  //Unified UUID config
  class UUIDConfig(objectName: String, tdb: JdbcTestDB, tdbName: String, initScripts: Seq[String])
    extends Config(objectName, tdb, tdbName, initScripts) {
      override def generator =tdb.driver.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def Table = new Table(_) {
          override def Column = new Column(_){
            override def defaultCode: (Any) => String = {
              case v: java.util.UUID => s"""java.util.UUID.fromString("${v.toString}")"""
              case v => super.defaultCode(v)
            }
          }
          override def code = {
            Seq("""
                    /*default UUID, which is the same as for 'create-uuid.sql'*/
                    val defaultUUID = java.util.UUID.fromString("2f3f866c-d8e6-11e2-bb56-50e549c9b654")
                    /*convert UUID for H2*/
                    implicit object GetUUID extends slick.jdbc.GetResult[java.util.UUID] {
                      def apply(rs: slick.jdbc.PositionedResult) = rs.nextObject().asInstanceOf[java.util.UUID]
                    }
                    /*convert Option[UUID] for H2*/
                    implicit object GetOptionUUID extends slick.jdbc.GetResult[Option[java.util.UUID]] {
                      def apply(rs: slick.jdbc.PositionedResult) = Option(rs.nextObject().asInstanceOf[java.util.UUID])
                    }
                """.trim) ++ super.code
          }
        }
      })
      override def testCode =
        """
          |  import java.util.UUID
          |  val u1 = UUID.randomUUID()
          |  val u2 = UUID.randomUUID()
          |  val p1 = PersonRow(1, u1)
          |  val p2 = PersonRow(2, u2)
          |  DBIO.seq(
          |    schema.create,
          |    Person += p1,
          |    Person += p2,
          |    Person.result.map { case all => {
          |       assertEquals( 2, all.size )
          |       assertEquals( Set(1,2), all.map(_.id).toSet )
          |       assertEquals( Set(u1, u2), all.map(_.uuid).toSet )
          |       //it should contain sample UUID
          |       assert(all.forall(_.uuidDef == Some(defaultUUID)))
          |     }
          |    }
          |  ).transactionally
        """.stripMargin
    }
}
