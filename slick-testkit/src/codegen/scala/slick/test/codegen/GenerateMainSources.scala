package slick.test.codegen

import java.io.File
import java.sql.Blob

import com.typesafe.slick.testkit.util.{TestCodeGenerator, InternalJdbcTestDB, StandardTestDBs, JdbcTestDB}

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
object GenerateMainSources extends TestCodeGenerator {
  def packageName = "slick.test.codegen.generated"
  def defaultTestCode(c: Config): String = "slick.test.codegen.GeneratedCodeTest.test" + c.objectName

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
    new UUIDConfig("CG10", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/uuid.sql")),
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
                |  /* default UUID, which is the same as for 'uuid.sql' */
                |  val defaultUUID = java.util.UUID.fromString("2f3f866c-d8e6-11e2-bb56-50e549c9b654")
                |  /* convert UUID */
                |  implicit object GetUUID extends slick.jdbc.GetResult[java.util.UUID] {
                |    def apply(rs: slick.jdbc.PositionedResult) = rs.nextObject().asInstanceOf[java.util.UUID]
                |  }
                |  /* convert Option[UUID] for H2 */
                |  implicit object GetOptionUUID extends slick.jdbc.GetResult[Option[java.util.UUID]] {
                |    def apply(rs: slick.jdbc.PositionedResult) = Option(rs.nextObject().asInstanceOf[java.util.UUID])
                |  }
                """.stripMargin) ++ super.code
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
          |
          |  def assertAll(all: Seq[PersonRow]) = {
          |    assertEquals( 2, all.size )
          |    assertEquals( Set(1,2), all.map(_.id).toSet )
          |    assertEquals( Set(u1, u2), all.map(_.uuid).toSet )
          |    //it should contain sample UUID
          |    assert(all.forall(_.uuidDef == Some(defaultUUID)))
          |  }
          |
          |  DBIO.seq(
          |    schema.create,
          |    Person += p1,
          |    Person += p2,
          |    Person.result.map(assertAll)
          |  ).transactionally
        """.stripMargin
    }
}
