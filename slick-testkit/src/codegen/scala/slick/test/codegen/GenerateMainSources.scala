package slick.test.codegen

import java.sql.Blob

import scala.concurrent.ExecutionContext.Implicits.global

import slick.codegen.SourceCodeGenerator
import slick.dbio.DBIO

import com.typesafe.slick.testkit.util.{JdbcTestDB, StandardTestDBs, TestCodeGenerator}


/** Generates files for GeneratedCodeTest */
object GenerateMainSources extends TestCodeGenerator {
  def packageName = "slick.test.codegen.generated"
  def defaultTestCode(c: Config): String = "slick.test.codegen.GeneratedCodeTest.test" + c.objectName

  lazy val configurations = Seq(
    new Config("CG1", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2.sql")),
    new Config("CG2", StandardTestDBs.HsqldbMem, "HsqldbMem", Seq("/dbs/hsqldb.sql")),
    new Config("CG3", StandardTestDBs.SQLiteMem, "SQLiteMem", Seq("/dbs/sqlite.sql")),
    new Config("DB2", StandardTestDBs.DB2, "DB2", Seq("/dbs/db2.sql")),
    new Config("DerbyMem", StandardTestDBs.DerbyMem, "DerbyMem", Seq("/dbs/derby.sql")),
    new Config("CG7", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2.sql")) {
      override def generator = tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
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
      override def generator = tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
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
import CustomTyping.SimpleA
//type SimpleA = CustomTyping.SimpleA
//val  SimpleA = CustomTyping.SimpleA
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
      override def generator = tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def Table = new Table(_){
          override def autoIncLast = true
          override def Column = new Column(_){
            override def asOption = autoInc
          }
        }
      })
    },
    new UUIDConfig("CG10", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/uuid-h2.sql")),
    new Config("CG11", StandardTestDBs.H2Mem, "H2Mem", Seq("/dbs/h2-simple.sql")) {
      override def generator = tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
        override def Table = new Table(_){
          override def Column = new Column(_){
            override def asOption = true
          }
        }
      })
    },
    new Config("Postgres1", StandardTestDBs.Postgres, "Postgres", Nil) {
      import tdb.profile.api._
      class A(tag: Tag) extends Table[(Int, Array[Byte], Blob)](tag, "a") {
        def id = column[Int]("id")
        def ba = column[Array[Byte]]("ba")
        def blob = column[Blob]("blob")
        def * = (id, ba, blob)
      }
      override def generator =
        TableQuery[A].schema.create >>
        tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_))
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
    new UUIDConfig("Postgres2", StandardTestDBs.Postgres, "Postgres", Seq("/dbs/uuid-postgres.sql")),
    new Config("Postgres3", StandardTestDBs.Postgres, "Postgres", Seq("/dbs/postgres.sql")) {
      override def testCode: String =
      """import slick.ast.{FieldSymbol, Select}
        |import slick.jdbc.meta.MTable
        |import slick.relational.RelationalProfile
        |DBIO.seq(
        |  schema.create,
        |  MTable.getTables(Some(""), Some("public"), None, None).map { tables =>
        |    def optionsOfColumn(c: slick.lifted.Rep[_]) =
        |      c.toNode.asInstanceOf[Select].field.asInstanceOf[FieldSymbol].options.toList
        |    //val smallserialOptions = optionsOfColumn(TestDefault.baseTableRow.smallintAutoInc)
        |    val serialOptions = optionsOfColumn(TestDefault.baseTableRow.intAutoInc)
        |    val bigserialOptions = optionsOfColumn(TestDefault.baseTableRow.bigintAutoInc)
        |    val char1EmptyOptions = optionsOfColumn(TestDefault.baseTableRow.char1DefaultEmpty)
        |    val char1ValidOptions = optionsOfColumn(TestDefault.baseTableRow.char1DefaultValid)
        |    val char1InvalidOptions = optionsOfColumn(TestDefault.baseTableRow.char1DefaultInvalid)
        |    //assertTrue("smallint_auto_inc should be AutoInc", smallserialOptions.exists(option => (option equals TestDefault.baseTableRow.O.AutoInc)))
        |    assertTrue("int_auto_inc should be AutoInc", serialOptions.exists(option => (option equals TestDefault.baseTableRow.O.AutoInc)))
        |    assertTrue("bigint_auto_inc should be AutoInc", bigserialOptions.exists(option => (option equals TestDefault.baseTableRow.O.AutoInc)))
        |    assertTrue("default value of char1_default_empty should be ' '", char1EmptyOptions.exists(option => (option equals TestDefault.baseTableRow.O.Default(Some(' ')))))
        |    assertTrue("default value of char1_default_valid should be 'a'", char1ValidOptions.exists(option => (option equals TestDefault.baseTableRow.O.Default(Some('a')))))
        |    assertTrue("default value of char1_default_invalid should not exist", char1InvalidOptions.forall(option => (option.isInstanceOf[RelationalProfile.ColumnOption.Default[_]])))
        |  }
        |)
      """.stripMargin
    },
    new Config("MySQL1", StandardTestDBs.MySQL, "MySQL", Nil) {
      import tdb.profile.api._
      class A(tag: Tag) extends Table[String](tag, "a") {
        def quote = column[String]("x", O.Default("\"\"")) // column name with double quote
        def * = quote
      }
      override def generator =
        TableQuery[A].schema.create >>
        tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_))
      override def testCode =
        """
          |  val a1 = ARow("e")
          |  DBIO.seq(
          |    schema.create,
          |    A += a1,
          |    A.result.map { case Seq(ARow(quote)) => assertEquals("e", quote) }
          |  ).transactionally
        """.stripMargin
    },
    new Config("MySQL", StandardTestDBs.MySQL, "MySQL", Seq("/dbs/mysql.sql") ){
      override def generator: DBIO[SourceCodeGenerator] =
        tdb.profile.createModel(ignoreInvalidDefaults=false).map(new SourceCodeGenerator(_){
          override def parentType = Some("com.typesafe.slick.testkit.util.TestCodeRunner.TestCase")
          override def code = {
            val testcode =
              """
                |  val entry = DefaultNumericRow(d0 = scala.math.BigDecimal(123.45), d1 = scala.math.BigDecimal(90), d3 = 0)
                |  val createStmt = schema.create.statements.mkString
                |  assertTrue("Schema name should be `slick_test`" , TableName.baseTableRow.schemaName.getOrElse("") == "slick_test" )
                |  assertTrue(createStmt contains "`entry1` LONGTEXT")
                |  assertTrue(createStmt contains "`entry2` MEDIUMTEXT")
                |  assertTrue(createStmt contains "`entry3` TEXT")
                |  assertTrue(createStmt contains "`entry4` VARCHAR(255)")
                |  def assertType(r: Rep[_], t: String) = assert(r.toNode.nodeType.toString == s"$t'")
                |  assertType(TableName.baseTableRow.id, "Int")
                |  assertType(TableName.baseTableRow.uti, "Short")
                |  assertType(TableName.baseTableRow.si, "Int")
                |  assertType(TableName.baseTableRow.mi, "Int")
                |  assertType(TableName.baseTableRow.ui, "Long")
                |  assertType(TableName.baseTableRow.bi, "Long")
                |  //assertType(TableName.baseTableRow.ubi, "BigInt")
                |  val bitEntries = Seq(BitTestRow(true), BitTestRow(false, true, true))
                |  DBIO.seq(
                |    schema.create,
                |    TableName += TableNameRow(0, 0, 0, 0, 0, 0/*, BigInt(0)*/),
                |    BitTest ++= bitEntries,
                |    BitTest.result.map{assertEquals(_, bitEntries)},
                |    TableName.result.map{ case rows: Seq[TableNameRow] => assert(rows.length == 1) },
                |    DefaultNumeric += entry,
                |    DefaultNumeric.result.head.map{ r =>  assertEquals(r , entry) }
                |  )
              """.stripMargin
            s"""
               |lazy val tdb = $fullTdbName
               |def test = {
               |  import org.junit.Assert._
               |  import scala.concurrent.ExecutionContext.Implicits.global
               |  $testcode
               |}
           """.stripMargin + super.code
          }
        })
    },
    new UUIDConfig("SQLServer1", StandardTestDBs.SQLServerSQLJDBC, "SQLServerSQLJDBC", Seq("/dbs/uuid-sqlserver.sql")),
    new Config("EmptyDB", StandardTestDBs.H2Mem, "H2Mem", Nil),
    new Config("Oracle1", StandardTestDBs.Oracle, "Oracle", Seq("/dbs/oracle1.sql")) {
      override def useSingleLineStatements = true
      override def testCode =
        """
          |  val entry = PersonRow(1)
          |  assertEquals(scala.math.BigDecimal(0), entry.age)
          |  DBIO.seq (
          |    schema.create,
          |    Person += entry,
          |    Person.result.head.map{ r =>  assertEquals(r , entry) }
          |  )
        """.stripMargin
    }
  )

  //Unified UUID config
  class UUIDConfig(objectName: String, tdb: JdbcTestDB, tdbName: String, initScripts: Seq[String])
    extends Config(objectName, tdb, tdbName, initScripts) {
    override def generator = tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_) {
      override def Table = new Table(_) {
        override def Column = new Column(_){
          override def defaultCode: Any => String = {
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
        |  val u3 = UUID.randomUUID()
        |  val u4 = UUID.randomUUID()
        |  val p1 = PersonRow(1, u1, uuidFunc = Some(u3))
        |  val p2 = PersonRow(2, u2, uuidFunc = Some(u4))
        |
        |  def assertAll(all: Seq[PersonRow]) = {
        |    assertEquals( "2 == all.size", 2, all.size )
        |    assertEquals( "Set(1,2) == all.map(_.id).toSet", Set(1,2), all.map(_.id).toSet )
        |    assertEquals( "Set(u1, u2) == all.map(_.uuid).toSet", Set(u1, u2), all.map(_.uuid).toSet )
        |    assertEquals( "Set(Some(u3), Some(u4)) == all.map(_.uuidFunc).toSet", Set(Some(u3), Some(u4)), all.map(_.uuidFunc).toSet )
        |    //it should contain sample UUID
        |    assertTrue("all.forall(_.uuidDef == Some(defaultUUID))", all.forall(_.uuidDef == Some(defaultUUID)))
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
