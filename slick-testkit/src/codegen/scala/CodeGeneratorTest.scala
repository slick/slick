package scala.slick.test.meta.codegen

import scala.slick.meta.codegen.SourceCodeGenerator
import scala.slick.driver._
import scala.slick.jdbc.JdbcBackend
import scala.slick.driver.JdbcDriver
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
        generator(config)(session).writeToFile(profile=slickDriver, folder=args(0), pkg="scala.slick.test.meta.codegen.generated", objectName, fileName=objectName+".scala" )
      }
    }
    ;{
      // generates code for CodeGenRoundTripTest
      // This is generated using Derby currently because Derby strips column size of some columns,
      // which works with all backend. If the code was generated using meta data where the size is included it would fail in derby and hsqldb.
      // The code is tested using all enabled drivers. We should also diversify generation as well at some point.
      val driver = scala.slick.driver.H2Driver
      val url = "jdbc:h2:mem:test3"
      val jdbcDriver = "org.h2.Driver"
      object Tables extends Tables(driver)
      import Tables._
      import Tables.profile.simple._
      val ddl = posts.ddl ++ categories.ddl ++ typeTest.ddl ++ large.ddl
      //println(ddl.createStatements.mkString("\n"))
      val db = Database.forURL(url=url,driver=jdbcDriver)
      val gen = db.withSession{ implicit session =>
        ddl.create
        (new SourceCodeGenerator(driver.metaModel(session)))
      }
      val pkg = "scala.slick.test.meta.codegen.roundtrip"
      gen.writeToFile( "scala.slick.driver.H2Driver", args(0), pkg )
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
            override def rawType = meta.name match {
              case "A1" => "Bool"
              case _ => super.rawType
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
class Tables(val profile: JdbcProfile){
  import profile.simple._
  case class Category(id: Int, name: String)
  class Categories(tag: Tag) extends Table[Category](tag, "categories") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def * = (id, name) <> (Category.tupled,Category.unapply)
    def idx = index("IDX_NAME",name)
  }
  val categories = TableQuery[Categories]

  class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "posts") {
    def id = column[Int]("id")
    def title = column[String]("title")
    def category = column[Option[Int]]("category")
    def * = (id, title, category)
    def categoryFK = foreignKey("category_fk", category, categories)(_.id)
  }
  val posts = TableQuery[Posts]

  class TypeTest(tag: Tag) extends Table[(Boolean,Byte,Short,Int,Long,Float,Double,String,java.sql.Date,java.sql.Time,java.sql.Timestamp,java.sql.Blob)](tag, "TYPE_TEST") {
    def Boolean = column[Boolean]("Boolean",O.Default(true))
    def Byte = column[Byte]("Byte")
    def Short = column[Short]("Short")
    def Int = column[Int]("Int",O.Default(5))
    def Long = column[Long]("Long",O.Default(5L))
    //def java_math_BigInteger = column[java.math.BigInteger]("java_math_BigInteger")
    def Float = column[Float]("Float",O.Default(9.999F))
    def Double = column[Double]("Double",O.Default(9.999))
    //def java_math_BigDecimal = column[java.math.BigDecimal]("java_math_BigDecimal")
    def String = column[String]("String",O.Default("someDefaultString"))
    def java_sql_Date = column[java.sql.Date]("java_sql_Date")
    def java_sql_Time = column[java.sql.Time]("java_sql_Time")
    def java_sql_Timestamp = column[java.sql.Timestamp]("java_sql_Timestamp")
    def java_sql_Blob = column[java.sql.Blob]("java_sql_Blob")
    def java_sql_Clob = column[java.sql.Clob]("java_sql_Clob")
    def * = (Boolean,Byte,Short,Int,Long,Float,Double,String,java_sql_Date,java_sql_Time,java_sql_Timestamp,java_sql_Blob)
    def pk = primaryKey("PK", (Int,Long))
  }
  val typeTest = TableQuery[TypeTest]

  // testing table larger 22 columns (code gen round trip does not preserve structure of the * projection or names of mapped to classes)
  case class Part(i1: Int, i2: Int, i3: Int, i4: Int, i5: Int, i6: Int)
  case class Whole(id: Int, p1: Part, p2: Part, p3: Part, p4: Part)
  class Large(tag: Tag) extends Table[Whole](tag, "LARGE") {
    def id = column[Int]("id", O.PrimaryKey)
    def p1i1 = column[Int]("p1i1")
    def p1i2 = column[Int]("p1i2")
    def p1i3 = column[Int]("p1i3")
    def p1i4 = column[Int]("p1i4")
    def p1i5 = column[Int]("p1i5")
    def p1i6 = column[Int]("p1i6")
    def p2i1 = column[Int]("p2i1")
    def p2i2 = column[Int]("p2i2")
    def p2i3 = column[Int]("p2i3")
    def p2i4 = column[Int]("p2i4")
    def p2i5 = column[Int]("p2i5")
    def p2i6 = column[Int]("p2i6")
    def p3i1 = column[Int]("p3i1")
    def p3i2 = column[Int]("p3i2")
    def p3i3 = column[Int]("p3i3")
    def p3i4 = column[Int]("p3i4")
    def p3i5 = column[Int]("p3i5")
    def p3i6 = column[Int]("p3i6")
    def p4i1 = column[Int]("p4i1")
    def p4i2 = column[Int]("p4i2")
    def p4i3 = column[Int]("p4i3")
    def p4i4 = column[Int]("p4i4")
    def p4i5 = column[Int]("p4i5")
    def p4i6 = column[Int]("p4i6")
    def * = (
      id,
      (p1i1, p1i2, p1i3, p1i4, p1i5, p1i6),
      (p2i1, p2i2, p2i3, p2i4, p2i5, p2i6),
      (p3i1, p3i2, p3i3, p3i4, p3i5, p3i6),
      (p4i1, p4i2, p4i3, p4i4, p4i5, p4i6)
    ).shaped <> ({ case (id, p1, p2, p3, p4) =>
      // We could do this without .shaped but then we'd have to write a type annotation for the parameters
      Whole(id, Part.tupled.apply(p1), Part.tupled.apply(p2), Part.tupled.apply(p3), Part.tupled.apply(p4))
    }, { w: Whole =>
      def f(p: Part) = Part.unapply(p).get
      Some((w.id, f(w.p1), f(w.p2), f(w.p3), f(w.p4)))
    })
  }
  val large = TableQuery[Large]
}

