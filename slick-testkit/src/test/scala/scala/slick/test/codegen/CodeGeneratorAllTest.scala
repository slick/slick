package scala.slick.test.codegen

import org.junit.Test
import scala.slick.codegen.SourceCodeGenerator
import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import com.typesafe.slick.testkit.util.StandardTestDBs._

object CodeGeneratorAllTest extends DBTestObject(H2Mem, SQLiteMem, Postgres, MySQL, DerbyMem, HsqldbMem)
class CodeGeneratorAllTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.simple._
  @Test def test { db.withSession{ implicit session =>

    case class Category(id: Int, name: String)
    class Categories(tag: Tag) extends Table[Category](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name) <> (Category.tupled,Category.unapply)
      def idx = index("IDX_NAME",name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "POSTS") {
      def id = column[Int]("id")
      def title = column[String]("title")
      def category = column[Option[Int]]("category")
      def * = (id, title, category)
      def categoryFK = foreignKey("category_fk", category, categories)(_.id.?)
    }
    val posts = TableQuery[Posts]

    (categories.ddl ++ posts.ddl).create 

    import scala.slick.jdbc.meta.createModel
    // fetch data model
    val model = tdb.profile.createModel()
    // customize code generator
    val codegen = new SourceCodeGenerator(model){
      // override mapped table and class name
      override def entityName = dbTableName => dbTableName.dropRight(1).toLowerCase.toCamelCase
      override def tableName  = dbTableName => dbTableName.toLowerCase.toCamelCase
    
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
          // use the data model member of this column to change the Scala type, e.g. to a custom enum or anything else
          override def rawType = if(model.name == "SOME_SPECIAL_COLUMN_NAME") "MyCustomType" else super.rawType
        }
      }
    }
    val driverName = tdb.driver.getClass.toString.dropRight(1).split("[\\. ]").last
    codegen.writeToFile("scala.slick.driver.H2Driver","target/slick-testkit-codegen-tests/","all.test",driverName+"Tables",driverName+".scala")
  }}
}
