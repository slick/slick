package slick.test.lifted

import org.junit.Test
import org.junit.Assert._

/** Test case for the SQL schema support in table definitions */
class SchemaSupportTest {

  @Test def testSchemaSupport: Unit = {
    import slick.jdbc.H2Profile.api._

    class T(tag: Tag) extends Table[Int](tag, Some("myschema"), "mytable") {
      def id = column[Int]("id")
      def * = id
    }
    val ts = TableQuery[T]

    val s1 = ts.filter(_.id < 5).result.statements.head
    println(s1)
    assertTrue("select ... from uses schema name", s1 contains """from "myschema"."mytable"""")

    //val s2 = ts.insertStatement
    //println(s2)

    val s3 = ts.filter(_.id < 5).updateStatement
    println(s3)
    assertTrue("update uses schema name", s3 contains """update "myschema"."mytable"""")

    val s4 = ts.filter(_.id < 5).delete.statements.head
    println(s4)
    assertTrue("delete uses schema name", s4 contains """delete from "myschema"."mytable"""")

    val s5 = ts.schema.createStatements.toList
    s5.foreach(println)
    s5.foreach(s => assertTrue("DDL (create) uses schema name", s contains """ "myschema"."mytable""""))

    val s6 = ts.schema.dropStatements.toList
    s6.foreach(println)
    s6.foreach(s => assertTrue("DDL (drop) uses schema name", s contains """ "myschema"."mytable""""))

    val s7 = ts.schema.dropIfExistsStatements.toList
    s7.foreach(println)
    s7.foreach(s => assertTrue("DDL (dropIfExists) uses schema name", s contains """ "myschema"."mytable""""))

    val s8 = ts.schema.createIfNotExistsStatements.toList
    s6.foreach(println)
    s6.foreach(s => assertTrue("DDL (createIfNotExists) uses schema name", s contains """ "myschema"."mytable""""))
  }


  @Test def createIfNotExist: Unit = {

    import slick.jdbc.PostgresProfile.api._

    case class User1(index: Option[Int],
                     auto_inc: Int,
                     id: String,
                     password: String)

    class Users1(tag: Tag) extends Table[User1](tag, "users1") {
      def index      = column[Int]("idx", O.PrimaryKey, O.AutoInc)
      def auto_inc      = column[Int]("auto_inc", O.AutoInc)
      def id         = column[String]("id", O.Unique)
      def password   = column[String]("pw")
      def * =
        (index.?, auto_inc, id, password) <> ((User1.apply _).tupled, User1.unapply)
    }

    val users1 = TableQuery[Users1]

    def create = users1.schema.createStatements.toSeq.head
    def createIfNotExists = users1.schema.createIfNotExistsStatements.toSeq.head
    assertFalse(create.contains("if not exists"))
    assertTrue(createIfNotExists.contains("if not exists"))

    val forCompare = createIfNotExists.replace("if not exists ", "")
    assertEquals(create, forCompare)
  }

}
