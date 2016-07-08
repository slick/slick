package com.typesafe.slick.testkit.tests

import org.junit.Assert
import org.junit.Assert._
import slick.jdbc.GetResult
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class PlainSQLTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  implicit val getUserResult = GetResult(r => new User(r.<<, r.<<))

  case class User(id:Int, name:String)

  def testSimpleQueries = ifCap(tcap.plainSql) {
    def userForID(id: Int) = sql"select id, name from USERS where id = $id".as[User]
    def userForIdAndName(id: Int, name: String) = sql"select id, name from USERS where id = $id and name = $name".as[User]

    val foo = "foo"
    val s1 = sql"select id from USERS where name = ${"szeiger"}".as[Int]
    val s2 = sql"select id from USERS where name = '#${"guest"}'".as[Int]
    val s3 = sql"select id from USERS where name = $foo".as[Int]
    val s4 = sql"select id from USERS where name = '#$foo'".as[Int]
    s1.statements.head shouldBe "select id from USERS where name = ?"
    s2.statements.head shouldBe "select id from USERS where name = 'guest'"
    s3.statements.head shouldBe "select id from USERS where name = ?"
    s4.statements.head shouldBe "select id from USERS where name = 'foo'"

    val create: DBIO[Int] = sqlu"create table USERS(ID int not null primary key, NAME varchar(255))"

    seq(
      create.map(_ shouldBe 0),
      DBIO.fold((for {
        (id, name) <- List((1, "szeiger"), (0, "admin"), (2, "guest"), (3, "foo"))
      } yield sqlu"insert into USERS values ($id, $name)"), 0)(_ + _) shouldYield(4),
      sql"select id from USERS".as[Int] shouldYield Set(0,1,2,3),
      userForID(2).head shouldYield User(2,"guest"),
      s1 shouldYield List(1),
      s2 shouldYield List(2),
      userForIdAndName(2, "guest").head shouldYield User(2,"guest"),
      userForIdAndName(2, "foo").headOption shouldYield None
    )
  }

  def testComposableQueries = ifCap(tcap.plainSql) {
    val createTable: DBIO[Int] = sqlu"create table USERS2(ID int not null primary key, NAME varchar(255))"

    def insertUser(id: Int, name: String) = sql"INSERT INTO USERS2 VALUES ($id, $name)".asUpdate

    val users = Set((1, "szeiger"), (0, "admin"), (2, "guest"), (3, "foo"), (4, "bar"))
    val insertAll: DBIO[Int] = DBIO.fold(users.map{case(i, s) => insertUser(i, s)}.toSeq, 0)(_ + _)


    def getAllIDs = sql"SELECT id FROM USERS2".as[Int]

    def getUsers(name: Option[String] = None) = {
      val q = sql"SELECT id, name FROM USERS2"
      (name map {n => q ++ sql"WHERE NAME LIKE ($n)" } getOrElse q).as[(Int, String)]
    }

    def findUser(name: Option[String] = None, id: Option[Long] = None) = {
      val nameQ = name.map{ n => sql"NAME = $n" }
      val idQ = id.map{ i => sql"ID = $i"}
      val query = sql"SELECT * FROM USERS2" ++
        (nameQ ++ idQ).reduceOption(_ ++ sql"AND" ++ _).map(sql"WHERE" ++ _).getOrElse(sql"")
      query.as[User].headOption
    }


    val q1 = getUsers()
    val q2 = getUsers(name = Some("ad%"))
    val q3 = getUsers(name = Some("%r"))
    val q4 = findUser(id = Some(2))
    val q5 = findUser(id = Some(3), name = Some("foo"))
    val q6 = findUser(id = Some(2), name = Some("foo"))

    q1.statements.head shouldBe "SELECT id, name FROM USERS2"
    q2.statements.head shouldBe "SELECT id, name FROM USERS2 WHERE NAME LIKE (?)"
    q3.statements.head shouldBe "SELECT id, name FROM USERS2 WHERE NAME LIKE (?)"
    q4.statements.head shouldBe "SELECT * FROM USERS2 WHERE ID = ?"
    q5.statements.head shouldBe "SELECT * FROM USERS2 WHERE NAME = ? AND ID = ?"
    q6.statements.head shouldBe "SELECT * FROM USERS2 WHERE NAME = ? AND ID = ?"


    seq(
      createTable,
      insertAll shouldYield 5,
      getAllIDs shouldYield Set(1, 0, 2, 3, 4),
      q1 shouldYield users,
      q2 shouldYield Set((0, "admin")),
      q3 shouldYield Set((1, "szeiger"), (4, "bar")),
      q4 shouldYield Some(User(2, "guest")),
      q5 shouldYield Some(User(3, "foo")),
      q6 shouldYield None
    )
  }
}
