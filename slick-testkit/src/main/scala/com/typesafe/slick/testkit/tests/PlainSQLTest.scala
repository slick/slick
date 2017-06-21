package com.typesafe.slick.testkit.tests

import slick.jdbc.GetResult
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class PlainSQLTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  implicit val getUserResult = GetResult(r => new User(r.<<, r.<<))

  case class User(id:Int, name:String)

  //TODO convert to new API:
  /*
  def testSimple = ifCap(tcap.plainSql) {
    def getUsers(id: Option[Int]) = {
      val q = Q[User] + "select id, name from USERS "
      id map { q + "where id =" +? _ } getOrElse q
    }

    def InsertUser(id: Int, name: String) = Q.u + "insert into USERS values (" +? id + "," +? name + ")"

    val createTable = Q[Int] + "create table USERS(ID int not null primary key, NAME varchar(255))"
    val populateUsers = List(InsertUser(1, "szeiger"), InsertUser(0, "admin"), InsertUser(2, "guest"), InsertUser(3, "foo"))

    val allIDs = Q[Int] + "select id from USERS"
    val userForID = Q[Int, User] + "select id, name from USERS where id = ?"
    val userForIdAndName = Q[(Int, String), User] + "select id, name from USERS where id = ? and name = ?"

    implicitSession.withTransaction {
      println("Creating user table: "+createTable.first)
      println("Inserting users:")
      for(i <- populateUsers) println("  "+i.first)
    }

    println("All IDs:")
    for(s <- allIDs.list) println("  "+s)
    assertEquals(Set(1,0,2,3), allIDs.list.toSet)

    println("All IDs with foreach:")
    var s1 = Set[Int]()
    allIDs foreach { s =>
      println("  "+s)
      s1 += s
    }
    assertEquals(Set(1,0,2,3), s1)

    val res = userForID(2).first
    println("User for ID 2: "+res)
    assertEquals(User(2,"guest"), res)

    assertEquals(User(2,"guest"), userForIdAndName(2, "guest").first)
    assertEquals(None, userForIdAndName(2, "foo").firstOption)

    println("User 2 with foreach:")
    var s2 = Set[User]()
    userForID(2) foreach { s =>
      println("  "+s)
      s2 += s
    }
    assertEquals(Set(User(2,"guest")), s2)

    println("User 2 with foreach:")
    var s3 = Set[User]()
    getUsers(Some(2)) foreach { s =>
      println("  "+s)
      s3 += s
    }
    assertEquals(Set(User(2,"guest")), s3)

    println("All users with foreach:")
    var s4 = Set[User]()
    getUsers(None) foreach { s =>
      println("  "+s)
      s4 += s
    }
    assertEquals(Set(User(1,"szeiger"), User(2,"guest"), User(0,"admin"), User(3,"foo")), s4)

    println("All users with iterator.foreach:")
    var s5 = Set[User]()
    for(s <- getUsers(None).iterator) {
      println("  "+s)
      s5 += s
    }
    assertEquals(Set(User(1,"szeiger"), User(2,"guest"), User(0,"admin"), User(3,"foo")), s5)

    if(tdb.canGetLocalTables) {
      println("All tables:")
      for(t <- tdb.getLocalTables) println("  "+t)
      assertEquals(List("users"), tdb.getLocalTables.map(_.toLowerCase))
    }
    assertUnquotedTablesExist("USERS")
  }
  */

  def testInterpolation = ifCap(tcap.plainSql) {
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
      sql"select id from USERS".as[Int] shouldYield Set(0,1,2,3), //TODO Support `to` in Plain SQL Actions
      userForID(2).head shouldYield User(2,"guest"), //TODO Support `head` and `headOption` in Plain SQL Actions
      s1 shouldYield List(1),
      s2 shouldYield List(2),
      userForIdAndName(2, "guest").head shouldYield User(2,"guest"), //TODO Support `head` and `headOption` in Plain SQL Actions
      userForIdAndName(2, "foo").headOption shouldYield None //TODO Support `head` and `headOption` in Plain SQL Actions
    )
  }

  def testByteArrayParams = ifCap(tcap.plainSql){
    case class Data(id: Int, data: Array[Byte])
    implicit val getDataResult = GetResult(r => new Data(r.<<, r.<<))

    val create: DBIO[Int] = sqlu"create table DATA(ID int not null primary key, data TEXT NOT NULL)"
    def dataForID(id: Int) = sql"select id, data from DATA where id = $id".as[Data]
    def dataForIdAndData(id: Int, data: Array[Byte]) = sql"select id, data from DATA where id = $id and data = #$data".as[Data]

    val s1 = sql"select id from DATA where data = ${Array[Byte](1,2,3)}".as[Int]
    val s2 = sql"select id from DATA where data = '#${Array[Byte](4, 5, 6)}'".as[Int]

    val data = (Array[Byte](1,2,3), Array[Byte](4, 5, 6), Array[Byte](7, 8 , 9), Array[Byte](0, -1, -2))
    seq(
      create.map(_ shouldBe 0),
      DBIO.fold((for {
        (id, d) <- List((1, data._1), (0, data._2), (2, data._3), (3, data._4))
      } yield sqlu"insert into DATA values ($id, ${d.mkString})"), 0)(_ + _) shouldYield(4),
      dataForID(2).head shouldYield Data(2, Array[Byte](7, 8 , 9)),
      s1 shouldYield List(1),
      s2 shouldYield List(2),
      dataForIdAndData(2, data._3).head shouldYield Data(2, data._3),
      dataForIdAndData(2, data._3).headOption shouldYield None
    )
  }
}
