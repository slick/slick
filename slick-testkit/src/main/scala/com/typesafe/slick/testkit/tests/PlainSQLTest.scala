package com.typesafe.slick.testkit.tests

import org.junit.Assert
import org.junit.Assert._
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import Q.interpolation
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class PlainSQLTest(val tdb: TestDB) extends TestkitTest {

  implicit val getUserResult = GetResult(r => new User(r.<<, r.<<))

  case class User(id:Int, name:String)

  def testSimple = ifCap(TestDB.plainSql) {
    def getUsers(id: Option[Int]) = {
      val q = Q[User] + "select id, name from users "
      id map { q + "where id =" +? _ } getOrElse q
    }

    def InsertUser(id: Int, name: String) = Q.u + "insert into USERS values (" +? id + "," +? name + ")"

    val createTable = Q[Int] + "create table USERS(ID int not null primary key, NAME varchar(255))"
    val populateUsers = List(InsertUser(1, "szeiger"), InsertUser(0, "admin"), InsertUser(2, "guest"), InsertUser(3, "foo"))

    val allIDs = Q[Int] + "select id from users"
    val userForID = Q[Int, User] + "select id, name from users where id = ?"
    val userForIdAndName = Q[(Int, String), User] + "select id, name from users where id = ? and name = ?"

    sharedSession.withTransaction {
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

    val res = userForID.first(2)
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

    println("All users with elements.foreach:")
    var s5 = Set[User]()
    for(s <- getUsers(None).elements) {
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

  def testInterpolation = ifCap(TestDB.plainSql) {
    def userForID(id: Int) = sql"select id, name from users where id = $id".as[User]
    def userForIdAndName(id: Int, name: String) = sql"select id, name from users where id = $id and name = $name".as[User]

    sqlu"create table USERS(ID int not null primary key, NAME varchar(255))".execute
    val total = (for {
      (id, name) <- List((1, "szeiger"), (0, "admin"), (2, "guest"), (3, "foo"))
    } yield sqlu"insert into USERS values ($id, $name)".first).sum
    assertEquals(4, total)

    assertEquals(Set(0,1,2,3), sql"select id from users".as[Int].to[Set])

    val res = userForID(2).first
    println("User for ID 2: "+res)
    assertEquals(User(2,"guest"), res)

    val s1 = sql"select id from users where name = ${"szeiger"}".as[Int]
    val s2 = sql"select id from users where name = '#${"guest"}'".as[Int]
    assertEquals("select id from users where name = ?", s1.getStatement)
    assertEquals("select id from users where name = 'guest'", s2.getStatement)
    assertEquals(List(1), s1.list)
    assertEquals(List(2), s2.list)

    assertEquals(User(2,"guest"), userForIdAndName(2, "guest").first)
    assertEquals(None, userForIdAndName(2, "foo").firstOption)
  }

  def assertUnquotedTablesExist(tables: String*) {
    for(t <- tables) {
      try ((Q[Int]+"select 1 from "+t+" where 1 < 0").list) catch { case _: Exception =>
        Assert.fail("Table "+t+" should exist")
      }
    }
  }
  def assertNotUnquotedTablesExist(tables: String*) {
    for(t <- tables) {
      try {
        (Q[Int]+"select 1 from "+t+" where 1 < 0").list
        Assert.fail("Table "+t+" should not exist")
      } catch { case _: Exception => }
    }
  }
}
