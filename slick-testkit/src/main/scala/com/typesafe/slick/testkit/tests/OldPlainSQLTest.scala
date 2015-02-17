package com.typesafe.slick.testkit.tests

import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Assert
import org.junit.Assert._

import slick.jdbc.StaticQuery.interpolation
import slick.jdbc.{GetResult, StaticQuery => Q}

@deprecated("Using deprecated old Plain SQL API", "3.0")
class OldPlainSQLTest extends TestkitTest[JdbcTestDB] {

  implicit val getUserResult = GetResult(r => new User(r.<<, r.<<))

  case class User(id:Int, name:String)

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

  def testInterpolation = ifCap(tcap.plainSql) {
    def userForID(id: Int) = sql"select id, name from USERS where id = $id".as[User]
    def userForIdAndName(id: Int, name: String) = sql"select id, name from USERS where id = $id and name = $name".as[User]

    sqlu"create table USERS(ID int not null primary key, NAME varchar(255))".execute
    val total = (for {
      (id, name) <- List((1, "szeiger"), (0, "admin"), (2, "guest"), (3, "foo"))
    } yield sqlu"insert into USERS values ($id, $name)".first).sum
    assertEquals(4, total)

    assertEquals(Set(0,1,2,3), sql"select id from USERS".as[Int].buildColl[Set])

    val res = userForID(2).first
    println("User for ID 2: "+res)
    assertEquals(User(2,"guest"), res)

    val s1 = sql"select id from USERS where name = ${"szeiger"}".as[Int]
    val s2 = sql"select id from USERS where name = '#${"guest"}'".as[Int]
    assertEquals("select id from USERS where name = ?", s1.getStatement)
    assertEquals("select id from USERS where name = 'guest'", s2.getStatement)
    assertEquals(List(1), s1.list)
    assertEquals(List(2), s2.list)

    assertEquals(User(2,"guest"), userForIdAndName(2, "guest").first)
    assertEquals(None, userForIdAndName(2, "foo").firstOption)

    sqlu"""create table LONGTABLE( 
      VAL1  int not null, VAL2  int not null, VAL3  int not null, VAL4  int not null, 
      VAL5  int not null, VAL6  int not null, VAL7  int not null, VAL8  int not null, 
      VAL9  int not null, VAL10 int not null, VAL11 int not null, VAL12 int not null, 
      VAL13 int not null, VAL14 int not null, VAL15 int not null, VAL16 int not null, 
      VAL17 int not null, VAL18 int not null, VAL19 int not null, VAL20 int not null, 
      VAL21 int not null, VAL22 int not null, VAL23 int not null, VAL24 int not null)""".execute

    val s3 = sqlu"""insert into LONGTABLE values ( 
      #${100}, #${101}, #${102}, #${103}, #${104}, #${105}, #${106}, #${107},
      #${108}, #${109}, #${110}, #${111}, #${112}, #${113}, #${114}, #${115},
      #${116}, #${117}, #${118}, #${119}, #${120}, #${121}, #${122}, #${123}) """
    val s4 = sqlu"""insert into LONGTABLE values ( 
      #${200}, #${201}, #${202}, #${203}, #${204}, #${205}, #${206}, #${207},
      #${208}, #${209}, #${210}, #${211}, #${212}, #${213}, #${214}, #${215},
      #${216}, #${217}, #${218}, #${219}, #${220}, #${221}, #${222}, #${223}) """
    assertEquals("""insert into LONGTABLE values ( 
      100, 101, 102, 103, 104, 105, 106, 107,
      108, 109, 110, 111, 112, 113, 114, 115,
      116, 117, 118, 119, 120, 121, 122, 123) """, s3.getStatement)
    s3.execute
    assertEquals("""insert into LONGTABLE values ( 
      200, 201, 202, 203, 204, 205, 206, 207,
      208, 209, 210, 211, 212, 213, 214, 215,
      216, 217, 218, 219, 220, 221, 222, 223) """, s4.getStatement)
    s4.execute

    val s5 = sql"""select VAL24 from LONGTABLE WHERE VAL1=${100} AND VAL2=${101} AND VAL3=${102} 
      AND VAL4=${103} AND VAL5=${104} AND VAL6=${105} AND VAL7=${106} AND VAL8=${107} 
      AND VAL9=${108} AND VAL10=${109} AND VAL11=${110} AND VAL12=${111} AND VAL13=${112} 
      AND VAL14=${113} AND VAL15=${114} AND VAL16=${115} AND VAL17=${116} AND VAL18=${117} 
      AND VAL19=${118} AND VAL20=${119} AND VAL21=${120} AND VAL22=${121} AND VAL23=${122}""".as[Int]
    val s6 = sql"""select VAL24 from LONGTABLE WHERE VAL1=${200} AND VAL2=${201} AND VAL3=${202} 
      AND VAL4=${203} AND VAL5=${204} AND VAL6=${205} AND VAL7=${206} AND VAL8=${207} 
      AND VAL9=${208} AND VAL10=${209} AND VAL11=${210} AND VAL12=${211} AND VAL13=${212} 
      AND VAL14=${213} AND VAL15=${214} AND VAL16=${215} AND VAL17=${216} AND VAL18=${217} 
      AND VAL19=${218} AND VAL20=${219} AND VAL21=${220} AND VAL22=${221} AND VAL23=${222}""".as[Int]
    assertEquals(123, s5.first)
    assertEquals(223, s6.first)
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

    val q = "select 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23" +
      tdb.driver.scalarFrom.map(" from " + _).getOrElse("")

    val r1 = Q.queryNA[((Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int),
                        (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int))](q).first
    assertEquals(((1,2,3,4,5,6,7,8,9,10,11,12),(13,14,15,16,17,18,19,20,21,22,23)), r1)

    class Foo(val v1: Int, val v2: Int, val v3: Int, val v4: Int,
              val v5: Int, val v6: Int, val v7: Int, val v8: Int,
              val v9: Int, val v10: Int, val v11: Int, val v12: Int,
              val v13: Int, val v14: Int, val v15: Int, val v16: Int,
              val v17: Int, val v18: Int, val v19: Int, val v20: Int,
              val v21: Int, val v22: Int, val v23: Int) {
      override def toString =
        s"Foo($v1, $v2, $v3, $v4, $v5, $v6, $v7, $v8, $v9, $v10, $v11, $v12, "+
          s"$v13, $v14, $v15, $v16, $v17, $v18, $v19, $v20, $v21, $v22, $v23)"
      override def equals(o: Any) = o match {
        case f: Foo =>
          f.v1 == v1 && f.v2 == v2 && f.v3 == v3 && f.v4 == v4 &&
            f.v5 == v5 && f.v6 == v6 && f.v7 == v7 && f.v8 == v8 &&
            f.v9 == v9 && f.v10 == v10 && f.v11 == v11 && f.v12 == v12 &&
            f.v13 == v13 && f.v14 == v14 && f.v15 == v15 && f.v16 == v16 &&
            f.v17 == v17 && f.v18 == v18 && f.v19 == v19 && f.v20 == v20 &&
            f.v21 == v21 && f.v22 == v22 && f.v23 == v23
        case _ => false
      }
    }
    implicit val getFooResult = GetResult(r => new Foo(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
    val r2 = Q.queryNA[Foo](q).first
    assertEquals(new Foo(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), r2)
  }
}
