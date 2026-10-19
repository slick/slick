package com.typesafe.slick.testkit.tests

import slick.jdbc.{GetResult, SetParameter}
import slick.jdbc.PositionedResult.SqlNullException

import com.typesafe.slick.testkit.util.{AsyncTest, JdbcTestDB}

class PlainSQLTest extends AsyncTest[JdbcTestDB] {

  import tdb.profile.api.*


  implicit val getUserResult: GetResult[User] = GetResult(r => new User(r.<<, r.<<))

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
      userForIdAndName(2, "foo").headOption shouldYield None, //TODO Support `head` and `headOption` in Plain SQL Actions
      // #878: an UPDATE without a WHERE clause must report the real number of affected
      // rows (4 here), not always 1. Run last so it doesn't disturb the reads above.
      sqlu"update USERS set NAME = 'Unicorn'".map(_ shouldBe 4)
    )
  }

  def testEdgeTypes = ifCap(tcap.plainSql) {
    object Const
    case class Weird(u: Unit, x: (Int, String), c: Const.type)

    implicit val getC = GetResult.const[Const.type](Const)
    implicit val get: GetResult[Weird] = GetResult[Weird](r => new Weird(r.<<[Unit], r.<<, r.<<[Const.type]))

    implicit val setC = SetParameter.const[Const.type]
    implicit val setU = SetParameter[User]{(u, p) => println(u); p.setInt(u.id); p.setString(u.name)}
    implicit val setT = setU.contramap((User.apply _).tupled)
    implicit val set  = SetParameter[Weird]{(w, p) =>
      SetParameter.SetUnit(w.u, p)
      setT(w.x, p)
      setC(w.c, p)
    }

    val w = Weird((), (3, "Ah"), Const)

    val create =
      sqlu"create table USERS2(ID int not null primary key, NAME varchar(255))".map(_ shouldBe 0)

    def testSet =
      sqlu"insert into USERS2 values (${w.x._1}, ${w.x._2})".map(_ shouldBe 1)
      //sqlu"insert into USERS2 values ($w)".map(_ shouldBe 1)

    def testGet =
      sql"select id, name from USERS2 where id = ${w.x._1}".as[Weird].map(_.head shouldBe w)

    seq(create, testSet, testGet)
  }

  def testGetNonNull = ifCap(tcap.plainSql) {
    implicit val getResultNonNull: GetResult[Int] = GetResult[Int](_.nextIntNonNull())

    val create = sqlu"create table NUMBERS(NUM int not null)".map(_ shouldBe 0)

    val selectMaxFromEmptyFails =
      sql"select MAX(NUM) from NUMBERS".as[Int].failed.map(_ shouldBe SqlNullException(classOf[Int]))

    seq(create, selectMaxFromEmptyFails)
  }

  def testAsSeqOfAnyAndAsMapOfAny = ifCap(tcap.plainSql) {
    // getObject returns driver-specific types (e.g. java.math.BigDecimal for an int on Oracle) and
    // unquoted labels come back upper-case or lower-case depending on the database, so normalize
    def norm(v: Any): Any = v match {
      case n: java.lang.Number => n.longValue
      case v => v
    }
    def normRow(m: Map[String, Any]): List[(String, Any)] =
      m.toList.map { case (k, v) => (k.toUpperCase, norm(v)) }

    val create =
      sqlu"create table ANYROWS(ID int not null primary key, NAME varchar(255), NICK varchar(255))".map(_ shouldBe 0)
    val populate = DBIO.seq(
      sqlu"insert into ANYROWS values (1, 'szeiger', NULL)",
      sqlu"insert into ANYROWS values (2, 'guest', 'g')"
    )

    // Values in column order, SQL NULL as null
    val seqs = sql"select NICK, ID, NAME from ANYROWS order by ID".asSeqOfAny.map { rows =>
      rows.map(_.map(norm)) shouldBe Vector(Vector(null, 1L, "szeiger"), Vector("g", 2L, "guest"))
    }

    // Keys are column labels (respecting aliases), in column order, SQL NULL as null
    val maps = sql"select NAME as USER_NAME, ID as USER_ID, NICK from ANYROWS order by ID".asMapOfAny.map { rows =>
      rows.map(normRow) shouldBe Vector(
        List("USER_NAME" -> "szeiger", "USER_ID" -> 1L, "NICK" -> null),
        List("USER_NAME" -> "guest", "USER_ID" -> 2L, "NICK" -> "g")
      )
    }

    // Duplicate labels: the Seq has all columns, the Map only the first column with that label
    val dupSeq = sql"select ID as X, NAME as X from ANYROWS where ID = 1".asSeqOfAny.map { rows =>
      rows.map(_.map(norm)) shouldBe Vector(Vector(1L, "szeiger"))
    }
    val dupMap = sql"select ID as X, NAME as X from ANYROWS where ID = 1".asMapOfAny.map { rows =>
      rows.map(normRow) shouldBe Vector(List("X" -> 1L))
    }

    seq(create, populate, seqs, maps, dupSeq, dupMap)
  }
}
