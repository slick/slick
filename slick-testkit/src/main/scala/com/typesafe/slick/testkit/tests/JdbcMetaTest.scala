package com.typesafe.slick.testkit.tests

import org.junit.{Test, Assert}
import org.junit.Assert._
import scala.slick.jdbc.meta._
import scala.slick.jdbc.StaticQuery
import com.typesafe.slick.testkit.util.{TestDB, JdbcTestDB, TestkitTest}

@deprecated("Using deprecated .simple API", "3.0")
class JdbcMetaTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users_xx") {
    def id = column[Int]("id", O.PrimaryKey)
    def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = (id, first, last)
  }
  lazy val users = TableQuery[Users]

  class Orders(tag: Tag) extends Table[(Int, Int, String, Boolean, Option[Boolean])](tag, "orders_xx") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O.PrimaryKey)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped", O Default false)
    def rebate = column[Option[Boolean]]("rebate", O Default Some(false))
    def * = (userID, orderID, product, shipped, rebate)
    def userFK = foreignKey("user_fk", userID, users)(_.id)
  }
  lazy val orders = TableQuery[Orders]

  def testMeta = ifCap(tcap.jdbcMeta) {
    val ddl = (users.schema ++ orders.schema)
    println("DDL used to create tables:")
    for(s <- ddl.createStatements) println("  "+s)
    ddl.create

    println("Type info from DatabaseMetaData:")
    for(t <- MTypeInfo.getTypeInfo) println("  "+t)

    ifCap(tcap.jdbcMetaGetFunctions) {
      /* Not supported by PostgreSQL and H2. */
      println("Functions from DatabaseMetaData:")
      for(f <- MFunction.getFunctions(MQName.local("%"))) {
        println("  "+f)
        for(c <- f.getFunctionColumns()) println("    "+c)
      }
    }

    println("UDTs from DatabaseMetaData:")
    for(u <- MUDT.getUDTs(MQName.local("%"))) println("  "+u)

    println("Procedures from DatabaseMetaData:")
    MProcedure.getProcedures(MQName.local("%")).foreach({ p =>
      println("  "+p)
      for(c <- p.getProcedureColumns()) println("    "+c)
    }, 3)

    println("Tables from DatabaseMetaData:")
    for(t <- MTable.getTables(None, None, None, None).list if Set("users", "orders") contains t.name.name) {
      println("  "+t)
      for(c <- t.getColumns) {
        println("    "+c)
        for(p <- c.getColumnPrivileges) println("      "+p)
      }
      for(v <- t.getVersionColumns) println("    "+v)
      for(k <- t.getPrimaryKeys) println("    "+k)
      for(k <- t.getImportedKeys) println("    Imported "+k)
      for(k <- t.getExportedKeys) println("    Exported "+k)
      ifCap(tcap.jdbcMetaGetIndexInfo) {
        for(i <- t.getIndexInfo()) println("    "+i)
      }
      for(p <- t.getTablePrivileges) println("    "+p)
      for(c <- t.getBestRowIdentifier(MBestRowIdentifierColumn.Scope.Session))
        println("    Row identifier for session: "+c)
    }

    println("Schemas from DatabaseMetaData:")
    for(t <- MSchema.getSchemas) println("  "+t)

    ifCap(tcap.jdbcMetaGetClientInfoProperties) {
      println("Client Info Properties from DatabaseMetaData:")
      for(t <- MClientInfoProperty.getClientInfoProperties) println("  "+t)
    }

    assertTrue("Tables before deleting",
      Set("orders_xx", "users_xx") subsetOf MTable.getTables(None, None, None, None).list.map(_.name.name).toSet)

    if(tdb.canGetLocalTables) {
      for (t <- tdb.getLocalTables.sorted) {
        val st = "drop table " + tdb.driver.quoteIdentifier(t)
        println("Executing statement: " + st)
        StaticQuery.updateNA(st).execute
      }
      val newTables = MTable.getTables(None, None, None, None).list.map(_.name.name).toSet
      println(newTables)
      assertTrue("Tables after deleting",
        (Set("orders_xx", "users_xx") intersect newTables).isEmpty)
    }
  }
}
