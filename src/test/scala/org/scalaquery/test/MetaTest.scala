package org.scalaquery.test

import java.sql._
import java.io.PrintWriter
import scala.Array
import org.junit.Test
import org.junit.Assert._
import org.scalaquery.ResultSetInvoker
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.ql.extended.{H2Driver, PostgresDriver, MySQLDriver}
import org.scalaquery.meta._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.simple.StaticQuery.updateNA
import org.scalaquery.test.util._
import org.scalaquery.test.util.TestDB._

object MetaTest extends DBTestObject(H2Mem, Postgres, MySQL, DerbyMem, HsqldbMem)

class MetaTest(tdb: TestDB) extends DBTest(tdb) {
  import tdb.driver.Implicit._

  object Users extends Table[(Int, String, Option[String])]("users") {
    def id = column[Int]("id", O PrimaryKey)
    def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = id ~ first ~ last
  }

  object Orders extends Table[(Int, Int, String, Boolean, Option[Boolean])]("orders") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O PrimaryKey)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped", O Default false)
    def rebate = column[Option[Boolean]]("rebate", O Default Some(false))
    def * = userID ~ orderID ~ product ~ shipped ~ rebate
    def userFK = foreignKey("user_fk", userID, Users)(_.id)
  }

  @Test def test() {

    db withSession {

      val ddl = (Users.ddl ++ Orders.ddl)
      println("DDL used to create tables:")
      for(s <- ddl.createStatements) println("  "+s)
      ddl.create

      println("Type info from DatabaseMetaData:")
      for(t <- MTypeInfo.getTypeInfo) println("  "+t)

      if(tdb.driver != PostgresDriver) {
        /* Not supported by PostgreSQL and H2 but calling it on H2 is safe
         * because it throws an AbstractMethodError which is handled
         * automatically by ScalaQuery and turned into an empty result set. */
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
        for(i <- t.getIndexInfo()) println("    "+i)
        for(p <- t.getTablePrivileges) println("    "+p)
        for(c <- t.getBestRowIdentifier(MBestRowIdentifierColumn.Scope.Session))
          println("    Row identifier for session: "+c)
      }

      println("Schemas from DatabaseMetaData:")
      for(t <- MSchema.getSchemas) println("  "+t)

      if(tdb.driver != H2Driver) { // Not supported by H2
        println("Client Info Properties from DatabaseMetaData:")
        for(t <- MClientInfoProperty.getClientInfoProperties) println("  "+t)
      }

      println("Generated code:")
      val out = new PrintWriter(System.out)
      for(t <- MTable.getTables(None, None, None, None).list if Set("users", "orders") contains t.name.name)
        CodeGen.output(t, out)
      out.flush

      assertTrue("Tables before deleting",
        Set("orders", "users") subsetOf MTable.getTables(None, None, None, None).list.map(_.name.name).toSet)
      for(t <- tdb.getLocalTables.sorted) {
        val st = "drop table " + tdb.driver.sqlUtils.quoteIdentifier(t)
        println("Executing statement: "+st)
        updateNA(st).execute
      }
      assertTrue("Tables after deleting",
        Set("orders", "users") intersect MTable.getTables(None, None, None, None).list.map(_.name.name).toSet isEmpty)
    }
  }
}
