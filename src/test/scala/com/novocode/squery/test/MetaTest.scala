package com.novocode.squery.test

import java.sql._
import scala.Array
import org.junit.Test
import org.junit.Assert._
import com.novocode.squery.ResultSetInvoker
import com.novocode.squery.combinator._
import com.novocode.squery.combinator.TypeMapper._
import com.novocode.squery.combinator.extended.H2Driver.Implicit._
import com.novocode.squery.combinator.extended.{ExtendedTable => Table}
import com.novocode.squery.meta._
import com.novocode.squery.session._
import com.novocode.squery.session.Database.threadLocalSession
import com.novocode.squery.simple.Implicit._

object MetaTest { def main(args: Array[String]) = new MetaTest().test() }

class MetaTest {

  object Users extends Table[(Int, String, Option[String])]("users") {
    def id = column[Int]("id", O AutoInc, O NotNull, O PrimaryKey)
    def first = column[String]("first", O Default "NFN", O DBType "varchar(64)")
    def last = column[Option[String]]("last")
    def * = id ~ first ~ last
  }

  object Orders extends Table[(Int, Int, String, Boolean, Option[Boolean])]("orders") {
    def userID = column[Int]("userID", O NotNull)
    def orderID = column[Int]("orderID", O AutoInc, O NotNull, O PrimaryKey)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped", O Default false, O NotNull)
    def rebate = column[Option[Boolean]]("rebate", O Default Some(false))
    def * = userID ~ orderID ~ product ~ shipped ~ rebate
    def userFK = foreignKey("user_fk", userID, Users)(_.id)
  }

  @Test def test() {

    Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver") withSession {

      val ddl = (Users.ddl ++ Orders.ddl)
      println("DDL used to create tables:")
      for(s <- ddl.createStatements) println("  "+s)
      ddl.create

      println("Tables from DatabaseMetaData:")
      for(t <- MTable.getTables) { 
        println("  "+t)
        for(c <- t.getColumns) println("    "+c)
        for(k <- t.getPrimaryKeys) println("    "+k)
        for(k <- t.getImportedKeys) println("    Imported "+k)
        for(k <- t.getExportedKeys) println("    Exported "+k)
      }
    } 
  }
}
