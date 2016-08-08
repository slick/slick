package com.typesafe.slick.testkit.tests

import org.junit.{Test, Assert}
import org.junit.Assert._
import slick.jdbc.meta._
import com.typesafe.slick.testkit.util.{TestDB, JdbcTestDB, AsyncTest}

class JdbcMetaTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users_xx") {
    def id = column[Int]("id", O.PrimaryKey)
    def first = column[String]("first", O Default "NFN", O SqlType "varchar(64)")
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

  def testMeta = ifCap(tcap.jdbcMeta)(DBIO.seq(
    (users.schema ++ orders.schema).create.named("DDL used to create tables"),

    MTypeInfo.getTypeInfo.named("Type info from DatabaseMetaData"),

    ifCap(tcap.jdbcMetaGetFunctions) {
      /* Not supported by PostgreSQL and H2. */
      MFunction.getFunctions(MQName.local("%")).flatMap { fs =>
        DBIO.sequence(fs.map(_.getFunctionColumns()))
      }
    }.named("Functions from DatabaseMetaData"),

    MUDT.getUDTs(MQName.local("%")).named("UDTs from DatabaseMetaData"),

    MProcedure.getProcedures(MQName.local("%")).flatMap { ps =>
      DBIO.sequence(ps.map(_.getProcedureColumns()))
    }.named("Procedures from DatabaseMetaData"),

    tdb.profile.defaultTables.flatMap { ts =>
      DBIO.sequence(ts.filter(t => Set("users", "orders") contains t.name.name).map { t =>
        DBIO.seq(
          t.getColumns.flatMap { cs =>
            val as = cs.map(_.getColumnPrivileges)
            DBIO.sequence(as)
          },
          t.getVersionColumns,
          t.getPrimaryKeys,
          t.getImportedKeys,
          t.getExportedKeys,
          ifCap(tcap.jdbcMetaGetIndexInfo)(t.getIndexInfo()),
          t.getTablePrivileges,
          t.getBestRowIdentifier(MBestRowIdentifierColumn.Scope.Session)
        )
      })
    }.named("Tables from DatabaseMetaData"),

    MSchema.getSchemas.named("Schemas from DatabaseMetaData"),

    ifCap(tcap.jdbcMetaGetClientInfoProperties)(MClientInfoProperty.getClientInfoProperties)
      .named("Client Info Properties from DatabaseMetaData"),

    tdb.profile.defaultTables.map(_.should(ts =>
      Set("orders_xx", "users_xx") subsetOf ts.map(_.name.name).toSet
    )).named("Tables before deleting")

    /* ,
    if(tdb.canGetLocalTables) {

      for (t <- tdb.getLocalTables.sorted) {
        val st = "drop table " + tdb.profile.quoteIdentifier(t)
        println("Executing statement: " + st)
        StaticQuery.updateNA(st).execute
      }
      val newTables = MTable.getTables(None, None, None, None).list.map(_.name.name).toSet
      println(newTables)
      assertTrue("Tables after deleting",
        (Set("orders_xx", "users_xx") intersect newTables).isEmpty)

    } else Action.successful(())
    */
  ))
}
