package com.typesafe.slick.testkit.tests

import slick.jdbc.{DerbyProfile, H2Profile, HsqldbProfile, SimpleJdbcAction}
import slick.jdbc.meta._
import com.typesafe.slick.testkit.util.{JdbcTestDB, AsyncTest}

class JdbcMetaTest extends AsyncTest[JdbcTestDB] {
  import tdb.profile.api._

  class Users(tag: Tag) extends Table[(Int, String, Option[String])](tag, "users_xx") {
    def id = column[Int]("id", O.PrimaryKey)
    def first = column[String]("first", O.Default("NFN"), O.SqlType("varchar(64)"))
    def last = column[Option[String]]("last")
    def * = (id, first, last)
  }
  lazy val users = TableQuery[Users]

  class Orders(tag: Tag) extends Table[(Int, Int, String, Boolean, Option[Boolean])](tag, "orders_xx") {
    def userID = column[Int]("userID")
    def orderID = column[Int]("orderID", O.PrimaryKey)
    def product = column[String]("product")
    def shipped = column[Boolean]("shipped", O.Default(false))
    def rebate = column[Option[Boolean]]("rebate", O.Default(Some(false)))
    def * = (userID, orderID, product, shipped, rebate)
    def userFK = foreignKey("user_fk", userID, users)(_.id)
  }
  lazy val orders = TableQuery[Orders]

  /** All objects without a catalog and schema, or without a schema in the current catalog where an empty catalog is
    * not accepted */
  def anyLocal =
    if (tdb.capabilities.contains(tcap.jdbcMetaEmptyCatalog)) MQName.local("%") else MQName(None, Some(""), "%")

  def testMeta = ifCap(tcap.jdbcMeta)(DBIO.seq(
    (users.schema ++ orders.schema).create.named("DDL used to create tables"),

    MTypeInfo.getTypeInfo.named("Type info from DatabaseMetaData"),

    ifCap(tcap.jdbcMetaGetFunctions) {
      /* Not supported by PostgreSQL, SQLite and H2. */
      MFunction.getFunctions(anyLocal).flatMap { fs =>
        DBIO.sequence(fs.map(_.getFunctionColumns()))
      }
    }.named("Functions from DatabaseMetaData"),

    MUDT.getUDTs(anyLocal).named("UDTs from DatabaseMetaData"),

    MProcedure.getProcedures(anyLocal).flatMap { ps =>
      DBIO.sequence(ps.map(_.getProcedureColumns()))
    }.named("Procedures from DatabaseMetaData"),

    tdb.profile.defaultTables.flatMap { ts =>
      DBIO.sequence(ts.filter(t => Set("users_xx", "orders_xx") contains t.name.name).map { t =>
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

    tdb.profile.defaultTables.map(_ should (ts =>
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

  class Lookups(tag: Tag) extends Table[Int](tag, "lookup_xx") {
    def id = column[Int]("id", O.PrimaryKey)
    def * = id
  }
  lazy val lookups = TableQuery[Lookups]

  def testLookupsByUnqualifiedName = for {
    _ <- lookups.schema.create
    _ <- MPrimaryKey.getPrimaryKeys("lookup_xx").map(_.map(_.column) shouldBe Vector("id"))
    _ <- MTable.getTables("lookup_xx").map(_.map(_.name.name) shouldBe Vector("lookup_xx"))
    _ <- MTable.getTables.map(_.map(_.name.name) should (_ contains "lookup_xx"))
  } yield ()

  class SchemaWt(tag: Tag, schema: String) extends Table[Int](tag, Some(schema), "wt") {
    def id = column[Int]("id", O.PrimaryKey)
    def * = id
  }

  /** The current schema's name is not a search pattern: `_` in it does not match another character */
  def testLookupsInCurrentSchemaWithWildcard = tdb.profile match {
    case _: H2Profile | _: HsqldbProfile | _: DerbyProfile => // CREATE SCHEMA and SET SCHEMA
      val schemas = Seq("MY_S", "MYXS")
      val tables = schemas.map(s => TableQuery(new SchemaWt(_, s)))
      SimpleJdbcAction(_.session.conn.getSchema).flatMap { defaultSchema =>
        (for {
          _ <- DBIO.seq(schemas.map(s => sqlu"create schema #$s")*)
          _ <- DBIO.seq(tables.map(_.schema.create)*)
          _ <- sqlu"set schema MY_S"
          _ <- MTable.getTables("wt").map(_.map(_.name.schema) shouldBe Vector(Some("MY_S")))
          _ <- MTable.getTables("wt").headOption.map(_.map(_.name.schema) shouldBe Some(Some("MY_S")))
          _ <- MTable.getTables.map(_.filter(_.name.name == "wt").map(_.name.schema) shouldBe Vector(Some("MY_S")))
          _ <- MPrimaryKey.getPrimaryKeys("wt").map(_.map(_.table.schema) shouldBe Vector(Some("MY_S")))
        } yield ()).andFinally(DBIO.seq(
          sqlu"""set schema "#$defaultSchema"""",
          DBIO.seq(tables.map(_.schema.dropIfExists)*),
          DBIO.seq(schemas.map(s => sqlu"drop schema #$s restrict")*)
        ))
      }.withPinnedSession
    case _ => DBIO.successful(())
  }
}
