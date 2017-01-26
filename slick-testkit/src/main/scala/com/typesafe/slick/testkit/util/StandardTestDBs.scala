package com.typesafe.slick.testkit.util

import java.io.File
import java.util.logging.{Level, Logger}
import java.sql.SQLException
import slick.compiler.Phase
import slick.dbio._
import slick.driver.JdbcProfile
import slick.memory.MemoryProfile
import slick.jdbc._
import slick.jdbc.GetResult._
import slick.jdbc.meta.MTable
import org.junit.Assert
import slick.util.ConfigExtensionMethods._


import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object StandardTestDBs {
  lazy val H2Mem = new H2TestDB("h2mem", false) {
    val url = "jdbc:h2:mem:test1"
    override def isPersistent = false
  }

  /** A modified H2Mem that tests the `removeTakeDrop` phase (which is not used by any of the
    * standard profiles). */
  lazy val H2Rownum = new H2TestDB("h2rownum", false) {
    val url = "jdbc:h2:mem:test_rownum"
    override def isPersistent = false
    override val profile = new H2Profile {
      override protected def computeQueryCompiler = super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
    }
  }

  lazy val H2MemKeepAlive = new H2TestDB("h2mem", true) {
    val url = "jdbc:h2:mem:test1"
  }

  lazy val H2Disk = new H2TestDB("h2disk", false) {
    val dbName = "h2-"+confName
    val url = "jdbc:h2:"+TestkitConfig.testDBPath+"/"+dbName
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val HsqldbMem = new HsqlDB("hsqldbmem") {
    val dbName = "test1"
    val url = "jdbc:hsqldb:mem:"+dbName+";user=SA;password=;shutdown=true"
    override def isPersistent = false
  }

  lazy val HsqldbDisk = new HsqlDB("hsqldbdisk") {
    val dbName = "hsqldb-"+confName
    val url = "jdbc:hsqldb:file:"+TestkitConfig.testDBPath+"/"+dbName+";user=SA;password=;shutdown=true;hsqldb.applog=0"
    override def cleanUpBefore() = TestDB.deleteDBFiles(dbName)
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val SQLiteMem = new SQLiteTestDB("jdbc:sqlite:file:slick_test?mode=memory&cache=shared", "sqlitemem") {
    override def isPersistent = false
  }

  lazy val SQLiteDisk = {
    val confName = "sqlitedisk"
    val prefix = "sqlite-"+confName
    new SQLiteTestDB("jdbc:sqlite:"+TestkitConfig.testDBPath+"/"+prefix+".db", confName) {
      override def cleanUpBefore() = TestDB.deleteDBFiles(prefix)
    }
  }

  lazy val DerbyMem = new DerbyDB("derbymem") {
    val dbName = "test1"
    val url = "jdbc:derby:memory:"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:memory:"+dbName+";drop=true"
      try { await(profile.backend.Database.forURL(dropUrl, driver = jdbcDriver).run(SimpleJdbcAction(_.connection))) }
      catch { case e: SQLException => }
    }
  }

  lazy val DerbyDisk = new DerbyDB("derbydisk") {
    val dbName = "derby-"+confName
    val url = "jdbc:derby:"+TestkitConfig.testDBPath+"/"+dbName+";create=true"
    override def cleanUpBefore() = {
      val dropUrl = "jdbc:derby:"+TestkitConfig.testDBPath+"/"+dbName+";shutdown=true"
      try { await(profile.backend.Database.forURL(dropUrl, driver = jdbcDriver).run(SimpleJdbcAction(_.connection))) }
      catch { case e: SQLException => }
      TestDB.deleteDBFiles(dbName)
    }
  }

  lazy val Postgres = new ExternalJdbcTestDB("postgres") {
    val profile = PostgresProfile
    override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
      ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
        ts.filter(_._4.toUpperCase == "TABLE").map(_._3).sorted
      }
    override def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] =
      ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables("", "public", null, null)).map { ts =>
        ts.filter(_._4.toUpperCase == "SEQUENCE").map(_._3).sorted
      }
    override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions
  }

  lazy val MySQL = new ExternalJdbcTestDB("mysql") {
    val profile = MySQLProfile
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val Heap = new RelationalTestDB {
    type Profile = MemoryProfile
    val profile: Profile = MemoryProfile
    val confName: String = "heap"
    def createDB: profile.Backend#Database = profile.backend.Database(ExecutionContext.global)
    def dropUserArtifacts(implicit session: profile.Backend#Session) {
      val db = session.database
      db.getTables.foreach(t => db.dropTable(t.name))
    }
    def assertTablesExist(tables: String*) = profile.api.SimpleDBIO { ctx =>
      val all = ctx.session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(!all.contains(t)) Assert.fail("Table "+t+" should exist")
      }
    }
    def assertNotTablesExist(tables: String*) = profile.api.SimpleDBIO { ctx =>
      val all = ctx.session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(all.contains(t)) Assert.fail("Table "+t+" should not exist")
      }
    }
  }

  lazy val DB2 = new ExternalJdbcTestDB("db2") {
    val profile = DB2Profile
    import profile.api.actionBasedSQLInterpolation

    override def canGetLocalTables = false

    lazy val schema = config.getString("schema")

    def dropSchema: DBIO[Unit] = {
      import ExecutionContext.Implicits.global
      for {
        schema <- sql"select schemaname from syscat.schemata where schemaname = '#$schema'".as[String].headOption
        _ <- if(schema.isDefined) {
          println(s"[Dropping DB2 schema '$schema']")
          sqlu"call sysproc.admin_drop_schema($schema, null, ${"ERRORSCHEMA"}, ${"ERRORTABLE"})"
        } else DBIO.successful(())
      } yield ()
    }

    override def cleanUpBefore(): Unit = {
      import ExecutionContext.Implicits.global
      await(databaseFor("testConn").run(for {
        _ <- dropSchema
        _ = println(s"[Creating DB2 schema '$schema']")
        _ <- sqlu"create schema #$schema"
      } yield ()))
    }

    override def cleanUpAfter(): Unit =
      await(databaseFor("adminConn").run(dropSchema))

    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  class SQLServerDB(confName: String) extends ExternalJdbcTestDB(confName) {
    val profile = SQLServerProfile
    import profile.api.actionBasedSQLInterpolation

    // sqlserver has valid "select for update" syntax, but in testing on Appveyor, the test hangs due to lock escalation
    // so exclude from explicit ForUpdate testing
    override def capabilities = super.capabilities - TestDB.capabilities.selectForUpdateRowLocking
    val defaultSchema = config.getStringOpt("defaultSchema")

    override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
      MTable.getTables(None, defaultSchema, None, Some(Seq("TABLE"))).map(_.map(_.name.name).sorted)
    }

    override def dropUserArtifacts(implicit session: profile.Backend#Session) = blockingRunOnSession { implicit ec =>
      for {
        tables <- localTables
        _ <- DBIO.sequence(tables.map(t => sqlu"exec sp_MSdropconstraints #$t"))
        tableStatements = tables.map(t => sqlu"drop table #${profile.quoteIdentifier(t)}")
        _ <- DBIO.sequence(tableStatements)
      } yield ()
    }
  }

  lazy val SQLServerJTDS = new SQLServerDB("sqlserver-jtds") {
    override def capabilities = super.capabilities - TestDB.capabilities.plainSql
  }
  lazy val SQLServer2012JTDS = new SQLServerDB("sqlserver2012-jtds") {
    override def capabilities = super.capabilities - TestDB.capabilities.plainSql
  }
  lazy val SQLServer2014JTDS = new SQLServerDB("sqlserver2014-jtds") {
    override def capabilities = super.capabilities - TestDB.capabilities.plainSql
  }
  lazy val SQLServerSQLJDBC = new SQLServerDB("sqlserver-sqljdbc") {
    override def capabilities = profile.capabilities - JdbcCapabilities.createModel
  }
  lazy val SQLServer2012SQLJDBC = new SQLServerDB("sqlserver2012-sqljdbc") {
    override def capabilities = profile.capabilities - JdbcCapabilities.createModel
  }
  lazy val SQLServer2014SQLJDBC = new SQLServerDB("sqlserver2014-sqljdbc") {
    override def capabilities = profile.capabilities - JdbcCapabilities.createModel
  }

  lazy val Oracle = new ExternalJdbcTestDB("oracle") {
    val profile = OracleProfile
    import profile.api.actionBasedSQLInterpolation

    override def canGetLocalTables = false
    override def capabilities =
      super.capabilities - TestDB.capabilities.jdbcMetaGetIndexInfo - TestDB.capabilities.transactionIsolation

    override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
      val tableNames = profile.defaultTables.map(_.map(_.name.name)).map(_.toVector)
      tableNames
    }

    override def localSequences(implicit ec: ExecutionContext): DBIO[Vector[String]] = {
      // user_sequences much quicker than going to metadata if you don't know the schema they are going to be in
      sql"select sequence_Name from user_sequences".as[String]
    }

    override def dropUserArtifacts(implicit session: profile.Backend#Session) =
      blockingRunOnSession { implicit ec =>
        for {
          tables <- localTables
          sequences <- localSequences
          _ <- DBIO.seq(tables.map(t => sqlu"drop table #${profile.quoteIdentifier(t)} cascade constraints") ++
                        sequences.map(s => sqlu"drop sequence #${profile.quoteIdentifier(s)}"): _*)
        } yield ()
      }

  }
}

abstract class H2TestDB(confName: String, keepAlive: Boolean) extends InternalJdbcTestDB(confName) {
  val profile: Profile = H2Profile
  val jdbcDriver = "org.h2.Driver"
  override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions - TestDB.capabilities.jdbcMetaGetClientInfoProperties
  override def createDB(): profile.Backend#Database = database.forURL(url, driver = jdbcDriver, keepAliveConnection = keepAlive)
}

class SQLiteTestDB(dburl: String, confName: String) extends InternalJdbcTestDB(confName) {
  import profile.api.actionBasedSQLInterpolation
  val profile = SQLiteProfile
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = blockingRunOnSession { implicit ec =>
    for {
      tables <- localTables
      sequences <- localSequences
      _ <- DBIO.seq((tables.map(t => sqlu"""drop table if exists #${profile.quoteIdentifier(t)}""") ++
                     sequences.map(t => sqlu"""drop sequence if exists #${profile.quoteIdentifier(t)}""")): _*)
    } yield ()
  }
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  import profile.api.actionBasedSQLInterpolation
  val profile = DerbyProfile
  System.setProperty("derby.stream.error.method", classOf[DerbyDB].getName + ".DEV_NULL")
  val jdbcDriver = "org.apache.derby.jdbc.EmbeddedDriver"
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables(null, "APP", null, null)).map { ts =>
      ts.map(_._3).sorted
    }
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = try {
    blockingRunOnSession { implicit ec =>
      for {
        _ <- sqlu"""create table "__derby_dummy"(x integer primary key)""".asTry
        constraints <- sql"""select c.constraintname, t.tablename
                             from sys.sysconstraints c, sys.sysschemas s, sys.systables t
                             where c.schemaid = s.schemaid and c.tableid = t.tableid and s.schemaname = 'APP'
                          """.as[(String, String)]
        _ <- DBIO.seq((for((c, t) <- constraints if !c.startsWith("SQL"))
                yield sqlu"""alter table ${profile.quoteIdentifier(t)} drop constraint ${profile.quoteIdentifier(c)}"""): _*)
        tables <- localTables
        sequences <- localSequences
        _ <- DBIO.seq((tables.map(t => sqlu"""drop table #${profile.quoteIdentifier(t)}""") ++
                       sequences.map(t => sqlu"""drop sequence #${profile.quoteIdentifier(t)}""")): _*)
      } yield ()
    }
  } catch {
    case e: Exception =>
      println("[Caught Exception while dropping user artifacts in Derby: "+e+"]")
      session.close()
      cleanUpBefore()
  }
}

object DerbyDB {
  val DEV_NULL = new java.io.OutputStream { def write(b: Int) {} };
}

abstract class HsqlDB(confName: String) extends InternalJdbcTestDB(confName) {
  val profile = HsqldbProfile
  val jdbcDriver = "org.hsqldb.jdbcDriver"
  // Hsqldb has valid "select for update" syntax, but in testing, it either takes a whole table lock or no exclusive
  // lock at all, so exclude from ForUpdate testing
  override def capabilities = super.capabilities - TestDB.capabilities.selectForUpdateRowLocking
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    ResultSetAction[(String,String,String, String)](_.conn.getMetaData().getTables(null, "PUBLIC", null, null)).map { ts =>
      ts.map(_._3).sorted
    }
  override def createDB(): profile.Backend#Database = {
    val db = super.createDB()
    Await.result(db.run(SimpleJdbcAction(_ => ())), Duration.Inf)
    Logger.getLogger("hsqldb.db").setLevel(Level.WARNING)
    Logger.getLogger("org.hsqldb").setLevel(Level.WARNING)
    db
  }
}
