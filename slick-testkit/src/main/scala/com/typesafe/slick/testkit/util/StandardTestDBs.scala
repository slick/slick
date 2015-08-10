package com.typesafe.slick.testkit.util

import java.io.File
import java.util.logging.{Level, Logger}
import java.sql.SQLException
import slick.compiler.Phase
import slick.dbio._
import slick.driver._
import slick.memory.MemoryDriver
import slick.jdbc.{SimpleJdbcAction, ResultSetAction}
import slick.jdbc.GetResult._
import slick.jdbc.meta.MTable
import org.junit.Assert

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

object StandardTestDBs {
  lazy val H2Mem = new H2TestDB("h2mem", false) {
    val url = "jdbc:h2:mem:test1"
    override def isPersistent = false
  }

  /** A modified H2Mem that tests the `removeTakeDrop` phase (which is not used by any of the
    * standard drivers. */
  lazy val H2Rownum = new H2TestDB("h2rownum", false) {
    val url = "jdbc:h2:mem:test_rownum"
    override def isPersistent = false
    override val driver = new H2Driver {
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
    val driver = PostgresDriver
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
    val driver = MySQLDriver
    // Recreating the DB is faster than dropping everything individually
    override def dropUserArtifacts(implicit session: profile.Backend#Session) = {
      session.close()
      cleanUpBefore()
    }
  }

  lazy val Heap = new RelationalTestDB {
    type Driver = MemoryDriver
    val driver: Driver = MemoryDriver
    val confName: String = "heap"
    def createDB: profile.Backend#Database = profile.backend.Database(ExecutionContext.global)
    def dropUserArtifacts(implicit session: profile.Backend#Session) {
      val db = session.database
      db.getTables.foreach(t => db.dropTable(t.name))
    }
    def assertTablesExist(tables: String*) = driver.api.SimpleDBIO { ctx =>
      val all = ctx.session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(!all.contains(t)) Assert.fail("Table "+t+" should exist")
      }
    }
    def assertNotTablesExist(tables: String*) = driver.api.SimpleDBIO { ctx =>
      val all = ctx.session.database.getTables.map(_.name).toSet
      for(t <- tables) {
        if(all.contains(t)) Assert.fail("Table "+t+" should not exist")
      }
    }
  }
}

abstract class H2TestDB(confName: String, keepAlive: Boolean) extends InternalJdbcTestDB(confName) {
  val driver: Driver = H2Driver
  val jdbcDriver = "org.h2.Driver"
  override def capabilities = super.capabilities - TestDB.capabilities.jdbcMetaGetFunctions - TestDB.capabilities.jdbcMetaGetClientInfoProperties
  override def createDB(): profile.Backend#Database = database.forURL(url, driver = jdbcDriver, keepAliveConnection = keepAlive)
}

class SQLiteTestDB(dburl: String, confName: String) extends InternalJdbcTestDB(confName) {
  import driver.api.actionBasedSQLInterpolation
  val driver = SQLiteDriver
  val url = dburl
  val jdbcDriver = "org.sqlite.JDBC"
  override def localTables(implicit ec: ExecutionContext): DBIO[Vector[String]] =
    super.localTables.map(_.filter(s => !s.toLowerCase.contains("sqlite_")))
  override def dropUserArtifacts(implicit session: profile.Backend#Session) = blockingRunOnSession { implicit ec =>
    for {
      tables <- localTables
      sequences <- localSequences
      _ <- DBIO.seq((tables.map(t => sqlu"""drop table if exists #${driver.quoteIdentifier(t)}""") ++
                     sequences.map(t => sqlu"""drop sequence if exists #${driver.quoteIdentifier(t)}""")): _*)
    } yield ()
  }
}

abstract class DerbyDB(confName: String) extends InternalJdbcTestDB(confName) {
  import driver.api.actionBasedSQLInterpolation
  val driver = DerbyDriver
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
                yield sqlu"""alter table ${driver.quoteIdentifier(t)} drop constraint ${driver.quoteIdentifier(c)}"""): _*)
        tables <- localTables
        sequences <- localSequences
        _ <- DBIO.seq((tables.map(t => sqlu"""drop table #${driver.quoteIdentifier(t)}""") ++
                       sequences.map(t => sqlu"""drop sequence #${driver.quoteIdentifier(t)}""")): _*)
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
  val driver = HsqldbDriver
  val jdbcDriver = "org.hsqldb.jdbcDriver"
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
