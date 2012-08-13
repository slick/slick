package scala.slick.test.driver

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{TestkitRunnable, TestkitRunner}
import scala.slick.testutil.TestDB

@RunWith(classOf[TestkitRunner])
class H2MemTest extends TestkitRunnable(TestDB.H2Mem)

@RunWith(classOf[TestkitRunner])
class H2DiskTest extends TestkitRunnable(TestDB.H2Disk)

@RunWith(classOf[TestkitRunner])
class HsqldbMemTest extends TestkitRunnable(TestDB.HsqldbMem)

@RunWith(classOf[TestkitRunner])
class HsqldbDiskTest extends TestkitRunnable(TestDB.HsqldbDisk)

@RunWith(classOf[TestkitRunner])
class SQLiteMemTest extends TestkitRunnable(TestDB.SQLiteMem)

@RunWith(classOf[TestkitRunner])
class SQLiteDiskTest extends TestkitRunnable(TestDB.SQLiteDisk)

@RunWith(classOf[TestkitRunner])
class DerbyMemTest extends TestkitRunnable(TestDB.DerbyMem)

@RunWith(classOf[TestkitRunner])
class DerbyDiskTest extends TestkitRunnable(TestDB.DerbyDisk)

@RunWith(classOf[TestkitRunner])
class PostgresTest extends TestkitRunnable(TestDB.Postgres)

@RunWith(classOf[TestkitRunner])
class MySQLTest extends TestkitRunnable(TestDB.MySQL)

@RunWith(classOf[TestkitRunner])
class SQLServerTest extends TestkitRunnable(TestDB.SQLServer)

@RunWith(classOf[TestkitRunner])
class MSAccessTest extends TestkitRunnable(TestDB.MSAccess)
