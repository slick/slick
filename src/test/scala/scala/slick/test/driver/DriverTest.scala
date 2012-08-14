package scala.slick.test.driver

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{DriverTest, Testkit}
import scala.slick.testutil.TestDB

@RunWith(classOf[Testkit])
class H2MemTest extends DriverTest(TestDB.H2Mem)

@RunWith(classOf[Testkit])
class H2DiskTest extends DriverTest(TestDB.H2Disk)

@RunWith(classOf[Testkit])
class HsqldbMemTest extends DriverTest(TestDB.HsqldbMem)

@RunWith(classOf[Testkit])
class HsqldbDiskTest extends DriverTest(TestDB.HsqldbDisk)

@RunWith(classOf[Testkit])
class SQLiteMemTest extends DriverTest(TestDB.SQLiteMem)

@RunWith(classOf[Testkit])
class SQLiteDiskTest extends DriverTest(TestDB.SQLiteDisk)

@RunWith(classOf[Testkit])
class DerbyMemTest extends DriverTest(TestDB.DerbyMem)

@RunWith(classOf[Testkit])
class DerbyDiskTest extends DriverTest(TestDB.DerbyDisk)

@RunWith(classOf[Testkit])
class PostgresTest extends DriverTest(TestDB.Postgres)

@RunWith(classOf[Testkit])
class MySQLTest extends DriverTest(TestDB.MySQL)

@RunWith(classOf[Testkit])
class SQLServerTest extends DriverTest(TestDB.SQLServer)

@RunWith(classOf[Testkit])
class MSAccessTest extends DriverTest(TestDB.MSAccess)
