package scala.slick.test.driver

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{DriverTest, Testkit}
import scala.slick.testutil.TestDBs

@RunWith(classOf[Testkit])
class H2MemTest extends DriverTest(TestDBs.H2Mem)

@RunWith(classOf[Testkit])
class H2DiskTest extends DriverTest(TestDBs.H2Disk)

@RunWith(classOf[Testkit])
class HsqldbMemTest extends DriverTest(TestDBs.HsqldbMem)

@RunWith(classOf[Testkit])
class HsqldbDiskTest extends DriverTest(TestDBs.HsqldbDisk)

@RunWith(classOf[Testkit])
class SQLiteMemTest extends DriverTest(TestDBs.SQLiteMem)

@RunWith(classOf[Testkit])
class SQLiteDiskTest extends DriverTest(TestDBs.SQLiteDisk)

@RunWith(classOf[Testkit])
class DerbyMemTest extends DriverTest(TestDBs.DerbyMem)

@RunWith(classOf[Testkit])
class DerbyDiskTest extends DriverTest(TestDBs.DerbyDisk)

@RunWith(classOf[Testkit])
class PostgresTest extends DriverTest(TestDBs.Postgres)

@RunWith(classOf[Testkit])
class MySQLTest extends DriverTest(TestDBs.MySQL)

@RunWith(classOf[Testkit])
class SQLServerJTDSTest extends DriverTest(TestDBs.SQLServerJTDS)

@RunWith(classOf[Testkit])
class SQLServerSQLJDBCTest extends DriverTest(TestDBs.SQLServerSQLJDBC)

@RunWith(classOf[Testkit])
class MSAccessTest extends DriverTest(TestDBs.MSAccess)

@RunWith(classOf[Testkit])
class HeapTest extends DriverTest(TestDBs.Heap)
