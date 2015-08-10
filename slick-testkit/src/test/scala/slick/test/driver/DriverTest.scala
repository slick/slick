package slick.test.driver

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{StandardTestDBs, DriverTest, Testkit}

@RunWith(classOf[Testkit])
class H2MemTest extends DriverTest(StandardTestDBs.H2Mem)

@RunWith(classOf[Testkit])
class H2RownumTest extends DriverTest(StandardTestDBs.H2Rownum)

@RunWith(classOf[Testkit])
class H2DiskTest extends DriverTest(StandardTestDBs.H2Disk)

@RunWith(classOf[Testkit])
class HsqldbMemTest extends DriverTest(StandardTestDBs.HsqldbMem)

@RunWith(classOf[Testkit])
class HsqldbDiskTest extends DriverTest(StandardTestDBs.HsqldbDisk)

@RunWith(classOf[Testkit])
class SQLiteMemTest extends DriverTest(StandardTestDBs.SQLiteMem)

@RunWith(classOf[Testkit])
class SQLiteDiskTest extends DriverTest(StandardTestDBs.SQLiteDisk)

@RunWith(classOf[Testkit])
class DerbyMemTest extends DriverTest(StandardTestDBs.DerbyMem)

@RunWith(classOf[Testkit])
class DerbyDiskTest extends DriverTest(StandardTestDBs.DerbyDisk)

@RunWith(classOf[Testkit])
class PostgresTest extends DriverTest(StandardTestDBs.Postgres)

@RunWith(classOf[Testkit])
class MySQLTest extends DriverTest(StandardTestDBs.MySQL)

@RunWith(classOf[Testkit])
class HeapTest extends DriverTest(StandardTestDBs.Heap)
