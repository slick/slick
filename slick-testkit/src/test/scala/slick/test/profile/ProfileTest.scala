package slick.test.profile

import org.junit.runner.RunWith
import com.typesafe.slick.testkit.util.{StandardTestDBs, ProfileTest, Testkit}

@RunWith(classOf[Testkit])
class H2MemTest extends ProfileTest(StandardTestDBs.H2Mem)

@RunWith(classOf[Testkit])
class H2RownumTest extends ProfileTest(StandardTestDBs.H2Rownum)

@RunWith(classOf[Testkit])
class H2DiskTest extends ProfileTest(StandardTestDBs.H2Disk)

@RunWith(classOf[Testkit])
class HsqldbMemTest extends ProfileTest(StandardTestDBs.HsqldbMem)

@RunWith(classOf[Testkit])
class HsqldbDiskTest extends ProfileTest(StandardTestDBs.HsqldbDisk)

@RunWith(classOf[Testkit])
class SQLiteMemTest extends ProfileTest(StandardTestDBs.SQLiteMem)

@RunWith(classOf[Testkit])
class SQLiteDiskTest extends ProfileTest(StandardTestDBs.SQLiteDisk)

@RunWith(classOf[Testkit])
class DerbyMemTest extends ProfileTest(StandardTestDBs.DerbyMem)

@RunWith(classOf[Testkit])
class DerbyDiskTest extends ProfileTest(StandardTestDBs.DerbyDisk)

@RunWith(classOf[Testkit])
class PostgresTest extends ProfileTest(StandardTestDBs.Postgres)

@RunWith(classOf[Testkit])
class MySQLTest extends ProfileTest(StandardTestDBs.MySQL)

@RunWith(classOf[Testkit])
class HeapTest extends ProfileTest(StandardTestDBs.Heap)

@RunWith(classOf[Testkit])
class DB2Test extends ProfileTest(StandardTestDBs.DB2)

@RunWith(classOf[Testkit])
class OracleTest extends ProfileTest(StandardTestDBs.Oracle)

@RunWith(classOf[Testkit])
class SQLServerJTDSTest extends ProfileTest(StandardTestDBs.SQLServerJTDS)

@RunWith(classOf[Testkit])
class SQLServer2012JTDSTest extends ProfileTest(StandardTestDBs.SQLServer2012JTDS)

@RunWith(classOf[Testkit])
class SQLServer2014JTDSTest extends ProfileTest(StandardTestDBs.SQLServer2014JTDS)

@RunWith(classOf[Testkit])
class SQLServerSQLJDBCTest extends ProfileTest(StandardTestDBs.SQLServerSQLJDBC)

@RunWith(classOf[Testkit])
class SQLServer2012SQLJDBCTest extends ProfileTest(StandardTestDBs.SQLServer2012SQLJDBC)

@RunWith(classOf[Testkit])
class SQLServer2014SQLJDBCTest extends ProfileTest(StandardTestDBs.SQLServer2014SQLJDBC)
