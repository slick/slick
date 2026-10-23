package slick.test.jdbc

import java.sql.Types

import org.junit.Assert._
import org.junit.Test

import slick.jdbc.JdbcModelBuilder
import slick.jdbc.meta.{MColumn, MQName, MTable}
import slick.sql.SqlProfile.ColumnOption.SqlType

class JdbcModelBuilderTypeNameTest {
  private val builder = new JdbcModelBuilder(Nil, ignoreInvalidDefaults = false)
  private val metaTable = MTable(MQName.local("types"), "TABLE", "", None, None, None)
  private val table = new builder.TableBuilder(
    metaTable, new builder.TableNamer(metaTable), Nil, Nil, Nil, Nil)

  private def column(sqlType: Int, name: String, size: Option[Int], digits: Option[Int]): builder.ColumnBuilder = {
    val meta = MColumn(
      table = metaTable.name, name = "value", sqlType = sqlType, typeName = name,
      size = size, decimalDigits = digits, numPrecRadix = 10, nullable = Some(false), remarks = None,
      columnDef = None, charOctetLength = 0, ordinalPosition = 1, isNullable = Some(false), scope = None,
      sourceDataType = None, isAutoInc = Some(false), isGenerated = Some(false))
    builder.createColumnBuilder(table, meta)
  }

  @Test def testNumericPrecisionAndScale(): Unit = {
    val cases = Seq(
      (Types.DECIMAL, "DECIMAL", Some(31), Some(25), "DECIMAL(31,25)"),
      (Types.NUMERIC, "numeric", Some(10), Some(0), "numeric(10,0)"),
      (Types.NUMERIC, "NUMBER", Some(12), Some(2), "NUMBER(12,2)"),
      (Types.DECIMAL, "DECIMAL(31,25)", Some(31), Some(25), "DECIMAL(31,25)"),
      (Types.DECIMAL, "DECIMAL", None, Some(2), "DECIMAL"),
      (Types.DECIMAL, "DECIMAL", Some(10), None, "DECIMAL"),
      (Types.DECIMAL, "DECIMAL", Some(10), Some(-1), "DECIMAL"),
      (Types.NUMERIC, "MONEY", Some(10), Some(2), "MONEY"))
    cases.foreach { case (sqlType, name, size, digits, expected) =>
      val c = column(sqlType, name, size, digits)
      assertEquals(name, Some(expected), c.dbType)
      assertTrue(name, c.model.options.contains(SqlType(expected)))
    }
  }

  @Test def testFractionalSecondPrecision(): Unit = {
    val cases = Seq(
      (Types.TIMESTAMP, "TIMESTAMP", Some(26), Some(6), "TIMESTAMP(6)"),
      (Types.TIME, "TIME", Some(12), Some(3), "TIME(3)"),
      (Types.TIMESTAMP, "DATETIME", Some(23), Some(3), "DATETIME(3)"),
      (Types.TIMESTAMP, "datetime2", Some(27), Some(7), "datetime2(7)"),
      (Types.TIMESTAMP, "TIMESTAMP(6)", Some(26), Some(6), "TIMESTAMP(6)"),
      (Types.TIMESTAMP, "TIMESTAMP WITH TIME ZONE", Some(32), Some(6), "TIMESTAMP WITH TIME ZONE"),
      (Types.TIME, "TIME", Some(8), None, "TIME"),
      (Types.TIME, "TIME", Some(8), Some(-1), "TIME"),
      (Types.DATE, "DATE", Some(10), Some(0), "DATE"))
    cases.foreach { case (sqlType, name, size, digits, expected) =>
      val c = column(sqlType, name, size, digits)
      assertEquals(name, Some(expected), c.dbType)
      assertTrue(name, c.model.options.contains(SqlType(expected)))
    }
  }

  @Test def testStringLengthIsStillSeparate(): Unit = {
    val c = column(Types.VARCHAR, "VARCHAR", Some(254), None)
    assertEquals(Some("VARCHAR"), c.dbType)
    assertEquals(Some(254), c.length)
  }
}
