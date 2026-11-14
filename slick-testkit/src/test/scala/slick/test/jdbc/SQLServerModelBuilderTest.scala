package slick.test.jdbc

import java.sql.Types
import java.util.UUID

import org.junit.Assert._
import org.junit.Test

import slick.jdbc.SQLServerProfile
import slick.jdbc.meta.{MColumn, MQName, MTable}
import slick.relational.RelationalProfile.ColumnOption.Default

class SQLServerModelBuilderTest {
  private val builder = new SQLServerProfile.ModelBuilder(Nil, ignoreInvalidDefaults = false)
  private val metaTable = MTable(MQName.local("defaults"), "TABLE", "", None, None, None)
  private val table = new builder.TableBuilder(
    metaTable, new builder.TableNamer(metaTable), Nil, Nil, Nil, Nil)

  private def column(value: Option[String], sqlType: Int = Types.INTEGER, typeName: String = "int",
                     nullable: Boolean = false): builder.ColumnBuilder = {
    val meta = MColumn(
      table = metaTable.name, name = "value", sqlType = sqlType, typeName = typeName,
      size = Some(254), decimalDigits = None, numPrecRadix = 10, nullable = Some(nullable), remarks = None,
      columnDef = value, charOctetLength = 0, ordinalPosition = 1, isNullable = Some(nullable), scope = None,
      sourceDataType = None, isAutoInc = Some(false), isGenerated = Some(false))
    builder.createColumnBuilder(table, meta)
  }

  @Test def testLiteralDefaults(): Unit = {
    val cases = Seq(
      ("((0))", Types.BIT, "bit", false),
      ("((1))", Types.BIT, "bit", true),
      ("((-5))", Types.INTEGER, "int", -5),
      ("((5))", Types.BIGINT, "bigint", 5L),
      ("((9.999))", Types.DOUBLE, "float", 9.999),
      ("((12.34))", Types.DECIMAL, "decimal", BigDecimal("12.34")),
      ("('x')", Types.VARCHAR, "varchar", "x"),
      ("(('(text)'))", Types.VARCHAR, "varchar", "(text)"),
      ("(')')", Types.VARCHAR, "varchar", ")"),
      ("('')", Types.VARCHAR, "varchar", ""))
    cases.foreach { case (raw, sqlType, typeName, expected) =>
      val c = column(Some(raw), sqlType, typeName)
      assertEquals(raw, Some(Some(expected)), c.default)
      assertTrue(raw, c.model.options.contains(Default(expected)))
    }
  }

  @Test def testNullableAndMissingDefaults(): Unit = {
    assertEquals(None, column(None).rawDefault)
    assertEquals(None, column(None).default)
    assertEquals(Some(None), column(Some("((NULL))"), nullable = true).default)
    assertTrue(column(Some("((NULL))"), nullable = true).model.options.contains(Default(None)))
    assertTrue(column(Some("((1))"), Types.BIT, "bit", nullable = true)
      .model.options.contains(Default(Some(true))))
  }

  @Test def testUUIDDefaults(): Unit = {
    val uuid = UUID.fromString("01234567-89ab-cdef-0123-456789abcdef")
    assertEquals(Some(Some(uuid)), column(Some(s"(('$uuid'))"), Types.CHAR, "uniqueidentifier").default)
    assertEquals(Some(None), column(Some("(newid())"), Types.CHAR, "uniqueidentifier").default)
  }

  @Test def testOnlyEnclosingParenthesesAreRemoved(): Unit = {
    val cases = Seq(
      "1" -> "1",
      " ( ( -5 ) ) " -> "-5",
      "('it''s (text)')" -> "'it''s (text)'",
      "('a'' ) b')" -> "'a'' ) b'",
      "('line\nbreak')" -> "'line\nbreak'",
      "(1)+(2)" -> "(1)+(2)",
      "((1)+(2))" -> "(1)+(2)",
      "('a')+('b')" -> "('a')+('b')",
      "(getdate())" -> "getdate()",
      "((1)" -> "((1)")
    cases.foreach { case (raw, expected) =>
      assertEquals(raw, Some(expected), column(Some(raw)).rawDefault)
    }
  }
}
