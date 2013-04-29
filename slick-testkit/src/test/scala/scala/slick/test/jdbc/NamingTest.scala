package scala.slick.test.jdbc

import java.io.PrintWriter
import org.junit.Test
import org.junit.Assert._
import scala.slick.schema.naming.Naming
import scala.slick.schema.naming.NamingDefault
import scala.collection.immutable.Map
import scala.slick.schema.naming.NamingConfigured
import com.typesafe.config.ConfigFactory
import scala.slick.schema.QualifiedName
import scala.slick.jdbc.meta.MQName
import com.typesafe.config.Config
import scala.slick.schema.naming.MappingConfiguration
import language.implicitConversions

class NamingTest {
  type Input = Map[String, List[String]]
  type Value = (String, String)
  type Result = Map[(String, String), List[(String, String)]]

  val inputTest = Map(
    "COFFEES" -> List("COFFEE_ID", "NAME"),
    "SUPPLIER" -> List("SUPP_ID", "TOTAL"))

  @Test def predefinedFunctionsTest() {
    import scala.slick.schema.naming.Rule._
    assertEquals("capitalize should work", "CoffeEs", Capitalize("coffeEs"))
    assertEquals("camelize should work", "coffeeIdSOmething", Camelize("coffee_id_sOmething"))
    assertEquals("lowercase should work", "coffee_id_something", LowerCase("COFFEE_Id_Something"))
    assertEquals("singularize should change plural to singular form", "CoFFEE", Singularize("CoFFEES"))
    assertEquals("singularize should append 'C' to singular", "CCOFfEE", Singularize("COFfEE"))
    assertEquals("pluralize should change convert 'Xs' to 'Xses'", "BuSes", Pluralize("BuS"))
    assertEquals("pluralize should change convert 'X' to 'Xs'", "CoFfEEs", Pluralize("CoFfEE"))
  }

  implicit def namingToMapper(namingConf: Config): MappingConfiguration =
    MappingConfiguration(namingConf)

  @Test def defaultNamingTest() {
    val naming = NamingDefault
    val correctMap = Map(
      ("Coffees", "Coffee") -> List(("coffeeId", "coffeeId"), ("name", "name")),
      ("Supplier", "CSupplier") -> List(("suppId", "suppId"), ("total", "total")))
    checker(naming, inputTest, correctMap)
  }

  @Test def configuredNamingTest() {
    val namingConf = ConfigFactory.parseString("""naming {
  table-module = [lowercase, capitalize, camelize, pluralize]
  case-class = [lowercase, capitalize, camelize]
  module-field = [lowercase]
}
""")
    val naming = new NamingConfigured(namingConf)
    val correctMap = Map(
      ("Coffeeses", "Coffees") -> List(("coffee_id", "coffeeId"), ("name", "name")),
      ("Suppliers", "Supplier") -> List(("supp_id", "suppId"), ("total", "total")))
    checker(naming, inputTest, correctMap)
  }

  @Test def customTableNamingTest() {
    val namingConf = ConfigFactory.parseString("""naming {
  table-module = [lowercase, capitalize, camelize, pluralize]
  custom {
    COFFEES {
      table-module = COffees
      case-class = CoFfee
    }
  }
}
""")
    val naming = new NamingConfigured(namingConf)
    val correctMap = Map(
      ("COffees", "CoFfee") -> List(("coffeeId", "coffeeId"), ("name", "name")),
      ("Suppliers", "CSupplier") -> List(("suppId", "suppId"), ("total", "total")))
    checker(naming, inputTest, correctMap)
  }

  @Test def customColumnNamingTest() {
    val namingConf = ConfigFactory.parseString("""naming {
  module-field = [lowercase]
  custom {
    COFFEES {
      custom {
        COFFEE_ID {
          module-field = id
          case-field = cid
        }
      }
    }
    SUPPLIER {
      module-field = [lowercase, capitalize]
    }
  }
}
""")
    val naming = new NamingConfigured(namingConf)
    val correctMap = Map(
      ("Coffees", "Coffee") -> List(("id", "cid"), ("name", "name")),
      ("Supplier", "CSupplier") -> List(("Supp_id", "suppId"), ("Total", "total")))
    checker(naming, inputTest, correctMap)
  }

  def checker(naming: Naming, input: Input, correctMap: Result) {
    @inline def qnameForTable(table: String): QualifiedName = QualifiedName(MQName(None, None, table))
    @inline def tableResult(table: String): (String, String) = {
      val name = qnameForTable(table)
      (naming.tableSQLToModule(name), naming.tableSQLToEntity(name))
    }
    @inline def columnResult(table: String)(column: String): (String, String) = {
      val name = QualifiedName.columnName(qnameForTable(table), column)
      (naming.columnSQLToModuleField(name), naming.columnSQLToEntityField(name))
    }
    val keys = input.keysIterator.zip(correctMap.keysIterator)
    keys.foreach {
      case (table, result) => {
        assertEquals("Table naming should work", result, tableResult(table))
        val columns = input(table)
        val resultColumns = correctMap(result)
        val values = columns.zip(resultColumns)
        values.foreach {
          case (column, resultColumn) => {
            assertEquals("Column naming should work", resultColumn, columnResult(table)(column))
          }
        }
      }
    }
  }
}
