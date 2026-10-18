package slick.test.jdbc

import org.junit.Test
import org.junit.Assert._

import cats.effect.unsafe.implicits.global

import com.typesafe.slick.testkit.util.{DBTest, DBTestObject, JdbcTestDB}
import com.typesafe.slick.testkit.util.StandardTestDBs._

import scala.reflect.{classTag, ClassTag}
import scala.util.Failure

import slick.codegen.SourceCodeGenerator
import slick.jdbc.{H2Profile, JdbcModelBuilder}
import slick.jdbc.meta.MTable

object ModelBuilderTest extends DBTestObject(H2Mem)

object Outer1803 { class Inner }

/** Maps INTEGER to a class nested in a Scala object and VARCHAR to a Java nested class (#1803) */
object NestedClassH2Profile1803 extends H2Profile {
  class NestedClassModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)
    extends H2ModelBuilder(mTables, ignoreInvalidDefaults) {
    override def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[?] = jdbcType match {
      case java.sql.Types.INTEGER => classTag[Outer1803.Inner]
      case java.sql.Types.VARCHAR => classTag[java.util.AbstractMap.SimpleEntry[?, ?]]
      case _ => super.jdbcTypeToScala(jdbcType, typeName)
    }
  }
  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean): JdbcModelBuilder =
    new NestedClassModelBuilder(tables, ignoreInvalidDefaults)
}

class ModelBuilderTest(val tdb: JdbcTestDB) extends DBTest {
  import tdb.profile.api._

  @Test
  def test(): Unit = {
    // test timestamps don't fail
    val a =
      sqlu"""create table BAR (FOO TIMESTAMP DEFAULT CURRENT_TIMESTAMP)""" >>
      tdb.profile.createModel(ignoreInvalidDefaults=false) >>
      sqlu"""create table BAZ (FOO VARCHAR(255) DEFAULT CURRENT_USER)""" >>
      tdb.profile.createModel(ignoreInvalidDefaults=false).asTry
    val mt = db.run(a.withPinnedSession).unsafeRunSync()
    assertTrue(mt.asInstanceOf[Failure[?]].exception.asInstanceOf[SlickException].getMessage.contains("not parse default"))
  }

  @Test
  def testNestedClassTypeNames(): Unit = {
    val a =
      sqlu"""create table T1803 (ID INTEGER NOT NULL, E VARCHAR(10) NOT NULL, B VARBINARY(10) NOT NULL)""" >>
      NestedClassH2Profile1803.createModel(Some(MTable.getTables(None, None, Some("T1803"), None)))
    val model = db.run(a.withPinnedSession).unsafeRunSync()
    val types = model.tables.head.columns.map(c => c.name -> c.tpe).toMap
    assertEquals("slick.test.jdbc.Outer1803.Inner", types("ID"))
    assertEquals("java.util.AbstractMap.SimpleEntry", types("E"))
    assertEquals("java.sql.Blob", types("B"))
    val code = new SourceCodeGenerator(model).code
    assertTrue(code, code.contains("id: slick.test.jdbc.Outer1803.Inner"))
    assertFalse(code, code.contains("$Inner"))
  }
}
