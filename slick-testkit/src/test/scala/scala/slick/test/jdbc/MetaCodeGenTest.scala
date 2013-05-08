package scala.slick.test.jdbc

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.PrintWriter
import java.io.StringWriter
import java.net.URLClassLoader

import scala.Array.apply
import scala.Some.apply
import scala.collection.immutable.List.apply
import scala.slick.jdbc.meta.CodeGen
import scala.slick.jdbc.meta.MColumn
import scala.slick.jdbc.meta.MTable
import scala.slick.testutil.DBTest
import scala.slick.testutil.DBTestObject
import scala.slick.testutil.TestDBs.DerbyMem
import scala.slick.testutil.TestDBs.H2Mem
import scala.slick.testutil.TestDBs.HsqldbMem
import scala.slick.testutil.TestDBs.MySQL
import scala.slick.testutil.TestDBs.Postgres
import scala.slick.testutil.TestDBs.SQLServerJTDS
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.Test
import org.junit.runner.RunWith

import com.typesafe.slick.testkit.util.TestDB

//object MetaCodeGenTest extends DBTestObject(SQLiteMem)
object MetaCodeGenTest extends DBTestObject(H2Mem,/* SQLiteMem,*/ Postgres, MySQL, DerbyMem, HsqldbMem, SQLServerJTDS)

class MetaCodeGenTest(val tdb: TestDB) extends DBTest {
  import tdb.profile.simple._
  import Database.threadLocalSession

  val userTable = "generated_user"

  object Users extends Table[(Int, String, Option[String], Option[Int])](userTable) {
    def id = column[Int]("id", O.PrimaryKey)
    def name = column[String]("name")
    def nick = column[Option[String]]("nick")
    def age = column[Option[Int]]("age")
    def * = id ~ name ~ nick ~ age
  }

  val cols = "id, name, age".split(",").transform(_.trim.toLowerCase).toSet

  def colPicker(c: MColumn) = cols.contains(c.column.toLowerCase)

  @Test def testOutput() {

    db withSession {

      println("DDL used to create tables:")
      for (s <- Users.ddl.createStatements) println("  " + s)
      Users.ddl.create

      Users.insert(10, "John", None, Some(99));
      Users.insert(11, "Petra", Some("Petra"), None);

      generate(CodeGen.output, "GeneratedUser")
    }
  }
  
  @Test def testOutputCase() {

    db withSession {

      println("DDL used to create tables:")
      for (s <- Users.ddl.createStatements) println("  " + s)
      Users.ddl.create

      Users.insert(10, "John", None, Some(99));
      Users.insert(11, "Petra", Some("Petra"), None);

      generate(CodeGen.outputCase, "GeneratedUserTable")
    }
  }

  def generate(method: (MTable, PrintWriter, MColumn => Boolean) => Unit, tableName: String) = {
    val strOut = new StringWriter
    val out = new PrintWriter(strOut)
    for (t <- MTable.getTables(None, None, Some(userTable), None).list)
      method(t, out, colPicker)
    out.flush
    val code = template(strOut.toString, tableName)

    println("Generated code:")
    println(code)

    val outdir = s"target/CodeGen"
    val dir = new File(outdir)
    dir.mkdirs

    val reporter = compile(save(code, dir), dir)
    reporter.printSummary
    assert(!reporter.hasErrors, "compiler reported errors")

    val result = run(dir)
    assert(result == 2, s"expected: 2, actual: $result")
  }

  private def className[A](a: A) = {
    val name = a.getClass.getName
    if (name endsWith "$") name.subSequence(0, name.length() - 1)
    else name
  }

  def save(code: String, dir: File) = {
    val source = "sourceCode.scala"
    val file = new File(dir, source)
    //   file.deleteOnExit

    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(code)
    writer.close

    file
  }

  def compile(source: File, outdir: File) = {

    // called in the event of a compilation error 
    def error(message: String) { println(message) }

    val settings = new Settings(error)
    settings.outdir.value = outdir.getPath
    //    settings.feature.value = true
    //    settings.language.
    settings.deprecation.value = true // enable detailed deprecation warnings 
    settings.unchecked.value = true // enable detailed unchecked warnings 

    val reporter = new ConsoleReporter(settings)

    val compiler = new EmbeddedScalaCompiler(settings, reporter)
    val run = new compiler.Run
    run.compile(List(source.getPath))

    reporter
  }

  def run(dir: File) = {
    val loader = new URLClassLoader(Array(dir.toURI.toURL))

    val clazz: Class[_] = Class.forName("scala.slick.test.jdbc.GeneratedUserTester", true, loader)
    val argClazz: Class[_] = Class.forName("scala.slick.jdbc.JdbcBackend$DatabaseDef", true, loader)

//    clazz.getDeclaredMethods() foreach println

    db withSession {

        val instance = clazz.newInstance
        val method = clazz.getMethod("test", argClazz)
        val result = method.invoke(instance, db)
        //    			println(s"Result: $result")
        result
    }
  }

  def template(code: String, tableName: String) = s"""
package scala.slick.test.jdbc
      
import ${className(tdb.driver)}.simple._
import Database.threadLocalSession
      
class GeneratedUserTester {
      
${code}

  def test(db: Database) = {
    db withSession {
      println("DDL used to create tables:")
      val res = (for (s <- Query(${tableName})) yield s).list
      
      val count = res.size
      printf("Found %d records in database", count)
      count
    }
  }
}
"""

}

