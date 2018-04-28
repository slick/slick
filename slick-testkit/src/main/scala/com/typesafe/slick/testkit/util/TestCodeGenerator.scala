package com.typesafe.slick.testkit.util

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.{Codec, Source}

import java.util.concurrent.ExecutionException

import slick.codegen.{OutputHelpers, SourceCodeGenerator}
import slick.dbio._
import slick.model.Model

import org.junit.Test

trait TestCodeGenerator {
  def packageName: String
  def defaultTestCode(c: Config): String
  def configurations: Seq[Config]
  def computeFullTdbName(tdbName: String) = StandardTestDBs.getClass.getName.replaceAll("\\$", "") + "." + tdbName

  def main(args: Array[String]): Unit = try {
    val clns = configurations.flatMap(_.generate(args(0)).toSeq)
    new OutputHelpers {
      def indent(code: String): String = code
      def code: String = ""
      def codePerTable:Map[String,String] = Map()
      def codeForContainer:String = ""
    }.writeStringToFile(
      s"""
         |package $packageName
         |object AllTests extends com.typesafe.slick.testkit.util.TestCodeRunner.AllTests {
         |  val clns = Seq(${clns.map("\"" + _ + "\"").mkString(", ")})
         |}
       """.stripMargin, args(0), packageName, "AllTests.scala"
    )
  } catch { case ex: Throwable =>
    ex.printStackTrace(System.err)
    System.exit(1)
  }

  class Config(val objectName: String, val tdb: JdbcTestDB, tdbName: String, initScripts: Seq[String]) { self =>
    def useSingleLineStatements = false

    def slickProfile = tdb.profile.getClass.getName.replaceAll("\\$", "")

    def fullTdbName = computeFullTdbName(tdbName)

    def generate(dir: String): Option[String] = if(tdb.isEnabled || tdb.isInstanceOf[InternalJdbcTestDB]) {
      tdb.cleanUpBefore()
      try {
        var init: DBIO[Any] = DBIO.successful(())
        var current: String = null
        initScripts.foreach { initScript =>
          import tdb.profile.api._
          Source.fromURL(self.getClass.getResource(initScript))(Codec.UTF8).getLines().foreach { s =>
            if(current eq null) current = s else current = current + "\n" + s
            if(s.trim.endsWith(";")) {
              if(useSingleLineStatements) {
                current = current.substring(0, current.length-1)
                current = current.replace("\r", "").replace('\n', ' ')
              }
              init = init >> sqlu"#$current"
              current = null
            }
          }
          if(current ne null) {
            if(useSingleLineStatements) current = current.replace("\r", "").replace('\n', ' ')
            init = init >> sqlu"#$current"
          }
        }
        val db = tdb.createDB()
        try {
          val m = Await.result(db.run((init >> generator).withPinnedSession), Duration.Inf)
          m.writeToFile(profile=slickProfile, folder=dir, pkg=packageName, objectName, fileName=objectName+".scala" )
        } finally db.close
      }
      finally tdb.cleanUpAfter()
      Some(s"$packageName.$objectName")
    } else None

    def generator: DBIO[SourceCodeGenerator] =
      tdb.profile.createModel(ignoreInvalidDefaults=false).map(new MyGen(_))

    def testCode: String = defaultTestCode(this)

    class MyGen(model:Model) extends SourceCodeGenerator(model) {
      override def entityName = sqlName => {
        val baseName = super.entityName(sqlName)
        if(baseName.dropRight(3).last == 's') baseName.dropRight(4)
        else baseName
      }
      override def parentType = Some("com.typesafe.slick.testkit.util.TestCodeRunner.TestCase")
      override def code = {
        s"""
           |lazy val tdb = $fullTdbName
           |def test = {
           |  import org.junit.Assert._
           |  import scala.concurrent.ExecutionContext.Implicits.global
           |  $testCode
           |}
           |""".stripMargin + super.code
      }
    }
  }
}

class TestCodeRunner(tests: TestCodeRunner.AllTests) {
  def run(cln: String): Unit = {
    val t = Class.forName(cln+"$").getField("MODULE$").get(null).asInstanceOf[TestCodeRunner.TestCase]
    val tdb = t.tdb
    println(s"Running test $cln on ${tdb.confName}")
    if(tdb.isEnabled) {
      tdb.cleanUpBefore()
      try {
        val a = t.test
        val db = tdb.createDB()
        try Await.result(db.run(a.withPinnedSession), Duration.Inf)
        catch { case e: ExecutionException => throw e.getCause }
        finally db.close()
      } finally tdb.cleanUpAfter()
    } else println("- Test database is disabled")
  }

  @Test def allTests = tests.clns.foreach(run)
}

object TestCodeRunner {
  trait AllTests {
    def clns: Seq[String]
  }
  trait TestCase {
    def test: slick.dbio.DBIO[Any]
    def tdb: JdbcTestDB
  }
}
