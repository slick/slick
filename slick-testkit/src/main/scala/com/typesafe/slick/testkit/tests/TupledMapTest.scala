package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}
import scala.slick.lifted.ColumnBase
import scala.language.postfixOps

class TupledMapTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

    abstract class IdTable[T](_schemaName: Option[String], _tableName: String) extends Table[Id[T]](_schemaName, _tableName) {
		def id = column[Int]("id", O NotNull, O PrimaryKey, O AutoInc)
		def this(_tableName: String) = this(None, _tableName)
		final def * : ColumnBase[Id[T]] = (id ~ **) <> (Id[T] _, Id.unapply[T] _)
		def ** : ColumnBase[T]
	}
	
	case class Triple(t1 : Int, t2 : String, t3 : String)
	
	object T extends IdTable[Triple]("T") {
	  def a = column[Int]("A")
	  def b = column[String]("B")
	  def c = column[String]("C")
	  def ** = a ~ b ~ c <> (Triple, Triple.unapply _)
	}  
	case class Id[A](val id:Int, val value:A) {
	  def map[B](f : A => B) = Id (id, f(value))
	}
	
  def testTupledMap {
    T.ddl.create
    T.**.insertAll(Triple(1, "1", "a"), Triple(2, "2", "b"))

    val res1 = List(Triple(1, "1", "a"), Triple(2, "2", "b"))

    val q1a = for {t <- T} yield t
    println("q1a: "+q1a.selectStatement)
    assertEquals(res1, q1a.to[List] map {id => id.value})
  }
}
