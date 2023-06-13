package com.typesafe.slick.testkit.util

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

import slick.dbio.DBIO

import org.junit.{After, Before}
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

@RunWith(classOf[Parameterized])
abstract class DBTest {
  private[this] var dbInitialized = false
  val tdb: JdbcTestDB

  lazy val db = {
    dbInitialized = true
    tdb.createDB()
  }

  println("[Using test database "+tdb+"]")

  def runBlocking[T](a: DBIO[T]): Unit = Await.result(db.run(a), Duration.Inf)

  @Before def beforeDBTest = tdb.cleanUpBefore()
  @After def afterDBTest = {
    if(dbInitialized) db.close()
    tdb.cleanUpAfter()
  }
}

abstract class DBTestObject(dbs: TestDB*) {
  val testClassName = {
    val s = getClass.getName
    s.substring(0, s.length-1)
  }
  @Parameters def parameters: java.util.List[Array[TestDB]] = dbs.filter(_.isEnabled).map(to => Array(to)).asJava
}
