package com.typesafe.slick.testkit.util

import scala.collection.JavaConversions
import org.junit.{Before, After}
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters

import scala.concurrent.Await
import scala.concurrent.duration.Duration

@RunWith(classOf[Parameterized])
abstract class DBTest {
  private[this] var dbInitialized = false
  val tdb: JdbcTestDB

  lazy val db = {
    dbInitialized = true
    tdb.createDB()
  }

  println("[Using test database "+tdb+"]")

  @Before def beforeDBTest = tdb.cleanUpBefore()
  @After def afterDBTest = {
    if(dbInitialized) Await.ready(db.shutdown, Duration.Inf)
    tdb.cleanUpAfter()
  }
}

abstract class DBTestObject(dbs: TestDB*) {
  val testClassName = {
    val s = getClass.getName
    s.substring(0, s.length-1)
  }
  @Parameters def parameters = JavaConversions.seqAsJavaList(dbs.filter(_.isEnabled).map(to => Array(to)))
}
