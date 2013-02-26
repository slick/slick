package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class BooleanConditionTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  object Users extends Table[Int]("users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id
  }

  def test {
    Users.ddl.create
    Users.insertAll(1,2,3,4,5,6)
    
    // === compiles and runs fine
    println((for {
      id <- Users.map(_.id) if id === 1
    } yield id).list)

    // == does not compile, caught by withFilter macro
    /*
    println((for {
      id <- Users.map(_.id) if id == 1
    } yield id).list)
    */

    // for expression with pattern matching works fine, too despite generated withFilter( T => Boolean )
    println((for {
      (id ~ id_) <- Users.map(t => t.id ~ t.id)
    } yield id).list)

    // ----------------------------------------------------
    // not the focus of this patch, but by the way:

    // works fine, no withFilter( T => Boolean ) is generated
    println((for {
      (id,id_) <- Users.map(t => (t.id,t.id))
    } yield id).list)

    /*
    // machting a constant does not compile
    println((for {
      (id ~ 1) <- Users.map(t => t.id ~ t.id)
    } yield id).list)
    println((for {
      (id,1) <- Users.map(t => (t.id,t.id))
    } yield id).list)
    
    // matching an explicitly lifted constant fails at runtime with exception
    println((for {
      (id ~ ConstColumn(1)) <- Users.map(t => t.id ~ t.id)
    } yield id).list)
    println((for {
      (id,ConstColumn(1)) <- Users.map(t => (t.id,t.id))
    } yield id).list)
    */
  }
}
