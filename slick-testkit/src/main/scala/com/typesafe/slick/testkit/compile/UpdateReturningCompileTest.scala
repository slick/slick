package com.typesafe.slick.testkit.compile

import slick.jdbc.PostgresProfile.api._

class Users(tag: Tag) extends Table[(Int, String)](tag, "users") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc) 
  def name = column[String]("name")
  def * = (id, name)
}

object UpdateReturningCompileTest {
  val users = TableQuery[Users]
  
  def test(): Unit = {
    // This should compile successfully:
    
    // Basic UPDATE RETURNING
    val updateReturning = users.filter(_.id === 1).map(_.name)
      .updateReturning(users.map(_.id))
      .update("New Name")
    
    // UPDATE RETURNING with into
    val updateReturningInto = users.filter(_.id === 1).map(_.name)
      .updateReturning(users.map(_.id))
      .into((value, id) => (id, value))
      .update("Another Name")
    
    println("UPDATE RETURNING functionality compiles successfully!")
  }
}