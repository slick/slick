package com.typesafe.slick.testkit.demo

import slick.jdbc.PostgresProfile.api._

/** 
 * Demo showing UPDATE ... RETURNING functionality.
 * This is similar to INSERT ... RETURNING but for UPDATE statements.
 */
object UpdateReturningDemo {
  
  class Users(tag: Tag) extends Table[(Int, String, String)](tag, "demo_users") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def firstName = column[String]("first_name")
    def lastName = column[String]("last_name") 
    def * = (id, firstName, lastName)
  }

  val users = TableQuery[Users]

  def demos(): Unit = {
    
    // Example 1: Basic UPDATE RETURNING - return the ID of updated row
    val updateWithId = users.filter(_.id === 1).map(u => (u.firstName, u.lastName))
      .updateReturning(users.map(_.id))
      .update(("John", "Smith"))
    // Type: ProfileAction[Option[Int], NoStream, Effect.Write]
    // Returns: Some(1) if row was updated, None if no row matched

    // Example 2: UPDATE RETURNING with multiple columns
    val updateWithMultipleColumns = users.filter(_.id === 2).map(_.firstName)
      .updateReturning(users.map(u => (u.id, u.lastName)))
      .update("Jane")
    // Type: ProfileAction[Option[(Int, String)], NoStream, Effect.Write]
    // Returns: Some((2, "originalLastName")) if updated, None if no match

    // Example 3: UPDATE RETURNING with into transformation
    val updateWithTransformation = users.filter(_.id === 3).map(_.firstName)
      .updateReturning(users.map(u => (u.id, u.lastName)))
      .into((newFirstName, returned) => s"Updated user ${returned._1}: ${newFirstName} ${returned._2}")
      .update("Alice")
    // Type: ProfileAction[Option[String], NoStream, Effect.Write]
    // Returns: Some("Updated user 3: Alice originalLastName") if updated

    // Example 4: UPDATE RETURNING the entire updated row
    val updateReturningWholeRow = users.filter(_.id === 4).map(_.firstName)
      .updateReturning(users)
      .update("Bob")
    // Type: ProfileAction[Option[(Int, String, String)], NoStream, Effect.Write]
    // Returns: Some((4, "Bob", "originalLastName")) if updated

    println("UPDATE RETURNING examples compiled successfully!")
    println("These can be executed with: db.run(updateWithId)")
    println("UPDATE RETURNING provides type-safe column retrieval from updated rows")
    println("Only works with databases that support UPDATE ... RETURNING (PostgreSQL, etc.)")
  }
}