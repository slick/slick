package com.typesafe.slick.testkit.tests

import scala.language.postfixOps
import org.junit.Assert._
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}
import org.junit.Test

/**
 * Created by Dmytro_Kovalskyi on 21.05.2014.
 */
class ScalarFunctionsTest extends TestkitTest[JdbcTestDB] {

  import tdb.profile.simple._

  override val reuseInstance = true

  def testSubstring {
    case class Entity(id: Int, name: String)

    class Entities(tag: Tag) extends Table[Entity](tag, "enities") {
      def id = column[Int]("id", O.PrimaryKey)

      def name = column[String]("name")

      def * = (id, name) <>(Entity.tupled, Entity.unapply)
    }

    val entities = TableQuery[Entities]

    entities.ddl.create
    entities += Entity(1, "Some")
    val names = for (s <- entities) yield s.name.substring(1, 3)
    names.foreach(println(_))
    names.foreach(n => assertEquals("Som", n))
  }
}



