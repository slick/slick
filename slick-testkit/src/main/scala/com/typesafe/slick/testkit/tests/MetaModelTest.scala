package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.meta._
import scala.slick.ast.ColumnOption
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.meta
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MetaModelTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test {
    class Categories(tag: Tag) extends Table[(Int, String)](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name")
      def * = (id, name)
      def idx = index("IDX_NAME",name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int])](tag, "posts") {
      def id = column[Int]("id")
      def title = column[String]("title")
      def category = column[Option[Int]]("category")
      def * = (id, title, category)
      def pk = primaryKey("posts_pk", (id,title))
      def categoryFK = foreignKey("category_fk", category, categories)(_.id)
    }
    val posts = TableQuery[Posts]

    val ddl = posts.ddl ++ categories.ddl
    ddl.create
    tdb.profile.metaModel.assertConsistency
    val tables = tdb.profile.getTables.list
    def createMetaModel(tables:Seq[MTable]): Model = meta.createMetaModel(tables,tdb.profile)
    createMetaModel(tables).assertConsistency
    ;{
      // checks that createMetaModel filters out foreign keys pointing out
      val model = createMetaModel(tables.filter(_.name.name.toUpperCase=="POSTS"))
      model.assertConsistency
      assertEquals( 0, model.tables.map(_.foreignKeys.size).sum )
    }
    createMetaModel(tables.filter(_.name.name.toUpperCase=="CATEGORIES")).assertConsistency
    try{
      // checks that assertConsistency fails when manually feeding the meta model with inconsistent meta model tables
      Model( createMetaModel(tables).tables.filter(_.name.table.toUpperCase=="POSTS") ).assertConsistency
      fail("Consistency assertion should have failed")
    } catch {
      case _:AssertionError => 
    }

    // check that the meta model matches the table classes
    val model = tdb.profile.metaModel
    assertEquals( model.tables.toString, 2, model.tables.size )
    ;{
      val categories = model.tables.filter(_.name.table.toUpperCase=="CATEGORIES").head
      assertEquals( 2, categories.columns.size )
      assertEquals( categories.indices.toString, 1, categories.indices.size )
      assertEquals( None, categories.primaryKey )
      assertEquals( 0, categories.foreignKeys.size )
      assertEquals( "IDX_NAME", categories.indices.head.name.get.toUpperCase )
      assertEquals( List("id"), categories.columns.filter(_.options.exists(_ == ColumnOption.PrimaryKey)).map(_.name).toList )
    }
    ;{
      val posts = model.tables.filter(_.name.table.toUpperCase=="POSTS").head
      assertEquals( 3, posts.columns.size )
      assertEquals( posts.indices.toString, 0, posts.indices.size )
      assertEquals( 2, posts.primaryKey.get.columns.size )
      assertEquals( 1, posts.foreignKeys.size )
      if(tdb.profile != slick.driver.SQLiteDriver){
        assertEquals( "CATEGORY_FK", posts.foreignKeys.head.name.get.toUpperCase )
      }
      assert( !posts.columns.exists(_.options.exists(_ == ColumnOption.PrimaryKey)) )
    }
  }
}
