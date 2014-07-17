package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.model._
import scala.slick.ast.ColumnOption
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.meta
import com.typesafe.slick.testkit.util.{JdbcTestDB, TestkitTest}

class MetaModelTest extends TestkitTest[JdbcTestDB] {
  import tdb.profile.simple._

  def test { ifCap(jcap.createModel){
    class Categories(tag: Tag) extends Table[(Int, String)](tag, "categories") {
      def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
      def name = column[String]("name", O.DBType("VARCHAR(123)"))
      def * = (id, name)
      def idx = index("IDX_NAME",name)
    }
    val categories = TableQuery[Categories]

    class Posts(tag: Tag) extends Table[(Int, String, Option[Int], Boolean, String)](tag, "posts") {
      def id = column[Int]("id")
      def title = column[String]("title",O.Length(99,varying=false)) // tests Length produces valid SQL
      def category = column[Option[Int]]("category",O.Default(Some(531)))
      def someBool = column[Boolean]("some_bool",O.Default(true)) // tests boolean default values parsing
      def someString = column[String]("some_string",O.Length(111,varying=true)) // tests Length produces valid SQL
      def * = (id, title, category, someBool, someString)
      def pk = primaryKey("posts_pk", (id,title))
      def categoryFK = foreignKey("category_fk", category, categories)(_.id)
    }
    val posts = TableQuery[Posts]

    class DefaultTest(tag: Tag) extends Table[(Boolean, Boolean, Boolean, Option[Boolean], Option[Boolean], Option[Boolean], String, String, String, Option[String], Option[String], Option[String], Option[String])](tag, "default_test") {
      def someBool = column[Boolean]("some_bool")
      def someBoolDefaultTrue = column[Boolean]("some_bool_default_true",O.Default(true))
      def someBoolDefaultFalse = column[Boolean]("some_bool_default_false",O.Default(false))
      def someBoolOption = column[Option[Boolean]]("some_bool_option")
      def someBoolOptionDefaultSome = column[Option[Boolean]]("some_bool_option_default_some",O.Default(Some(true)))
      def someBoolOptionDefaultNone = column[Option[Boolean]]("some_bool_option_default_none",O.Default(None))
      def someString = column[String]("some_string")
      def someStringDefaultNonEmpty = column[String]("some_string_default_non_empty",O.Default("bar"))
      def someStringDefaultEmpty = column[String]("some_string_default_empty",O.Default(""))
      def someStringOption = column[Option[String]]("some_string_option")
      def someStringOptionDefaultEmpty = column[Option[String]]("some_string_option_default_empty",O.Default(Some("")))
      def someStringOptionDefaultNone = column[Option[String]]("some_string_option_default_none",O.Default(None))
      def someStringOptionDefaultNonEmpty = column[Option[String]]("some_string_option_default_non_empty",O.Default(Some("foo")))
      def * = (someBool,someBoolDefaultTrue,someBoolDefaultFalse,someBoolOption,someBoolOptionDefaultSome,someBoolOptionDefaultNone,someString,someStringDefaultNonEmpty,someStringDefaultEmpty,someStringOption,someStringOptionDefaultEmpty,someStringOptionDefaultNonEmpty,someStringOptionDefaultNone)
    }
    val defaultTest = TableQuery[DefaultTest]

    val ddl = posts.ddl ++ categories.ddl ++ defaultTest.ddl
    ddl.create
    //println(ddl.createStatements.toList.toString)
    import tdb.profile.createModel
    createModel(ignoreInvalidDefaults=false).assertConsistency
    val tables = tdb.profile.defaultTables
    createModel(Some(tables), ignoreInvalidDefaults = false).assertConsistency
    ;{
      // checks that createModel filters out foreign keys pointing out
      val model = createModel(
        Some(tables.filter(_.name.name.toUpperCase=="POSTS")),
        ignoreInvalidDefaults = false
      )
      model.assertConsistency
      assertEquals( 0, model.tables.map(_.foreignKeys.size).sum )
    }
    createModel(
      Some(tables.filter(_.name.name.toUpperCase=="CATEGORIES")),
      ignoreInvalidDefaults = false
    ).assertConsistency
    try{
      // checks that assertConsistency fails when manually feeding the model with inconsistent tables
      Model(
        createModel(Some(tables), ignoreInvalidDefaults = false).tables.filter(_.name.table.toUpperCase=="POSTS")
      ).assertConsistency
      fail("Consistency assertion should have failed")
    } catch {
      case _:AssertionError => 
    }

    // postgres uses lower case and things like int4
    // seen in jtds: int identity
    // seen in oracle: VARCHAR2
    val DBTypePattern = "^[a-zA-Z][a-zA-Z0-9 ]*$".r

    // check that the model matches the table classes
    val model = tdb.profile.createModel(ignoreInvalidDefaults=false)
    assertEquals( model.tables.toString, 3, model.tables.size )
    ;{
      val categories = model.tables.filter(_.name.table.toUpperCase=="CATEGORIES").head
      assertEquals( 2, categories.columns.size )
      assertEquals( None, categories.primaryKey )
      assertEquals( 0, categories.foreignKeys.size )
      assertEquals( List("id"), categories.columns.filter(_.options.exists(_ == ColumnOption.PrimaryKey)).map(_.name).toList )
      assertEquals(
        (123,true),
        categories.columns.filter(_.name == "name").head
                  .options.collect{case ColumnOption.Length(length,varying) => (length,varying)}.head
      )
      //assertEquals( categories.indices.toString, 1, categories.indices.size ) // Removed until made sure all dbs actually produce indices model
      //assertEquals( "IDX_NAME", categories.indices.head.name.get.toUpperCase )
      categories.columns.foreach{
        _.options.foreach{
          case ColumnOption.Length(length,varying) => length < 256
          case ColumnOption.DBType(DBTypePattern()) => 
          case ColumnOption.DBType(dbType) => assert(false, "invalid DBType: "+dbType)
          case _ =>
        }
      }
    }
    ;{
      val posts = model.tables.filter(_.name.table.toUpperCase=="POSTS").head
      assertEquals( 5, posts.columns.size )
      assertEquals( posts.indices.toString, 0, posts.indices.size )
      assertEquals( 2, posts.primaryKey.get.columns.size )
      assertEquals( 1, posts.foreignKeys.size )
      if(tdb.profile != slick.driver.SQLiteDriver){
        assertEquals( "CATEGORY_FK", posts.foreignKeys.head.name.get.toUpperCase )
      }
      def tpe(col:String) = posts.columns.filter(_.name == col).head
               .options.collect{case ColumnOption.DBType(tpe) => tpe}.head
      assert(
        Seq(
          "CHAR","CHARACTER",
          "BPCHAR" // bpchar: postgres
          //"char" // jtds
        ) contains tpe("title").toUpperCase,
        tpe("title")
      )
      assert(
        Seq(
          "VARCHAR",
          "VARCHAR2" // oracle
        ) contains tpe("some_string").toUpperCase,
        tpe("title")
      )
      assertEquals(
        (99,false),
        posts.columns.filter(_.name == "title").head
             .options.collect{case ColumnOption.Length(length,varying) => (length,varying)}.head
      )
      assertEquals(
        (111,true),
        posts.columns.filter(_.name == "some_string").head
             .options.collect{case ColumnOption.Length(length,varying) => (length,varying)}.head
      )
      assert( !posts.columns.exists(_.options.exists(_ == ColumnOption.PrimaryKey)) )
      posts.columns.foreach{
        _.options.foreach{
          case ColumnOption.Length(length,varying) => length < 256
          case ColumnOption.DBType(DBTypePattern()) => 
          case ColumnOption.DBType(dbType) => assert(false, "invalid DBType: "+dbType)
          case _ =>
        }
      }
    };{
      val defaultTest = model.tables.filter(_.name.table.toUpperCase=="DEFAULT_TEST").head
      assertEquals(None,defaultTest.name.schema)
      assert(Some("PUBLIC") != defaultTest.name.catalog.map(_.toUpperCase))
      ifCap(jcap.defaultValueMetaData){
        def column(name: String)
          = defaultTest.columns.filter(_.name == name).head
        def columnDefault(name: String)
          = column(name)
             .options.collect{case ColumnOption.Default(v) => v}
             .headOption
        assertEquals(None, columnDefault("some_bool"))
        ifCap(jcap.booleanMetaData){
          assertEquals(Some(true), columnDefault("some_bool_default_true"))
        }
        ifNotCap(jcap.booleanMetaData){
          assertEquals(Some(1), columnDefault("some_bool_default_true"))
        }
        ifCap(jcap.booleanMetaData){
          assertEquals(Some(false), columnDefault("some_bool_default_false"))
        }
        ifNotCap(jcap.booleanMetaData){
          assertEquals(Some(0), columnDefault("some_bool_default_false"))
        }
        ifCap(jcap.nullableNoDefault){
          assertEquals(None,columnDefault("some_bool_option"))
        }
        ifNotCap(jcap.nullableNoDefault){
          assertEquals(Some(None),columnDefault("some_bool_option"))
        }
        ifCap(jcap.booleanMetaData){
          assertEquals(Some(Some(true)), columnDefault("some_bool_option_default_some"))
        }
        ifNotCap(jcap.booleanMetaData){
          assertEquals(Some(Some(1)), columnDefault("some_bool_option_default_some"))
        }
        assertEquals(Some(None),columnDefault("some_bool_option_default_none"))
        assertEquals(None,columnDefault("some_string"))
        assertEquals(Some("bar"),columnDefault("some_string_default_non_empty"))
        assertEquals(Some(""),columnDefault("some_string_default_empty"))
        ifCap(jcap.nullableNoDefault){
          assertEquals(None,columnDefault("some_string_option"))
        }
        ifNotCap(jcap.nullableNoDefault){
          assertEquals(Some(None),columnDefault("some_string_option"))
        }
        assertEquals(Some(Some("")),columnDefault("some_string_option_default_empty"))
        assertEquals(Some(None),columnDefault("some_string_option_default_none"))
        assertEquals(Some(Some("foo")),columnDefault("some_string_option_default_non_empty"))
      }
    }
  }}
}
