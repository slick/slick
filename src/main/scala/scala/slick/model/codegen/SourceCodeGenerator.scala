package scala.slick.model.codegen

import scala.slick.{model => m}

/**
 * A customizable code generator for working with Slick.
 *
 * Parts of the generator were explained in our <a href="http://slick.typesafe.com/docs/#20131203_patterns_for_slick_database_applications_at_scala_exchange_2013">talk at Scala eXchange 2013</a>.
 *
 * By default it generates Table classes, corresponding TableQuery values which
 * can be used in a collection-like manner and case classes for holding complete
 * rows of values. For Tables > 22 columns the generator automatically switches
 * to Slick HLists for overcoming Scala's tuple size limit.
 *
 * The implementation is ready for practical use, but since it is new in
 * Slick 2.0 we consider it experimental and reserve the right to remove features
 * without a deprecation cycle if we think that it is necessary. It would be only
 * a small effort to run an old generator against a future version of Slick though,
 * if necessary, as it's implementation is rather isolated from the rest of Slick.
 * 
 * The generator can be flexibly customized by overriding methods to programmatically
 * generate any code based on the data model. This can be used for minor customizations
 * as well as heavy, model driven code generation, e.g. for framework bindings (Play,...)
 * repetetive sections of applications, etc.
 *
 * The implementation is structured into a small hierarchy of sub-generators responsible
 * for different fragments of the complete output. The implementation of each
 * sub-generator can be swapped out for a customized one by overriding the corresponding
 * factory method. SourceCodeGenerator contains a factory method Table, which it uses to
 * generate a sub-generator for each table. The sub-generator Table in turn contains
 * sub-generators for Table classes, entity case classes, columns, key, indices, etc.
 * Custom sub-generators can easily be added as well.
 *
 * Within the sub-generators the relevant part of the Slick data `model` can
 * be accessed to drive the code generation.
 * 
 * Here is an example for customizing the generator:
 * {{{
    import scala.slick.jdbc.model.createModel
    // fetch data model
    val model = db.withSession{ implicit session =>
     createModel(H2Driver.getTables.list.filter(...),H2Driver)
    }
    // customize code generator
    val codegen = new SourceCodeGenerator(model){
      // override mapped table and class name
      override def entityName = dbTableName => dbTableName.dropRight(1).toLowerCase.toCamelCase
      override def tableName  = dbTableName => dbTableName.toLowerCase.toCamelCase
    
      // add some custom import
      override def code = "import foo.{MyCustomType,MyCustomTypeMapper}" + "\n" + super.code
    
      // override table generator
      override def Table = new Table(_){
        // disable entity class generation and mapping
        override def EntityType = new EntityType{
          override def classEnabled = false
        }
    
        // override contained column generator
        override def Column = new Column(_){
          // use the data model member of this column to change the Scala type, e.g. to a custom enum or anything else
          override def rawType = if(model.name == "SOME_SPECIAL_COLUMN_NAME") "MyCustomType" else super.rawType
        }
      }
    }
    codegen.writeToFile("scala.slick.driver.H2Driver","some/folder/","some.packag","Tables","Tables.scala")
 * }}}
 *
 * Of coures it makes sense to integrate this into your build process.
 * @param model Slick data model for which code should be generated.
 */
class SourceCodeGenerator(model: m.Model)
                   extends AbstractSourceCodeGenerator(model) with OutputHelpers{
  // "Tying the knot": making virtual classes concrete
  type Table = TableDef
  def Table = new TableDef(_)
  class TableDef(model: m.Table) extends super.TableDef(model){
    // Using defs instead of (caching) lazy vals here to provide consitent interface to the user.
    // Performance should really not be critical in the code generator. Models shouldn't be huge.
    // Also lazy vals don't inherit docs from defs
    type EntityType     =     EntityTypeDef
    def  EntityType     = new EntityType{}
    type PlainSqlMapper =     PlainSqlMapperDef
    def  PlainSqlMapper = new PlainSqlMapper{}
    type TableClass     =     TableClassDef
    def  TableClass     = new TableClass{}
    type TableValue     =     TableValueDef
    def  TableValue     = new TableValue{}
    type Column         =     ColumnDef
    def  Column         = new Column(_)
    type PrimaryKey     =     PrimaryKeyDef
    def  PrimaryKey     = new PrimaryKey(_)
    type ForeignKey     =     ForeignKeyDef  
    def  ForeignKey     = new ForeignKey(_)
    type Index          =     IndexDef  
    def  Index          = new Index(_)
  }
}

/** A runnable class to execute the code generator without further setup */
object SourceCodeGenerator{
  import scala.slick.driver.JdbcProfile
  import scala.reflect.runtime.currentMirror
  def main(args: Array[String]) = {
    args.toList match {
      case List(slickDriver, jdbcDriver, url, outputFolder, pkg) => {
        val driver: JdbcProfile = {
          val module = currentMirror.staticModule(slickDriver)
          val reflectedModule = currentMirror.reflectModule(module)
          val driver = reflectedModule.instance.asInstanceOf[JdbcProfile]
          driver
        }
        driver.simple.Database
          .forURL(url, driver = jdbcDriver)
          .withSession{ implicit session =>
            (new SourceCodeGenerator(driver.createModel)).writeToFile(slickDriver,outputFolder,pkg)
          }
      }
      case _ => {
        println("""
Usage: SourceCodeGenerator.main(Array( slickDriver, jdbcDriver, url, outputFolder, pkg ))

slickDriver: Fully qualified name to Slick driver class, e.g. "scala.slick.driver.H2Driver"

jdbcDriver: Fully qualified name to Slick driver class, e.g. "org.h2.Driver"

url: jdbc url, e.g. "jdbc:postgresql://localhost/test"

outputFolder: Place where the package folder structure is put.

pkg: Scala package, the generated code should be places in
            """.trim
        )
      }
    }
  }
}
