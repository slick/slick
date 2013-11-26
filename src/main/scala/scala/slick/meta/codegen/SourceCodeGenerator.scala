package scala.slick.meta.codegen

import scala.slick.{meta => m}

/**
 * A customizable code generator for working with Slick.
 *
 * By default it generates Table classes, corresponding TableQuery values which
 * can be used in a collection-like manner and case classes for holding complete
 * rows of values.
 * 
 * The uses range from minor customizations, to model driven application code generation.
 *
 * The generator can be flexibly customized by overriding methods. There is a tiny hierachy
 * involved. A few methods are contained directly. One of them is Table, the
 * factory method for Table generator classes. By overriding it and returning
 * a customized Table subclass, the table-specific generation can be customized.
 * In the same manner, Table contains factory methods Column, PrimaryKey,
 * ForeignKey and Index, which can be overriden to return customized
 * subclasses. The final methods are logically bound to the model, which can
 * be modified before handing it to the code generator.
 *
 * Within the classes the relevant part of the Slick meta model can
 * be accessed to drive the code generation. (In some cases it may be usedful
 * to fetch additional meta data from jdbc.)
 * 
 * Here is an example:
 * ----------
 * import scala.slick.jdbc.meta.createMetaModel
 * // fetch meta model
 * val model = db.withSession{ implicit session =>
 *   createMetaModel(H2Driver.getTables.list.filter(...),H2Driver)
 * }
 * // customize code generator
 * val codegen = new SourceCodeGenerator(model){
 *   // override mapped table and class name
 *   override def entityName = dbTableName => dbTableName.dropRight(1).toLowerCase.toCamelCase
 *   override def tableName  = dbTableName => dbTableName.toLowerCase.toCamelCase // <- thats actually already the default
 *
 *   // add some custom import
 *   override def code = "import foo.{MyCustomType,MyCustomTypeMapper}" + "\n" + super.code
 *
 *   // override table generator
 *   override def Table = new Table(_){
 *     // disable entity class generation and mapping
 *     override def entityClassEnabled = false
 *
 *     // override contained column generator
 *     override def Column = new Column(_){
 *       // use the meta data (a member) of this column to change the Scala type, e.g. to a custom enum or anything else
 *       override def rawType = if(meta.name == "SOME_SPECIAL_COLUMN_NAME") "MyCustomType" else super.rawType
 *
 *     }
 *   }
 * }
 * codegen.writeToFile("scala.slick.driver.H2Driver","some/folder/","some.packag","Tables","Tables.scala")
 * ----------
 *
 * Of coures it makes sense to integrate this into your build process.
 *
 * @param meta Slick meta model for which code should be generated.
 */
class SourceCodeGenerator(model: m.Model)
                   extends AbstractSourceCodeGenerator(model) with OutputHelpers{
  // "Tying the knot": making virtual class concrete
  type Table = TableDef
  def Table = new TableDef(_)
  class TableDef(meta: m.Table) extends super.TableDef(meta)
                                with DefaultColumn
                                with DefaultPrimaryKey
                                with DefaultForeignKey
                                with DefaultIndex
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
            (new SourceCodeGenerator(driver.metaModel)).writeToFile(slickDriver,outputFolder,pkg)
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
