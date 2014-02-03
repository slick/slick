package scala.slick.model.codegen

import scala.slick.{model => m}

/**
 * A customizable code generator for working with Slick.
 *
 * For usage information please see the corresponding part of the Slick documentation.
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
      case slickDriver :: jdbcDriver :: url :: outputFolder :: pkg :: tail if tail.size == 0 || tail.size == 2 => {
        val driver: JdbcProfile = {
          val module = currentMirror.staticModule(slickDriver)
          val reflectedModule = currentMirror.reflectModule(module)
          val driver = reflectedModule.instance.asInstanceOf[JdbcProfile]
          driver
        }
        val db = driver.simple.Database
        (tail match{
          case user :: password :: Nil => db.forURL(url, driver = jdbcDriver, user=user, password=password)
          case Nil => db.forURL(url, driver = jdbcDriver)
          case _ => throw new Exception("This should never happen.")
        }).withSession{ implicit session =>
          (new SourceCodeGenerator(driver.createModel)).writeToFile(slickDriver,outputFolder,pkg)
        }
      }
      case _ => {
        println("""
Usage:
  SourceCodeGenerator.main(Array(slickDriver, jdbcDriver, url, outputFolder, pkg))
  SourceCodeGenerator.main(Array(slickDriver, jdbcDriver, url, outputFolder, pkg, user, password))

slickDriver: Fully qualified name of Slick driver class, e.g. "scala.slick.driver.H2Driver"

jdbcDriver: Fully qualified name of jdbc driver class, e.g. "org.h2.Driver"

url: jdbc url, e.g. "jdbc:postgresql://localhost/test"

outputFolder: Place where the package folder structure should be put

pkg: Scala package the generated code should be places in

user: database connection user name

password: database connection password
            """.trim
        )
      }
    }
  }
}
