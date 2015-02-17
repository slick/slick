package slick.codegen

import java.net.URI

import scala.concurrent.{ExecutionContext, Await}
import scala.concurrent.duration.Duration
import slick.backend.DatabaseConfig
import slick.{model => m}
import slick.driver.JdbcProfile
import slick.util.ConfigExtensionMethods.configExtensionMethods

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
object SourceCodeGenerator {

  def run(slickDriver: String, jdbcDriver: String, url: String, outputDir: String, pkg: String, user: Option[String], password: Option[String]): Unit = {
    val driver: JdbcProfile =
      Class.forName(slickDriver + "$").getField("MODULE$").get(null).asInstanceOf[JdbcProfile]
    val dbFactory = driver.api.Database
    val db = dbFactory.forURL(url, driver = jdbcDriver,
      user = user.getOrElse(null), password = password.getOrElse(null), keepAliveConnection = true)
    try {
      val m = Await.result(db.run(driver.createModel(None, false)(ExecutionContext.global).withPinnedSession), Duration.Inf)
      new SourceCodeGenerator(m).writeToFile(slickDriver,outputDir,pkg)
    } finally db.close
  }

  def run(uri: URI, outputDir: Option[String]): Unit = {
    val dc = DatabaseConfig.forURI[JdbcProfile](uri)
    val pkg = dc.config.getString("codegen.package")
    val out = outputDir.getOrElse(dc.config.getStringOr("codegen.outputDir", "."))
    val slickDriver = if(dc.driverIsObject) dc.driverName else "new " + dc.driverName
    try {
      val m = Await.result(dc.db.run(dc.driver.createModel(None, false)(ExecutionContext.global).withPinnedSession), Duration.Inf)
      new SourceCodeGenerator(m).writeToFile(slickDriver, out, pkg)
    } finally dc.db.close
  }

  def main(args: Array[String]): Unit = {
    args.toList match {
      case uri :: Nil =>
        run(new URI(uri), None)
      case uri :: outputDir :: Nil =>
        run(new URI(uri), Some(outputDir))
      case slickDriver :: jdbcDriver :: url :: outputDir :: pkg :: Nil =>
        run(slickDriver, jdbcDriver, url, outputDir, pkg, None, None)
      case slickDriver :: jdbcDriver :: url :: outputDir :: pkg :: user :: password :: Nil =>
        run(slickDriver, jdbcDriver, url, outputDir, pkg, Some(user), Some(password))
      case _ => {
        println("""
            |Usage:
            |  SourceCodeGenerator configURI [outputDir]
            |  SourceCodeGenerator slickDriver jdbcDriver url outputDir pkg [user password]
            |
            |Options:
            |  configURI: A URL pointing to a standard database config file (a fragment is
            |    resolved as a path in the config), or just a fragment used as a path in
            |    application.conf on the class path
            |  slickDriver: Fully qualified name of Slick driver class, e.g. "slick.driver.H2Driver"
            |  jdbcDriver: Fully qualified name of jdbc driver class, e.g. "org.h2.Driver"
            |  url: JDBC URL, e.g. "jdbc:postgresql://localhost/test"
            |  outputDir: Place where the package folder structure should be put
            |  pkg: Scala package the generated code should be places in
            |  user: database connection user name
            |  password: database connection password
            |
            |When using a config file, in addition to the standard config parameters from
            |slick.backend.DatabaseConfig you can set "codegen.package" and
            |"codegen.outputDir". The latter can be overridden on the command line.
          """.stripMargin.trim)
        System.exit(1)
      }
    }
  }
}
