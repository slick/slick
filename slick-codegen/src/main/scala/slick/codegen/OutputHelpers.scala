package slick.codegen
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter

/** Output-related code-generation utilities. */
trait OutputHelpers{
  def code: String

  /** The parent type of the generated main trait. This can be overridden in subclasses. */
  def parentType: Option[String] = None

  /**
   * The generated code stored in a map that associates the scala filename with the generated code (this map contains one entry per table).
   */
  def codePerTable: Map[String, String]

  /**
    * Foreign keys used for mapping a minimal set of dependencies between tables.
  */
  def foreignKeysPerTable: Map[String, List[String]]

  /**
    * The generated code used to generate the container class.
    */
  def codeForContainer: String

  /** Indents all but the first line of the given string
   *  No indent is added to empty lines.
   */
  def indent(code: String): String

  /** Writes given content to a file.
   *  Ensures the file ends with a newline character.
   *  @group Output
   */
  def writeStringToFile(content: String, folder: String, pkg: String, fileName: String): Unit = {
    val folder2: String = folder + "/" + pkg.replace(".", "/") + "/"
    new File(folder2).mkdirs()
    val file = new File(folder2 + fileName)
    if (!file.exists()) {
      file.createNewFile()
    }
    file.setWritable(true)
    val fw = new FileWriter(file.getAbsoluteFile());
    val bw = new BufferedWriter(fw)
    bw.write(content)
    if (!content.endsWith("\n")) bw.write("\n")
    bw.close()
    file.setWritable(false)
  }

  /**
   * Generates code and writes it to a file.
   * Creates a folder structure for the given package inside the given srcFolder
   * and places the new file inside or overrides the existing one.
   * @group Output
   * @param folder target folder, in which the package structure folders are placed
   * @param profile Slick profile that is imported in the generated package (e.g. slick.jdbc.H2Profile)
   * @param pkg Scala package the generated code is placed in (a subfolder structure will be created within srcFolder)
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   * @param fileName Name of the output file, to which the code will be written
   */
  def writeToFile(profile: String, folder:String, pkg: String, container:String="Tables", fileName: String="Tables.scala"): Unit = {
    writeStringToFile(packageCode(profile, pkg, container, parentType), folder, pkg, fileName)
  }

   /**
   * Generates code and writes it to multiple files.
   * Creates a folder structure for the given package inside the given srcFolder
   * and places the new files inside or overrides the existing one.
   * @group Output
   * @param folder target folder, in which the output files are placed
   * @param profile Slick profile that is imported in the generated package (e.g. scala.slick.driver.H2Driver)
   * @param pkg Scala package the generated code is placed in (a subfolder structure will be created within srcFolder)
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   */
  def writeToMultipleFiles(profile: String, folder: String, pkg: String, container: String = "Tables"): Unit = {
    // Write the container file (the file that contains the stand-alone object).
    writeStringToFile(rootTraitCode(profile, pkg, container), folder, pkg, container + "Root.scala")
    writeStringToFile(packageContainerCode(profile, pkg, container), folder, pkg, container + ".scala")
    // Write one file for each table.
    codePerTable.foreach {
      case (tableName, tableCode) => writeStringToFile(packageTableCode(tableName, tableCode, pkg, container), folder, pkg, handleQuotedNamed(tableName)+".scala")
    }
  }

  private def handleQuotedNamed(tableName: String) = {
    if (tableName.endsWith("`")) s"${tableName.init}Table`" else s"${tableName}Table"
  }

  def rootTraitCode(profile: String, pkg: String, container: String = "Tables"): String = {
  s"""
package ${pkg}
// AUTO-GENERATED Slick data model

trait ${container}Root {
  val profile: slick.jdbc.JdbcProfile
}
"""
  }

  /**
   * Generates code providing the data model as trait and object in a Scala package
   * @group Basic customization overrides
   * @param profile Slick profile that is imported in the generated package (e.g. slick.jdbc.H2Profile)
   * @param pkg Scala package the generated code is placed in
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   */
  def packageCode(profile: String, pkg: String, container: String, parentType: Option[String]) : String = {
      s"""
package ${pkg}
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */

object ${container} extends ${container}Profile with ${container}

trait ${container}Profile {
  val profile: slick.jdbc.JdbcProfile = $profile
}

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait ${container}${parentType.map(t => s" extends $t").getOrElse("")} {
  val profile: slick.jdbc.JdbcProfile
  import profile.api._
  ${indent(code)}
}
      """.trim()
    }

  /**
   * Generates code providing the stand-alone slick data model for immediate use.
   * @group Basic customization overrides
   * @param profile Slick profile that is imported in the generated package (e.g. scala.slick.driver.H2Driver)
   * @param pkg Scala package the generated code is placed in
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   */
  def packageContainerCode(profile: String, pkg: String, container: String = "Tables"): String = {
    val tableTraits = parentType.toSeq ++ codePerTable.keys.map(tableName => s"${handleQuotedNamed(tableName) }")
    val allTraits = s"${container}Root" :: tableTraits.toList
    val mixinCode = allTraits.mkString("extends ", " with ", "")
    s"""
package ${pkg}
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */
object ${container} extends ${container}Profile with ${container}

trait ${container}Profile {
  val profile: slick.jdbc.JdbcProfile = $profile
}

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.)
    Each generated XXXXTable trait is mixed in this trait hence allowing access to all the TableQuery lazy vals.
  */
trait ${container} ${mixinCode} {
  val profile: slick.jdbc.JdbcProfile
  import profile.api._
  ${indent(codeForContainer)}

}
      """.trim()
  }

  /**
   * Generates code for the given table. The tableName and tableCode parameters should come from the #codePerTable map.
   * @group Basic customization overrides
   * @param tableName : the name of the table
   * @param tableCode : the generated code for the table.
   * @param pkg Scala package the generated code is placed in
   * @param container The name of the container
   */
  def packageTableCode(tableName: String, tableCode: String, pkg: String, container: String): String = {

    val foreignKeys = foreignKeysPerTable.getOrElse(tableName, Nil).map(handleQuotedNamed)
    val selfTraits = s"${container}Root" :: foreignKeys

    s"""
package ${pkg}
// AUTO-GENERATED Slick data model for table ${tableName}
trait ${handleQuotedNamed(tableName) } {

  self:${selfTraits.mkString(" with ")}  =>

  import profile.api._
  ${indent(tableCode)}
}
      """.trim()
  }
}
