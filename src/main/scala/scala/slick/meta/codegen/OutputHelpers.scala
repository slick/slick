package scala.slick.meta.codegen
import java.io.File
import java.io.BufferedWriter
import java.io.FileWriter
import scala.slick.SlickException

/** Output-related code-generation utilities. */
trait OutputHelpers{
   // generated code
  def code: String

   // implemented by GeneratorHelpers
  def indent(code: String): String

  /** Writes given content to a file */
  def writeStringToFile(content: String, folder:String, pkg: String, fileName: String) {
    val folder2 : String = folder + "/" + (pkg.replace(".","/")) + "/"
    new File(folder2).mkdirs()
    val file = new File( folder2+fileName )
    if (!file.exists()) {
      file.createNewFile();
    }
    val fw = new FileWriter(file.getAbsoluteFile());
    val bw = new BufferedWriter(fw);
    bw.write(content);
    bw.close();
  }

  /**
   * Generates code and writes it to a file.
   * Creates a folder structure for the given package inside the given srcFolder
   * and places the new file inside or overrides the existing one.
   * @param folder target folder, in which the package structure folders are placed
   * @param profile Slick profile that is imported in the generated package (e.g. scala.slick.driver.H2Driver)
   * @param pkg Scala package the generated code is placed in (a subfolder structure will be created within srcFolder)
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   * @param fileName Name of the output file, to which the code will be written
   */
  def writeToFile(profile: String, folder:String, pkg: String, container:String="Tables", fileName: String="Tables.scala") {
    writeStringToFile(packageCode(profile, pkg, container), folder, pkg, fileName)
  }

  /**
   * Generates code providing the data model as trait and object in a Scala package
   * @param profile Slick profile that is imported in the generated package (e.g. scala.slick.driver.H2Driver)
   * @param pkg Scala package the generated code is placed in
   * @param container The name of a trait and an object the generated code will be placed in within the specified package.
   */
    def packageCode(profile: String, pkg: String, container:String="Tables") : String = {
      s"""
package ${pkg}
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */
object ${container} extends {
  val profile = $profile
} with ${container}

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait ${container} {
  val profile: scala.slick.driver.JdbcProfile
  import profile.simple._
  ${indent(code)}
}
      """.trim()
    }
}
