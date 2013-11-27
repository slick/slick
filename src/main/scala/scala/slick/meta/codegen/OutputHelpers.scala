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
   * @param driver Slick driver that is imported in the generated package
   * @param pkg Scala package the generated code is placed in (a subfolder structure will be created within srcFolder)
   * @param obj The name of an object the generated code will be placed in within the specified package. (It inherits from a class with the same name which can be customized if needed.)
   * @param fileName Name of the output file, to which the code will be written
   */
  def writeToFile(driver: String, folder:String, pkg: String, obj:String="Tables", fileName: String="Tables.scala") {
    writeStringToFile(packageCode(driver, pkg, obj), folder, pkg, fileName)
  }

  /**
   * Generate code wrapped in a Scala package
   * @param driver Slick driver that is imported in the generated package
   * @param pkg Scala package the generated code is placed in
   * @param obj The name of an object the generated code will be placed in within the specified package. (It inherits from a class with the same name which can be customized if needed.)
   */
    def packageCode(driver: String, pkg: String, obj:String="Tables") : String = {
      s"""
package ${pkg}
/** AUTO-GENERATED FILE */
object ${obj} extends ${obj}
class ${obj}{
  /** Auto-generated Slick classes for working with the corresponding database tables */
  import $driver.simple._
  ${indent(code)}
}
      """.trim()
    }	
}
