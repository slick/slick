package scala.slick
package object util{
  /** Slick String extension methods */
  implicit class StringExtensionMethods(val str: String) extends AnyVal{
    /** Lowercases the first character */
    final def uncapitalize: String = str(0).toString.toLowerCase + str.tail

    /**
     * Capitalizes the first character of each word separated by one or more '_'. Lower cases all other characters. 
     * Removes one '_' from each sequence of one or more subsequent '_' (to avoid collision).
     */
    final def toCamelCase: String
      = str.toLowerCase
           .split("_")
           .map{ case "" => "_" case s => s } // avoid possible collisions caused by multiple '_'
           .map(_.capitalize)
           .mkString("")
  }
}
