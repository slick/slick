/*
 * see
 * http://svn.apache.org/repos/asf/incubator/clerezza/trunk/scala-scripting/script-engine/src/main/scala/org/apache/clerezza/scala/scripting/BundleContextScalaCompiler.scala
 */
package scala.slick.test.jdbc

import scala.tools.nsc._
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.io.Directory
import scala.tools.nsc.io.Jar.isJarOrZip
import scala.tools.nsc.io.Path
import scala.tools.nsc.io.PlainFile
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util._
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.util.PathResolver
import scala.tools.nsc.backend.JavaPlatform

class EmbeddedScalaCompiler(settings: Settings, reporter: Reporter)
  extends Global(settings, reporter) {

  override lazy val platform: ThisPlatform =
    new { val global: EmbeddedScalaCompiler.this.type = EmbeddedScalaCompiler.this } with JavaPlatform

  lazy val classPathOrig = new PathResolver(settings).result //platform.classPath

  override lazy val classPath: PlatformClassPath = {

    // get the compact classpath value
    val path = System.getProperty("java.class.path");

    // the character : on windows and ; on unixes
    val separator = System.getProperty("path.separator");

    // the character \ on windows and / on unixes
    val fileSep = System.getProperty("file.separator");
    val myUrls = path.split(separator).map(cp => { /*println(cp);*/ new java.io.File(cp).toURI.toURL })

    val classPathAbstractFiles = for (url <- myUrls; if url != null) yield new PlainFile(new java.io.File(url.toURI))

    val classPaths: List[ClassPath[AbstractFile]] = (for (abstractFile <- classPathAbstractFiles) yield {
      new DirectoryClassPath(abstractFile, classPathOrig.context)
    }) toList

    val result = new MergedClassPath[AbstractFile](classPathOrig :: classPaths,
      classPathOrig.context)

//    println("resulting classpath:")
//    result.entries.foreach(println)

    result.asInstanceOf[PlatformClassPath]
  }

  override lazy val rootMirror: Mirror = {

    val rm = new GlobalMirror {
      override lazy val rootLoader: LazyType =
        new loaders.PackageLoader(classPath)
    }
    rm.init()
    rm.asInstanceOf[Mirror]
  }

}


