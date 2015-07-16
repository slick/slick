package slick.util

/**
 * Utilities for working with classloaders
 */
object ClassLoaderUtil {

  /** Get the default classloader to use to dynamically load classes. */
  val defaultClassLoader: ClassLoader = {
    new ClassLoader(this.getClass.getClassLoader) {
      override def loadClass(name: String): Class[_] = {
        try {
          // Try the context classloader first. But, during macro compilation, it's probably wrong, so fallback to this
          // classloader.
          Thread.currentThread().getContextClassLoader.loadClass(name)
        } catch {
          case e: ClassNotFoundException => super.loadClass(name)
        }
      }
    }
  }
}
