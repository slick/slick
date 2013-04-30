package scala.slick

package object typeproviders {
  type ContextUtils = {
    def freshName(name: String): String
  }
}