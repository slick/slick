package scala.slick.schema

package object naming {
  /**
   * Specifies a Rule for naming, which gets a name, and applies that
   * rule over it, and returns the result string.
   */
  type Rule = (String => String)
}

