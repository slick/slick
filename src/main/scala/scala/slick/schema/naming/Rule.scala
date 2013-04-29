package scala.slick.schema.naming

import scala.slick.SlickException

/**
 * Specifies a rule which is composite of some rules.
 */
case class CompositeRule(keys: List[String]) extends Rule {
  val rules: List[Rule] = keys map (Rule.getRule)
  val rule: Rule = rules.reduce(_ andThen _)
  override def apply(input: String): String = rule(input)

  override def toString(): String = keys.mkString("", " o ", "( )")
}

object Rule {
  object Capitalize extends Rule {
    override def apply(input: String): String = {
      input.capitalize
    }
  }

  object Camelize extends Rule {
    override def apply(input: String): String = {
      val b = new StringBuilder
      var cap = false
      for (c <- input) {
        if (c == '_') cap = true
        else {
          val allowed = if (b.isEmpty) c.isUnicodeIdentifierStart else c.isUnicodeIdentifierPart
          if (allowed) b append (if (cap) c.toUpper else c)
          cap = false
        }
      }
      b.toString
    }
  }

  object Pluralize extends Rule {
    override def apply(input: String): String = {
      val lastChar = input.last
      if (lastChar.toLower == 's')
        input + "es"
      else
        input + "s"
    }
  }

  object Singularize extends Rule {
    override def apply(input: String): String = {
      val lastChar = input.last
      if (lastChar.toLower == 's')
        input.dropRight(1)
      else
        "C" + input
    }
  }

  object LowerCase extends Rule {
    override def apply(input: String): String = {
      input.toLowerCase
    }
  }

  def getRule(key: String): Rule = {
    key.toLowerCase match {
      case "capitalize" => Capitalize
      case "camelize" => Camelize
      case "pluralize" => Pluralize
      case "lowercase" => LowerCase
      case "singularize" => Singularize
      case _ => throw new SlickException(s"Rule with key '$key' could not be found!")
    }
  }
}
