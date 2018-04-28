package slick.util

import java.sql.PreparedStatement
import scala.collection.mutable.ArrayBuffer

final class SQLBuilder { self =>
  import SQLBuilder._

  private val sb = new StringBuilder(128)
  private val setters = new ArrayBuffer[Setter]
  private var currentIndentLevel: Int = 0

  def +=(s: String) = { sb append s; this }

  def +=(c: Char) = { sb append c; this }

  def +?=(f: Setter) = { setters append f; sb append '?'; this }

  def sep[T](sequence: Traversable[T], separator: String)(f: T => Unit): Unit = {
    var first = true
    for(x <- sequence) {
      if(first) first = false else self += separator
      f(x)
    }
  }

  def sep[T](sequence: ConstArray[T], separator: String)(f: T => Unit): Unit = {
    var i = 0
    while(i < sequence.length) {
      if(i != 0) self += separator
      f(sequence(i))
      i += 1
    }
  }

  def build = Result(sb.toString, { (p: PreparedStatement, idx: Int, param: Any) =>
    var i = idx
    for(s <- setters) {
      s(p, i, param)
      i += 1
    }
  })

  def newLineIndent(): Unit = {
    currentIndentLevel += 1
    newLine()
  }

  def newLineDedent(): Unit = {
    currentIndentLevel -= 1
    newLine()
  }

  def newLineOrSpace(): Unit =
    if(GlobalConfig.sqlIndent) newLine() else this += " "

  private def newLine(): Unit = if(GlobalConfig.sqlIndent) {
    this += "\n"
    if (1 <= currentIndentLevel) 1.to(currentIndentLevel).foreach(_ => this += "  ")
  }
}

object SQLBuilder {
  final type Setter = ((PreparedStatement, Int, Any) => Unit)

  final case class Result(sql: String, setter: Setter)
}
