package org.scalaquery.util

import scala.collection.mutable.ArrayBuffer
import org.scalaquery.session.PositionedParameters

final class SQLBuilder extends SQLBuilder.Segment { self =>
  import SQLBuilder._

  private val segments = new ArrayBuffer[Segment]
  private var currentStringSegment: StringSegment = null

  private def ss = {
    if(currentStringSegment eq null) {
      if(segments.isEmpty || segments.last.isInstanceOf[SQLBuilder]) {
        currentStringSegment = new StringSegment
        segments += currentStringSegment
      }
      else currentStringSegment = segments.last.asInstanceOf[StringSegment]
    }
    currentStringSegment
  }

  def +=(s: String) = { ss.sb append s; this }

  def +=(i: Int) = { ss.sb append i; this }

  def +=(l: Long) = { ss.sb append l; this }

  def +=(c: Char) = { ss.sb append c; this }

  def +=(s: SQLBuilder) = { ss.sb append s; this }

  def +?=(f: Setter) = { ss.setters append f; ss.sb append '?'; this }

  def sep[T](seq: Iterable[T], separator: String): Iterator[T] = new Iterator[T] {
    var first = true
    val it = seq.iterator
    def hasNext = it.hasNext
    def next() = {
      val n = it.next()
      if(first) first = false else self += separator
      n
    }
  }

  def isEmpty = ss.sb.isEmpty

  def createSlot = {
    val s = new SQLBuilder
    segments += s
    currentStringSegment = null
    s
  }

  def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]): Unit =
    for(s <- segments) s.appendTo(res, setters)

  def build = {
    val sb = new StringBuilder(64)
    val setters = new ArrayBuffer[Setter]
    appendTo(sb, setters)
    Result(sb.toString, new CombinedSetter(setters))
  }
}

object SQLBuilder {
  final type Setter = ((PositionedParameters, Any) => Unit)

  final case class Result(sql: String, setter: Setter)

  private class CombinedSetter(b: Seq[Setter]) extends Setter {
    def apply(p: PositionedParameters, param: Any): Unit = for(s <- b) s(p, param)
  }

  trait Segment {
    def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]): Unit
  }

  class StringSegment extends Segment {
    val sb = new StringBuilder(32)
    val setters = new ArrayBuffer[Setter]

    def appendTo(res: StringBuilder, setters: ArrayBuffer[Setter]) {
      res append sb
      setters ++= this.setters
    }
  }
}
