package com.novocode.squery.combinator.sql

import scala.collection.mutable.ListBuffer
import com.novocode.squery.session.TypeMapper.StringTypeMapper

final class SQLBuilder {
  private val sb = new StringBuilder(32)
  private var slots = new ListBuffer[(Int,SQLBuilder)]

  def +=(s: String) = { sb append s; this }

  def +=(c: Char) = { sb append c; this }

  def +=(s: SQLBuilder) = { sb append s.sb; this }

  def +?=(s: String) = { StringTypeMapper.createStringLiteral(s, sb); this }

  def createSlot = {
    val d = new SQLBuilder
    slots += ((sb.length, d))
    d
  }

  private def buildString(res: StringBuilder) {
    var start = 0
    for((i,d) <- slots) {
      if(start < i) res.append(sb.substring(start, i))
      start = i
      res.append(d.toString)
    }
    val len = sb.length
    if(start < len) res.append(sb.substring(start, len))
  }

  override def toString = {
    if(slots.isEmpty) sb.toString
    else {
      val res = new StringBuilder
      buildString(res)
      res.toString
    }
  }
}
