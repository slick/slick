package com.novocode.squery.combinator

import scala.collection.mutable.HashMap
import com.novocode.squery.RefId

class NamingContext {
  private val tnames = new HashMap[RefId[Node], String]
  private var nextTid = 1

  def nameFor(t: Node) = tnames.get(RefId(t)) match {
    case Some(n) => n
    case None =>
      val n = "t" + nextTid
      nextTid += 1
      tnames.put(RefId(t), n)
      n
  }
}
