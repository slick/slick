package org.scalaquery.util

import scala.collection.mutable.HashMap

trait NamingContext { self =>
  def nameFor(t: Node): String

  def overrideName(node: Node, newName: String): NamingContext = new NamingContext {
    def nameFor(t: Node) = if(t eq node) newName else self.nameFor(t)
  }
}

object NamingContext {
  def apply() = new NamingContext {
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
}
