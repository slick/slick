package org.scalaquery.util

import scala.collection.mutable.HashMap

trait NamingContext { self =>
  def nameFor(t: Node) = checkNameFor(t)._1

  def checkNameFor(t: Node): (String, Boolean)

  def overrideName(node: Node, newName: String): NamingContext = new NamingContext {
    def checkNameFor(t: Node) = if(t eq node) (newName, false) else self.checkNameFor(t)
  }
}

object NamingContext {
  def apply() = new NamingContext {
    private val tnames = new HashMap[RefId[Node], String]
    private var nextTid = 1

    def checkNameFor(t: Node) = tnames.get(RefId(t)) match {
      case Some(n) => (n, false)
      case None =>
        val n = "t" + nextTid
        nextTid += 1
        tnames.put(RefId(t), n)
        (n, true)
    }
  }
}
