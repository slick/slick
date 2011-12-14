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

trait IdContext {
  def checkIdFor(t: Node): Option[(Int, Boolean)]
  def idFor(t: Node) = checkIdFor(t).map(_._1)
}

object IdContext {
  def apply(t: Node) = {
    val counts = new HashMap[RefId[Node], Int]
    def scan(n: Node) {
      val r = RefId(n)
      val c = counts.getOrElse(r, 0)
      counts(r) = c + 1
      if(c == 0) n.nodeChildren.foreach(scan _)
    }
    scan(t)
    new IdContext {
      private val ids = new HashMap[RefId[Node], Int]
      private var nextId = 1
      def checkIdFor(t: Node) = {
        val r = RefId(t)
        val id = ids.getOrElse(r, 0)
        if(id > 0) Some((id, false))
        else if(counts.getOrElse(r, 0) > 1) {
          val nid = nextId
          nextId += 1
          ids(r) = nid
          Some((nid, true))
        }
        else None
      }
    }
  }
}
