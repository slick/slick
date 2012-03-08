package org.scalaquery.ast

import scala.collection.mutable.HashMap
import org.scalaquery.util.RefId

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
