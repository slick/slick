package scala.slick.util

import scala.collection.mutable.ArrayBuffer

/** Utility methods for creating result set debug output. */
class TableDump(maxColumnWidth: Int = 20) {
  protected[this] val dashes = Iterator.fill(maxColumnWidth+2)('-').mkString
  protected[this] val spaces = Iterator.fill(maxColumnWidth+2)(' ').mkString

  protected[this] def formatLine(line: IndexedSeq[Any]): IndexedSeq[String] = line.map { v =>
    val s = if(v == null) "NULL" else v.toString
    val s2 = s.replace("\r", "\\r").replace("\n", "\\n").replace("\t", "\\t")
    if(s2.length > maxColumnWidth) s2.substring(0, maxColumnWidth-3) + TreeDump.green+"..."+TreeDump.normal else s2
  }

  def apply(header: IndexedSeq[String], data: IndexedSeq[IndexedSeq[Any]]): IndexedSeq[String] = {
    val columns = header.length
    val texts = formatLine(header) +: data.map(formatLine)
    val widths = 0.until(columns).map(idx => texts.map(_.apply(idx).length).max)
    val buf = new ArrayBuffer[String](data.length + 4)
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("/", "+", "\\") + TreeDump.normal
    var first = true
    for(line <- texts) {
      if(first) {
        buf += (line, widths).zipped.map((s, len) => TreeDump.yellow+" "+s+spaces.substring(0, len-s.length)+" ").mkString(TreeDump.blue+"|", TreeDump.blue+"|", TreeDump.blue+"|"+TreeDump.normal)
        buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("+", "+", "+") + TreeDump.normal
        first = false
      } else {
        buf += (line, widths).zipped.map((s, len) => TreeDump.normal+" "+s+spaces.substring(0, len-s.length)+" ").mkString(TreeDump.blue+"|", TreeDump.blue+"|", TreeDump.blue+"|"+TreeDump.normal)
      }
    }
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("\\", "+", "/") + TreeDump.normal
    buf
  }
}
