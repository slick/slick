package scala.slick.util

import scala.collection.mutable.ArrayBuffer

/** Utility methods for creating result set debug output. */
class TableDump(maxColumnWidth: Int = 20) {
  protected[this] val dashes = Iterator.fill(maxColumnWidth+2)('-').mkString
  protected[this] val spaces = Iterator.fill(maxColumnWidth+2)(' ').mkString

  protected[this] def formatLine(line: IndexedSeq[Any]): IndexedSeq[String] = line.map { v =>
    (if(v == null) "NULL" else v.toString).replace("\r", "\\r").replace("\n", "\\n").replace("\t", "\\t")
  }

  def apply(header: IndexedSeq[String], data: IndexedSeq[IndexedSeq[Any]]): IndexedSeq[String] = {
    val columns = header.length
    val texts = formatLine(header) +: data.map(formatLine)
    val widths = 0.until(columns).map { idx => math.min(maxColumnWidth, texts.map(_.apply(idx).length).max) }
    val buf = new ArrayBuffer[String](data.length + 4)
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("/", "+", "\\") + TreeDump.normal
    var first = true
    def pad(s: String, len: Int): String = {
      if(s.length > maxColumnWidth) s.substring(0, maxColumnWidth-3) + TreeDump.cyan+"..."
      else s + spaces.substring(0, len-s.length)
    }
    for(line <- texts) {
      if(first) {
        buf += (line, widths).zipped.map((s, len) => TreeDump.yellow+" "+pad(s, len)+" ").mkString(TreeDump.blue+"|", TreeDump.blue+"|", TreeDump.blue+"|"+TreeDump.normal)
        buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("+", "+", "+") + TreeDump.normal
        first = false
      } else {
        buf += (line, widths).zipped.map((s, len) => TreeDump.normal+" "+pad(s, len)+" ").mkString(TreeDump.blue+"|", TreeDump.blue+"|", TreeDump.blue+"|"+TreeDump.normal)
      }
    }
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString("\\", "+", "/") + TreeDump.normal
    buf
  }
}
