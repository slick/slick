package scala.slick.util

import scala.collection.mutable.ArrayBuffer

/** Utility methods for creating result set debug output. */
class TableDump(maxColumnWidth: Int = 20) {
  protected[this] val box: IndexedSeq[String] =
    (if(GlobalConfig.unicodeDump) "\u2501\u250f\u2533\u2513\u2523\u254b\u252b\u2517\u253b\u251b\u2503" else "-/+\\|+|\\+/|").map(_.toString)

  protected[this] val dashes = Iterator.fill(maxColumnWidth+2)(box(0)).mkString
  protected[this] val spaces = Iterator.fill(maxColumnWidth+2)(' ').mkString

  protected[this] def formatLine(line: IndexedSeq[Any]): IndexedSeq[String] = line.map { v =>
    (if(v == null) "NULL" else v.toString).replace("\r", "\\r").replace("\n", "\\n").replace("\t", "\\t")
  }

  def apply(header: IndexedSeq[String], data: IndexedSeq[IndexedSeq[Any]]): IndexedSeq[String] = {
    val columns = header.length
    val texts = formatLine(header) +: data.map(formatLine)
    val widths = 0.until(columns).map { idx => math.min(maxColumnWidth, texts.map(_.apply(idx).length).max) }
    val buf = new ArrayBuffer[String](data.length + 4)
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString(box(1), box(2), box(3)) + TreeDump.normal
    var first = true
    def pad(s: String, len: Int): String = {
      val slen = s.codePointCount(0, s.length)
      if(slen > maxColumnWidth) {
        if(slen == s.length) s.substring(0, maxColumnWidth-3) else limitCodepoints(s, maxColumnWidth-3)
      } + TreeDump.cyan+"..."
      else s + spaces.substring(0, len-slen)
    }
    for(line <- texts) {
      if(first) {
        buf += (line, widths).zipped.map((s, len) => TreeDump.yellow+" "+pad(s, len)+" ").mkString(TreeDump.blue+box(10), TreeDump.blue+box(10), TreeDump.blue+box(10)+TreeDump.normal)
        buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString(box(4), box(5), box(6)) + TreeDump.normal
        first = false
      } else {
        buf += (line, widths).zipped.map((s, len) => TreeDump.normal+" "+pad(s, len)+" ").mkString(TreeDump.blue+box(10), TreeDump.blue+box(10), TreeDump.blue+box(10)+TreeDump.normal)
      }
    }
    buf += TreeDump.blue + widths.map(l => dashes.substring(0, l+2)).mkString(box(7), box(8), box(9)) + TreeDump.normal
    buf
  }

  /** Return the first `len` codepoints from a String */
  protected[this] def limitCodepoints(s: String, len: Int): String = {
    val b = new StringBuilder(s.length, "")
    var i = 0
    var cps = 0
    while(cps < len) {
      val c = s.charAt(i)
      if(Character.isHighSurrogate(c)) {
        b.append(c)
        b.append(s.charAt(i+1))
        i += 1
      } else {
        b.append(c)
      }
      cps += 1
      i += 1
    }
    b.toString
  }
}
