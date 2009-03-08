package com.novocode.squery.combinator

import java.io.PrintWriter


trait ColumnOp extends Dump

object ColumnOp {
  case class SortOp(base: Column[_], by: Column[_], desc: Boolean) extends ColumnOp {
    def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
      out.println(prefix+name+"ColumnOp.Sort(desc="+desc+")")
      dumper(out, prefix+"  ", "base: ", base)
      dumper(out, prefix+"  ", "by: ", by)
    }
  }

  case class JoinOp(base: TableBase[_], join: Join[_,_]) extends ColumnOp {
    def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
      out.println(prefix+name+"ColumnOp.JoinOp "+System.identityHashCode(this))
      dumper(out, prefix+"  ", "base: ", base)
      out.println(prefix+"  join: [...]")
    }
  }

  case class SubQueryOp(query: Query[_]) extends ColumnOp {
    def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
      out.println(prefix+name+"ColumnOp.SubQueryOp "+System.identityHashCode(this))
      dumper(out, prefix+"  ", "query: ", query)
    }
  }

  case class BaseTableQueryOp(base: TableBase[_]) extends ColumnOp {
    def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
      out.println(prefix+name+"ColumnOp.BaseTableQueryOp "+System.identityHashCode(this))
      dumper(out, prefix+"  ", "base: ", base)
    }
  }

  case class UnionOp(union: Union[_]) extends ColumnOp {
    def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper) {
      out.println(prefix+name+"ColumnOp.UnionOp "+System.identityHashCode(this))
    }
  }
}
