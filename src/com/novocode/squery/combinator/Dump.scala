package com.novocode.squery.combinator

import java.io.PrintWriter


trait Dump {
  def dump(out: PrintWriter, prefix: String, name: String) = Dump.DEFAULT(out, prefix, name, this)

  def dump(name: String) {
    val out = new PrintWriter(System.out)
    Dump.DEFAULT(out, "", name, this)
    out.flush()
  }

  def dumpThis(out: PrintWriter, prefix: String, name: String, dumper: Dump.Dumper): Unit
}

object Dump {
  type Dumper = (PrintWriter, String, String, Any) => Unit

  class DefaultDumper extends Dumper {
    def apply(out: PrintWriter, prefix: String, name: String, o: Any) {
      if(o.isInstanceOf[WithOp]) {
        val w = o.asInstanceOf[WithOp]
        if(w.op != null) {
          apply(out, prefix, name+"<WithOp> ", w.op)
          return
        }
      }
      o match {
        case null => out.println(prefix+name+"null")
        //case WithOp(op) => dumpObject(out, prefix, name+"<WithOp> ", op)
        case d: Dump => d.dumpThis(out, prefix, name, this)
        case p: Product => {
          out.println(prefix+name+"Product "+p.getClass.getName)
          for(i <- 0 until p.productArity)
            apply(out, prefix+"  ", "_"+(i+1)+": ", p.productElement(i))
        }
        case o => out.println(prefix+name+"[_] "+o)
      }
    }
  }

  val DEFAULT = new DefaultDumper
}
