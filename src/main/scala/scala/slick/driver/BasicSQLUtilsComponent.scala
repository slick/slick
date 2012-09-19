package scala.slick.driver

import scala.slick.lifted.TypeMapper
import scala.slick.ast.{SymbolNamer, Symbol}

trait BasicSQLUtilsComponent { driver: BasicDriver =>

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for(c <- id) if(c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quote[T](v: T)(implicit tm: TypeMapper[T]): String = tm(driver).valueToSQLLiteral(v)

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for(c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }

  class QuotingSymbolNamer(parent: Option[SymbolNamer]) extends SymbolNamer("x", parent) {
    override def namedSymbolName(s: Symbol) = quoteIdentifier(s.name)
  }
}
