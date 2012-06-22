package scala.slick.driver

import java.sql.Types._
import scala.slick.ql.{TypeMapper, TypeMapperDelegate}
import scala.collection.mutable.HashMap
import scala.slick.ast.{AnonSymbol, Symbol}

trait BasicSQLUtilsComponent { driver: BasicDriver =>

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for(c <- id) if(c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def quote[T](v: T)(implicit tm: TypeMapper[T]): String = tm(driver).valueToSQLLiteral(v)

  def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case VARCHAR => "VARCHAR(254)"
    case _ => tmd.sqlTypeName
  }

  def likeEncode(s: String) = {
    val b = new StringBuilder
    for(c <- s) c match {
      case '%' | '_' | '^' => b append '^' append c
      case _ => b append c
    }
    b.toString
  }

  /** Provides names for symbols */
  class SymbolNamer(symbolPrefix: String = "x") {
    private var curSymbolId = 1
    private val map = new HashMap[Symbol, String]
    private val reverse = new HashMap[String, Option[Symbol]]

    def create = {
      curSymbolId += 1
      symbolPrefix + curSymbolId
    }

    def apply(s: Symbol): String = map.getOrElse(s, s match {
      case a: AnonSymbol =>
        if(a.hasName) a.name else {
          val n = create
          update(a, n)
          n
        }
      case s => quoteIdentifier(s.name)
    })

    def update(s: Symbol, n: String) {
      map += s -> n
      reverse += n -> Some(s)
    }
  }
}
