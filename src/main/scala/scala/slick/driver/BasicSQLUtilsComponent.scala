package scala.slick.driver

import java.sql.Types._
import scala.slick.ql.TypeMapperDelegate
import scala.collection.mutable.HashMap
import scala.slick.ast.{AnonSymbol, Symbol}

trait BasicSQLUtilsComponent {

  def quoteIdentifier(id: String): String = {
    val s = new StringBuilder(id.length + 4) append '"'
    for(c <- id) if(c == '"') s append "\"\"" else s append c
    (s append '"').toString
  }

  def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case VARCHAR => "VARCHAR(254)"
    case _ => tmd.sqlTypeName
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
