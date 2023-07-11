package slick.lifted

import scala.quoted.*

import slick.ast.Node

/** Represents a database table. Profiles add extension methods to TableQuery
 * for operations that can be performed on tables but not on arbitrary
 * queries, e.g. getting the table DDL. */
class TableQuery[E <: AbstractTable[_]](cons: Tag => E) extends Query[E, TableQuery.Extract[E], Seq] {
  lazy val shaped = {
    val baseTable = cons(new BaseTag { base =>
      def taggedAs(path: Node): AbstractTable[_] = cons(new RefTag(path) {
        def taggedAs(path: Node) = base.taggedAs(path)
      })
    })
    ShapedValue(baseTable, RepShape[FlatShapeLevel, E, TableQuery.Extract[E]])
  }

  lazy val toNode = shaped.toNode

  /** Get the "raw" table row that represents the table itself, as opposed to
   * a Path for a variable of the table's type. This method should generally
   * not be called from user code. */
  def baseTableRow: E = shaped.value
}

object TableQuery {
  /** Create a TableQuery for a table row class using an arbitrary constructor function. */
  def apply[E <: AbstractTable[_]](cons: Tag => E): TableQuery[E] =
    new TableQuery[E](cons)

  type Extract[E] = E match {
    case AbstractTable[t] => t
  }

  /** Create a TableQuery for a table row class which has a constructor of type (Tag). */
  inline def apply[E <: AbstractTable[_]]: TableQuery[E] = ${ applyExpr[E] }

  private def applyExpr[E <: AbstractTable[_]](using quotes: Quotes, e: Type[E]): Expr[TableQuery[E]] = {
    import quotes.reflect.*
    val eTpe = TypeRepr.of(using e)
    val tagTpe = TypeRepr.of[Tag]
    val mt = MethodType(List("tag"))(_ => List(tagTpe), _ => eTpe)

    val cons = Lambda(Symbol.spliceOwner, mt, { (meth, tag) =>
      Select.overloaded(New(TypeIdent(eTpe.typeSymbol)), "<init>",
        List(),
        List(tag.head.asInstanceOf[Term])
      )
    })

    val ctorExpr = cons.asExprOf[Tag => E]

    '{ TableQuery.apply[E](${ ctorExpr }) }
  }
}
