package scala.slick.lifted

import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.reflect.macros.Context
import scala.slick.ast.{Join => AJoin, _}
import FunctionSymbolExtensionMethods._
import ScalaBaseType._

/** An instance of Query represents a query or view, i.e. a computation of a
  * collection type (Rep[Seq[T]]). It is parameterized with both, the mixed
  * type (the type of values you see e.g. when you call map()) and the unpacked
  * type (the type of values that you get back when you run the query).  */
abstract class Query[+E, U] extends Rep[Seq[U]] with EncodeRef { self =>

  def unpackable: ShapedValue[_ <: E, U]
  final lazy val packed = unpackable.packedNode

  def flatMap[F, T](f: E => Query[F, T]): Query[F, T] = {
    val generator = new AnonSymbol
    val aliased = {
      val uv = unpackable.value
      EncodeRef(uv, generator)
    }
    val fv = f(aliased)
    new WrappingQuery[F, T](new Bind(generator, Node(this), Node(fv)), fv.unpackable)
  }

  def map[F, G, T](f: E => F)(implicit shape: Shape[F, T, G]): Query[G, T] =
    flatMap(v => Query.pure[F, T, G](f(v)))

  def >>[F, T](q: Query[F, T]): Query[F, T] = flatMap(_ => q)

  def filter[T](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = unpackable.encodeRef(generator)
    val fv = f(aliased.value)
    new WrappingQuery[E, U](Filter.ifRefutable(generator, Node(this), Node(wt(fv))), unpackable)
  }

  def withFilter[T : CanBeQueryCondition](f: E => T) = filter(f)

  def where[T <: Column[_] : CanBeQueryCondition](f: E => T) = filter(f)

  def join[E2, U2](q2: Query[E2, U2], jt: JoinType = JoinType.Inner) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = unpackable.encodeRef(leftGen)
    val aliased2 = q2.unpackable.encodeRef(rightGen)
    new BaseJoinQuery[E, E2, U, U2](leftGen, rightGen, Node(this), Node(q2), jt, aliased1.zip(aliased2))
  }
  def innerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Inner)
  def leftJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Left)
  def rightJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Right)
  def outerJoin[E2, U2](q2: Query[E2, U2]) = join(q2, JoinType.Outer)
  def zip[E2, U2](q2: Query[E2, U2]): Query[(E, E2), (U, U2)] = join(q2, JoinType.Zip)
  def zipWith[E2, U2, F, G, T](q2: Query[E2, U2], f: (E, E2) => F)(implicit shape: Shape[F, T, G]): Query[G, T] =
    join(q2, JoinType.Zip).map[F, G, T](x => f(x._1, x._2))
  def zipWithIndex = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = unpackable.encodeRef(leftGen)
    val aliased2 = ShapedValue(Column.forNode[Long](Ref(rightGen)), Shape.columnShape[Long])
    new BaseJoinQuery[E, Column[Long], U, Long](leftGen, rightGen, Node(this), RangeFrom(0L), JoinType.Zip, aliased1.zip(aliased2))
  }

  def sortBy[T <% Ordered](f: E => T): Query[E, U] = {
    val generator = new AnonSymbol
    val aliased = unpackable.encodeRef(generator)
    new WrappingQuery[E, U](SortBy(generator, Node(this), f(aliased.value).columns), unpackable)
  }

  def sorted(implicit ev: (E => Ordered)): Query[E, U] = sortBy(identity)

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[K, T, G], vshape: Shape[E, _, P]): Query[(G, Query[P, U]), (T, Query[P, U])] = {
    val sym = new AnonSymbol
    val key = ShapedValue(f(unpackable.encodeRef(sym).value), kshape).packedValue
    val value = ShapedValue(pack, Shape.impureShape.asInstanceOf[Shape[Query[P, U], Query[P, U], Query[P, U]]])
    val group = GroupBy(sym, Node(this), Node(key.value))
    new WrappingQuery(group, key.zip(value))
  }

  def encodeRef(sym: Symbol, positions: List[Int] = Nil): Query[E, U] = new Query[E, U] {
    val unpackable = self.unpackable.encodeRef(sym, positions)
    lazy val nodeDelegate =
      positions.foldRight[Node](Ref(sym))((idx, node) => Select(node, ElementSymbol(idx)))
  }

  def union[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(this), Node(other), false), unpackable)

  def unionAll[O >: E, R](other: Query[O, U]) =
    new WrappingQuery[O, U](Union(Node(this), Node(other), true), unpackable)

  def length: Column[Int] = Library.CountAll.column(Node(this))
  def countDistinct: Column[Int] = Library.CountDistinct.column(Node(this))
  def exists = Library.Exists.column[Boolean](Node(this))

  def pack[R](implicit packing: Shape[E, _, R]): Query[R, U] =
    new Query[R, U] {
      val unpackable: ShapedValue[_ <: R, U] = self.unpackable.packedValue(packing)
      def nodeDelegate = self.nodeDelegate
    }

  def take(num: Int): Query[E, U] = new WrappingQuery[E, U](Take(Node(this), num), unpackable)
  def drop(num: Int): Query[E, U] = new WrappingQuery[E, U](Drop(Node(this), num), unpackable)
}

object Query extends Query[Column[Unit], Unit] {
  def nodeDelegate = packed
  def unpackable = ShapedValue(Column.forNode[Unit](Pure(LiteralNode[Unit](()))), Shape.unpackColumnBase[Unit, Column[Unit]])

  def apply[E, U, R](value: E)(implicit unpack: Shape[E, U, R]): Query[R, U] = {
    val unpackable = ShapedValue(value, unpack).packedValue
    if(unpackable.packedNode.isInstanceOf[TableNode])
      new NonWrappingQuery[R, U](unpackable.packedNode, unpackable)
    else new WrappingQuery[R, U](Pure(unpackable.packedNode), unpackable)
  }

  def pure[E, U, R](value: E)(implicit unpack: Shape[E, U, R]): Query[R, U] = {
    val unpackable = ShapedValue(value, unpack).packedValue
    new WrappingQuery[R, U](Pure(unpackable.packedNode), unpackable)
  }
}

@implicitNotFound("Type ${T} cannot be a query condition (only Boolean, Column[Boolean] and Column[Option[Boolean]] are allowed")
trait CanBeQueryCondition[-T] extends (T => Column[_])

object CanBeQueryCondition {
  implicit object BooleanColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Boolean]] {
    def apply(value: Column[Boolean]) = value
  }
  implicit object BooleanOptionColumnCanBeQueryCondition extends CanBeQueryCondition[Column[Option[Boolean]]] {
    def apply(value: Column[Option[Boolean]]) = value
  }
  implicit object BooleanCanBeQueryCondition extends CanBeQueryCondition[Boolean] {
    def apply(value: Boolean) = new ConstColumn(value)
  }
}

class WrappingQuery[+E, U](val nodeDelegate: Node, val base: ShapedValue[_ <: E, U]) extends Query[E, U] {
  lazy val unpackable = base.encodeRef(nodeDelegate.nodeIntrinsicSymbol)
}

class NonWrappingQuery[+E, U](val nodeDelegate: Node, val unpackable: ShapedValue[_ <: E, U]) extends Query[E, U]

final class BaseJoinQuery[+E1, +E2, U1, U2](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: ShapedValue[_ <: (E1, E2), (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2)](AJoin(leftGen, rightGen, left, right, jt, LiteralNode(true)), base) {
  def on[T <: Column[_]](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T]) =
    new WrappingQuery[(E1, E2), (U1, U2)](AJoin(leftGen, rightGen, left, right, jt, Node(wt(pred(base.value._1, base.value._2)))), base)
}

/** Represents a database table. Profiles add extension methods to TableQuery
  * for operations that can be performed on tables but not on arbitrary
  * queries, e.g. getting the table DDL. */
final class TableQuery[+E <: AbstractTable[_], U](shaped: ShapedValue[_ <: E, U])
  extends NonWrappingQuery[E, U](shaped.packedNode, shaped) {

  /** Get the "raw" table row that represents the table itself, as opposed to
    * a Path for a variable of the table's type. This method should generally
    * not be called from user code. */
  def baseTableRow: E = unpackable.value
}

object TableQuery {
  def apply[E <: AbstractTable[_]](cons: Tag => E): TableQuery[E, E#TableElementType] = {
    val baseTable = cons(new BaseTag { base =>
      def taggedAs(tableRef: Node): AbstractTable[_] = cons(new RefTag {
        def nodeDelegate: Node = tableRef
        def taggedAs(tableRef: Node) = base.taggedAs(tableRef)
      })
    })
    new TableQuery[E, E#TableElementType](ShapedValue(baseTable, Shape.impureShape.asInstanceOf[Shape[E, E#TableElementType, E]]))
  }

  def apply[E <: AbstractTable[_]]: TableQuery[E, E#TableElementType] =
    macro TableQueryMacroImpl.apply[E]
}

object TableQueryMacroImpl {

  def apply[E <: AbstractTable[_]](c: Context)(implicit e: c.WeakTypeTag[E]): c.Expr[TableQuery[E, E#TableElementType]] = {
    import c.universe._
    val cons = c.Expr[Tag => E](Function(
      List(ValDef(Modifiers(Flag.PARAM), newTermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("tag")))
      )
    ))
    reify { TableQuery.apply[E](cons.splice) }
  }
}
