package slick.lifted

import slick.util.ConstArray

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.reflect.macros.blackbox.Context
import slick.ast.{Join => AJoin, _}
import FunctionSymbolExtensionMethods._
import ScalaBaseType._

sealed trait QueryBase[T] extends Rep[T]

/** An instance of Query represents a query or view, i.e. a computation of a
  * collection type (Rep[Seq[T]]). It is parameterized with both, the mixed
  * type (the type of values you see e.g. when you call map()) and the unpacked
  * type (the type of values that you get back when you run the query).
  *
  * Additional extension methods for queries containing a single column are
  * defined in [[slick.lifted.SingleColumnQueryExtensionMethods]].
  */
sealed abstract class Query[+E, U, C[_]] extends QueryBase[C[U]] { self =>
  def shaped: ShapedValue[_ <: E, U]
  final lazy val packed = shaped.toNode

  /** Build a new query by applying a function to all elements of this query
    * and using the elements of the resulting queries. This corresponds to an
    * implicit inner join in SQL. */
  def flatMap[F, T, D[_]](f: E => Query[F, T, D]): Query[F, T, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    new WrappingQuery[F, T, C](new Bind(generator, toNode, fv.toNode), fv.shaped)
  }

  /** Build a new query by applying a function to all elements of this query. */
  def map[F, G, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    flatMap(v => Query[F, T, G](f(v)))

  /** Select all elements of this query which satisfy a predicate. */
  private def filterHelper[T](f: E => T, wrapExpr: Node => Node)
                             (implicit wt: CanBeQueryCondition[T]): Query[E, U, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator))
    val fv = f(aliased.value)
    new WrappingQuery[E, U, C](Filter.ifRefutable(generator, toNode, wrapExpr(wt(fv).toNode)), shaped)
  }
  /** Select all elements of this query which satisfy a predicate. Unlike
    * `withFilter, this method only allows `Rep`-valued predicates, so it
    * guards against the accidental use use plain Booleans. */
  def filter[T <: Rep[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    withFilter(f)
  def filterNot[T <: Rep[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    filterHelper(f, node => Library.Not.typed(node.nodeType, node) )

  /** Select all elements of this query which satisfy a predicate. This method
    * is used when desugaring for-comprehensions over queries. There is no
    * reason to call it directly because it is the same as `filter`. */
  def withFilter[T : CanBeQueryCondition](f: E => T) = filterHelper(f, identity)

  /** Join two queries with a cross join or inner join.
    * An optional join predicate can be specified later by calling `on`. */
  def join[E2, U2, D[_]](q2: Query[E2, U2, D]) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(Ref(leftGen))
    val aliased2 = q2.shaped.encodeRef(Ref(rightGen))
    new BaseJoinQuery[E, E2, U, U2, C, E, E2](leftGen, rightGen, toNode, q2.toNode, JoinType.Inner,
      aliased1.zip(aliased2), aliased1.value, aliased2.value)
  }

  /** Join two queries with a left outer join.
    * An optional join predicate can be specified later by calling `on`.
    * The right side of the join is lifted to an `Option`. If at least one element on the right
    * matches, all matching elements are returned as `Some`, otherwise a single `None` row is
    * returned. */
  def joinLeft[E2, U2, D[_], O2](q2: Query[E2, _, D])(implicit ol: OptionLift[E2, O2], sh: Shape[FlatShapeLevel, O2, U2, _]) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(Ref(leftGen))
    val aliased2 = ShapedValue(ol.lift(q2.shaped.value), sh).encodeRef(Ref(rightGen))
    new BaseJoinQuery[E, O2, U, U2, C, E, E2](leftGen, rightGen, toNode, q2.toNode, JoinType.LeftOption,
      aliased1.zip(aliased2), aliased1.value, q2.shaped.encodeRef(Ref(rightGen)).value)
  }

  /** Join two queries with a right outer join.
    * An optional join predicate can be specified later by calling `on`.
    * The left side of the join is lifted to an `Option`. If at least one element on the left
    * matches, all matching elements are returned as `Some`, otherwise a single `None` row is
    * returned. */
  def joinRight[E1 >: E, E2, U2, D[_], O1, U1](q2: Query[E2, U2, D])(implicit ol: OptionLift[E1, O1], sh: Shape[FlatShapeLevel, O1, U1, _]) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = ShapedValue(ol.lift(shaped.value), sh).encodeRef(Ref(leftGen))
    val aliased2 = q2.shaped.encodeRef(Ref(rightGen))
    new BaseJoinQuery[O1, E2, U1, U2, C, E, E2](leftGen, rightGen, toNode, q2.toNode, JoinType.RightOption,
      aliased1.zip(aliased2), shaped.encodeRef(Ref(leftGen)).value, aliased2.value)
  }

  /** Join two queries with a full outer join.
    * An optional join predicate can be specified later by calling `on`.
    * Both sides of the join are lifted to an `Option`. If at least one element on either side
    * matches the other side, all matching elements are returned as `Some`, otherwise a single
    * `None` row is returned. */
  def joinFull[E1 >: E, E2, U2, D[_], O1, U1, O2](q2: Query[E2, _, D])(implicit ol1: OptionLift[E1, O1], sh1: Shape[FlatShapeLevel, O1, U1, _], ol2: OptionLift[E2, O2], sh2: Shape[FlatShapeLevel, O2, U2, _]) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = ShapedValue(ol1.lift(shaped.value), sh1).encodeRef(Ref(leftGen))
    val aliased2 = ShapedValue(ol2.lift(q2.shaped.value), sh2).encodeRef(Ref(rightGen))
    new BaseJoinQuery[O1, O2, U1, U2, C, E, E2](leftGen, rightGen, toNode, q2.toNode, JoinType.OuterOption,
      aliased1.zip(aliased2), shaped.encodeRef(Ref(leftGen)).value, q2.shaped.encodeRef(Ref(rightGen)).value)
  }

  private[this] def standardJoin[E2, U2, D[_]](q2: Query[E2, U2, D], jt: JoinType) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(Ref(leftGen))
    val aliased2 = q2.shaped.encodeRef(Ref(rightGen))
    new BaseJoinQuery[E, E2, U, U2, C, E, E2](leftGen, rightGen, toNode, q2.toNode, jt,
      aliased1.zip(aliased2), aliased1.value, aliased2.value)
  }

  /** Return a query formed from this query and another query by combining
    * corresponding elements in pairs. */
  def zip[E2, U2, D[_]](q2: Query[E2, U2, D]): Query[(E, E2), (U, U2), C] = standardJoin(q2, JoinType.Zip)

  /** Return a query formed from this query and another query by combining
    * corresponding elements with the specified function. */
  def zipWith[E2, U2, F, G, T, D[_]](q2: Query[E2, U2, D], f: (E, E2) => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    standardJoin(q2, JoinType.Zip).map[F, G, T](x => f(x._1, x._2))

  /** Zip this query with its indices (starting at 0). */
  def zipWithIndex = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(Ref(leftGen))
    val aliased2 = ShapedValue(Rep.forNode[Long](Ref(rightGen)), Shape.repColumnShape[Long, FlatShapeLevel])
    new BaseJoinQuery[E, Rep[Long], U, Long, C, E, Rep[Long]](leftGen, rightGen, toNode, RangeFrom(0L), JoinType.Zip, aliased1.zip(aliased2), aliased1.value, aliased2.value)
  }

  /** Sort this query according to a function which extracts the ordering
    * criteria from the query's elements. */
  def sortBy[T](f: E => T)(implicit ev: T => Ordered): Query[E, U, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator))
    new WrappingQuery[E, U, C](SortBy(generator, toNode, ConstArray.from(f(aliased.value).columns)), shaped)
  }

  /** Sort this query according to a the ordering of its elements. */
  def sorted(implicit ev: (E => Ordered)): Query[E, U, C] = sortBy(identity)

  /** Partition this query into a query of pairs of a key and a nested query
    * containing the elements for the key, according to some discriminator
    * function. */
  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): Query[(G, Query[P, U, Seq]), (T, Query[P, U, Seq]), C] = {
    val sym = new AnonSymbol
    val key = ShapedValue(f(shaped.encodeRef(Ref(sym)).value), kshape).packedValue
    val value = ShapedValue(pack.to[Seq], RepShape[FlatShapeLevel, Query[P, U, Seq], Query[P, U, Seq]])
    val group = GroupBy(sym, toNode, key.toNode)
    new WrappingQuery[(G, Query[P, U, Seq]), (T, Query[P, U, Seq]), C](group, key.zip(value))
  }

  /** Specify part of a select statement for update and marked for row level locking */
  def forUpdate: Query[E, U, C] = {
    val generator = new AnonSymbol
    new WrappingQuery[E, U, C](ForUpdate(generator, toNode), shaped)
  }
  def encodeRef(path: Node): Query[E, U, C] = new Query[E, U, C] {
    val shaped = self.shaped.encodeRef(path)
    def toNode = path
  }

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are eliminated from the result. */
  def union[O >: E, R, D[_]](other: Query[O, U, D]): Query[O, U, C] =
    new WrappingQuery[O, U, C](Union(toNode, other.toNode, false), shaped)

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are preserved. */
  def unionAll[O >: E, R, D[_]](other: Query[O, U, D]): Query[O, U, C] =
    new WrappingQuery[O, U, C](Union(toNode, other.toNode, true), shaped)

  /** Return a new query containing the elements from both operands. Duplicate
    * elements are preserved. */
  def ++[O >: E, R, D[_]](other: Query[O, U, D]) = unionAll(other)

  /** The total number of elements (i.e. rows). */
  def length: Rep[Int] = Library.CountAll.column(toNode)
  /** The total number of elements (i.e. rows). */
  def size = length

  /** The number of distinct elements of the query. */
  def countDistinct: Rep[Int] = Library.CountDistinct.column(toNode)

  /** Test whether this query is non-empty. */
  def exists = Library.Exists.column[Boolean](toNode)

  def pack[R](implicit packing: Shape[_ <: FlatShapeLevel, E, _, R]): Query[R, U, C] =
    new Query[R, U, C] {
      val shaped: ShapedValue[_ <: R, U] = self.shaped.packedValue(packing)
      def toNode = self.toNode
    }

  /** Select the first `num` elements. */
  def take(num: ConstColumn[Long]): Query[E, U, C] = new WrappingQuery[E, U, C](Take(toNode, num.toNode), shaped)
  /** Select the first `num` elements. */
  def take(num: Long): Query[E, U, C] = take(LiteralColumn(num))
  /** Select the first `num` elements. */
  def take(num: Int): Query[E, U, C] = take(num.toLong)

  /** Select all elements except the first `num` ones. */
  def drop(num: ConstColumn[Long]): Query[E, U, C] = new WrappingQuery[E, U, C](Drop(toNode, num.toNode), shaped)
  /** Select all elements except the first `num` ones. */
  def drop(num: Long): Query[E, U, C] = drop(LiteralColumn(num))
  /** Select all elements except the first `num` ones. */
  def drop(num: Int): Query[E, U, C] = drop(num.toLong)

  /** Remove duplicate elements. When used on an ordered Query, there is no guarantee in which
    * order duplicates are removed. This method is equivalent to `distinctOn(identity)`. */
  def distinct: Query[E, U, C] =
    distinctOn[E, U](identity)(shaped.shape.asInstanceOf[Shape[FlatShapeLevel, E, U, _]])
  /** Remove duplicate elements which are the same in the given projection. When used on an
    * ordered Query, there is no guarantee in which order duplicates are removed. */
  def distinctOn[F, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, _]): Query[E, U, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    new WrappingQuery[E, U, C](Distinct(generator, toNode, shape.toNode(fv)), shaped)
  }

  /** Change the collection type to build when executing the query. */
  def to[D[_]](implicit ctc: TypedCollectionTypeConstructor[D]): Query[E, U, D] = new Query[E, U, D] {
    val shaped = self.shaped
    def toNode = CollectionCast(self.toNode, ctc)
  }

  /** Force a subquery to be created when using this Query as part of a larger Query. This method
    * should never be necessary for correctness. If a query works with an explicit `.subquery` call
    * but fails without, this should be considered a bug in Slick. The method is exposed in the API
    * to enable workarounds to be written in such cases. */
  def subquery: Query[E, U, C] = new WrappingQuery[E, U, C](Subquery(toNode, Subquery.Default), shaped)
}

/** The companion object for Query contains factory methods for creating queries. */
object Query {
  /** Lift a scalar value to a Query. */
  def apply[E, U, R](value: E)(implicit unpack: Shape[_ <: FlatShapeLevel, E, U, R]): Query[R, U, Seq] = {
    val shaped = ShapedValue(value, unpack).packedValue
    new WrappingQuery[R, U, Seq](Pure(shaped.toNode), shaped)
  }

  /** The empty Query. */
  def empty: Query[Unit, Unit, Seq] = new Query[Unit, Unit, Seq] {
    val toNode = shaped.toNode
    def shaped = ShapedValue((), Shape.unitShape[FlatShapeLevel])
  }

  @inline implicit def queryShape[Level >: NestedShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]
}

/** A typeclass for types that can be used as predicates in `filter` calls. */
@implicitNotFound("Type ${T} cannot be a query condition (only Boolean, Rep[Boolean] and Rep[Option[Boolean]] are allowed")
trait CanBeQueryCondition[-T] extends (T => Rep[_])

object CanBeQueryCondition {
  // Using implicits with explicit type annotation here (instead of previously implicit objects)
  // because otherwise they would not be found in this file above this line. 
  // See https://github.com/slick/slick/pull/217
  implicit val BooleanColumnCanBeQueryCondition : CanBeQueryCondition[Rep[Boolean]] =
    new CanBeQueryCondition[Rep[Boolean]] {
      def apply(value: Rep[Boolean]) = value
    }
  implicit val BooleanOptionColumnCanBeQueryCondition : CanBeQueryCondition[Rep[Option[Boolean]]] =
    new CanBeQueryCondition[Rep[Option[Boolean]]] {
      def apply(value: Rep[Option[Boolean]]) = value
    }
  implicit val BooleanCanBeQueryCondition : CanBeQueryCondition[Boolean] =
    new CanBeQueryCondition[Boolean] {
      def apply(value: Boolean) = new LiteralColumn(value)
    }
}

class WrappingQuery[+E, U, C[_]](val toNode: Node, val shaped: ShapedValue[_ <: E, U]) extends Query[E, U, C]

final class BaseJoinQuery[+E1, +E2, U1, U2, C[_], +B1, +B2](leftGen: TermSymbol, rightGen: TermSymbol, left: Node, right: Node, jt: JoinType, base: ShapedValue[_ <: (E1, E2), (U1, U2)], b1: B1, b2: B2)
    extends WrappingQuery[(E1, E2), (U1,  U2), C](AJoin(leftGen, rightGen, left, right, jt, LiteralNode(true)), base) {
  /** Add a join condition to a join operation. */
  def on[T <: Rep[_]](pred: (B1, B2) => T)(implicit wt: CanBeQueryCondition[T]): Query[(E1, E2), (U1, U2), C] =
    new WrappingQuery[(E1, E2), (U1, U2), C](AJoin(leftGen, rightGen, left, right, jt, wt(pred(b1, b2)).toNode), base)
}

/** Represents a database table. Profiles add extension methods to TableQuery
  * for operations that can be performed on tables but not on arbitrary
  * queries, e.g. getting the table DDL. */
class TableQuery[E <: AbstractTable[_]](cons: Tag => E) extends Query[E, E#TableElementType, Seq] {
  lazy val shaped = {
    val baseTable = cons(new BaseTag { base =>
      def taggedAs(path: Node): AbstractTable[_] = cons(new RefTag(path) {
        def taggedAs(path: Node) = base.taggedAs(path)
      })
    })
    ShapedValue(baseTable, RepShape[FlatShapeLevel, E, E#TableElementType])
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

  /** Create a TableQuery for a table row class which has a constructor of type (Tag). */
  def apply[E <: AbstractTable[_]]: TableQuery[E] =
    macro TableQueryMacroImpl.apply[E]
}

object TableQueryMacroImpl {

  def apply[E <: AbstractTable[_]](c: Context)(implicit e: c.WeakTypeTag[E]): c.Expr[TableQuery[E]] = {
    import c.universe._
    val cons = c.Expr[Tag => E](Function(
      List(ValDef(Modifiers(Flag.PARAM), TermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), termNames.CONSTRUCTOR),
        List(Ident(TermName("tag")))
      )
    ))
    reify { TableQuery.apply[E](cons.splice) }
  }
}
