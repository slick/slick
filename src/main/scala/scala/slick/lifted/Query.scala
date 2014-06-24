package scala.slick.lifted

import scala.language.higherKinds
import scala.language.experimental.macros
import scala.annotation.implicitNotFound
import scala.reflect.macros.Context
import scala.slick.ast.{Join => AJoin, _}
import FunctionSymbolExtensionMethods._
import ScalaBaseType._
import scala.util.Random

sealed trait QueryBase[T] extends Rep[T]

/** An instance of Query represents a query or view, i.e. a computation of a
  * collection type (Rep[Seq[T]]). It is parameterized with both, the mixed
  * type (the type of values you see e.g. when you call map()) and the unpacked
  * type (the type of values that you get back when you run the query).  */
sealed abstract class Query[+E, U, C[_]] extends QueryBase[C[U]] { self =>
  def shaped: ShapedValue[_ <: E, U]
  final lazy val packed = shaped.toNode

  /** Build a new query by applying a function to all elements of this query
    * and using the elements of the resulting queries. This corresponds to an
    * implicit inner join in SQL. */
  def flatMap[F, T, D[_]](f: E => Query[F, T, D]): Query[F, T, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(generator :: Nil).value
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
    val aliased = shaped.encodeRef(generator :: Nil)
    val fv = f(aliased.value)
    new WrappingQuery[E, U, C](Filter.ifRefutable(generator, toNode, wrapExpr(wt(fv).toNode)), shaped)
  }
  /** Select all elements of this query which satisfy a predicate. Unlike
    * `withFilter, this method only allows `Column`-valued predicates, so it
    * guards against the accidental use use plain Booleans. */
  def filter[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    withFilter(f)
  def filterNot[T <: Column[_]](f: E => T)(implicit wt: CanBeQueryCondition[T]): Query[E, U, C] =
    filterHelper(f, node => Library.Not.typed(node.nodeType, node) )

  /** Select all elements of this query which satisfy a predicate. This method
    * is used when desugaring for-comprehensions over queries. There is no
    * reason to call it directly because it is the same as `filter`. */
  def withFilter[T : CanBeQueryCondition](f: E => T) = filterHelper(f, identity)

  /** Select all elements of this query which satisfy a predicate. Unlike
    * `withilter`, this method only allows `Column`-valued predicates, so it
    * guards against the accidental use use plain Booleans. */
  @deprecated("Use `filter` instead of `where`", "2.1")
  def where[T <: Column[_] : CanBeQueryCondition](f: E => T) = filter(f)

  /** Join two collections.
    * An optional join predicate can be specified later by calling `on`. */
  def join[E2, U2, D[_]](q2: Query[E2, U2, D], jt: JoinType = JoinType.Inner) = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(leftGen :: Nil)
    val aliased2 = q2.shaped.encodeRef(rightGen :: Nil)
    new BaseJoinQuery[E, E2, U, U2, C](leftGen, rightGen, toNode, q2.toNode, jt, aliased1.zip(aliased2))
  }
  /** Join two collections with an inner join.
    * An optional join predicate can be specified later by calling `on`. */
  def innerJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Inner)
  /** Join two collections with a left outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def leftJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Left)
  /** Join two collections with a right outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def rightJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Right)
  /** Join two collections with a full outer join.
    * An optional join predicate can be specified later by calling `on`. */
  def outerJoin[E2, U2, D[_]](q2: Query[E2, U2, D]) = join(q2, JoinType.Outer)
  /** Return a query formed from this query and another query by combining
    * corresponding elements in pairs. */
  def zip[E2, U2, D[_]](q2: Query[E2, U2, D]): Query[(E, E2), (U, U2), C] = join(q2, JoinType.Zip)
  /** Return a query formed from this query and another query by combining
    * corresponding elements with the specified function. */
  def zipWith[E2, U2, F, G, T, D[_]](q2: Query[E2, U2, D], f: (E, E2) => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): Query[G, T, C] =
    join(q2, JoinType.Zip).map[F, G, T](x => f(x._1, x._2))
  /** Zip this query with its indices (starting at 0). */
  def zipWithIndex = {
    val leftGen, rightGen = new AnonSymbol
    val aliased1 = shaped.encodeRef(leftGen :: Nil)
    val aliased2 = ShapedValue(Column.forNode[Long](Ref(rightGen)), Column.columnShape[Long, FlatShapeLevel])
    new BaseJoinQuery[E, Column[Long], U, Long, C](leftGen, rightGen, toNode, RangeFrom(0L), JoinType.Zip, aliased1.zip(aliased2))
  }

  /** Sort this query according to a function which extracts the ordering
    * criteria from the query's elements. */
  def sortBy[T <% Ordered](f: E => T): Query[E, U, C] = {
    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(generator :: Nil)
    new WrappingQuery[E, U, C](SortBy(generator, toNode, f(aliased.value).columns), shaped)
  }

  /** Sort this query according to a the ordering of its elements. */
  def sorted(implicit ev: (E => Ordered)): Query[E, U, C] = sortBy(identity)

  /** Partition this query into a query of pairs of a key and a nested query
    * containing the elements for the key, according to some discriminator
    * function. */
  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): Query[(G, Query[P, U, Seq]), (T, Query[P, U, Seq]), C] = {
    val sym = new AnonSymbol
    val key = ShapedValue(f(shaped.encodeRef(sym :: Nil).value), kshape).packedValue
    val value = ShapedValue(pack.to[Seq], RepShape[FlatShapeLevel, Query[P, U, Seq], Query[P, U, Seq]])
    val group = GroupBy(sym, toNode, key.toNode)
    new WrappingQuery[(G, Query[P, U, Seq]), (T, Query[P, U, Seq]), C](group, key.zip(value))
  }

  def encodeRef(path: List[Symbol]): Query[E, U, C] = new Query[E, U, C] {
    val shaped = self.shaped.encodeRef(path)
    lazy val toNode = Path(path)
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

  def recursiveUnion[F >: E](f: E => Query[F, U, C]): Query[F, U, C] = {
    def process(outerSymbol: Symbol, toNode: Node, select: Node): Node = {
      def convertRef(outerSymbol: Symbol, toNodeSymbol: Symbol)(n: Node): Node = {
        def innerConvertRef(outerSymbol: Symbol, toNodeSymbol: Symbol)(n: Node): Node = {
          n match {
            case Ref(_) => Ref(outerSymbol)
            case Path(symbols) => Path(symbols.last :: Nil)
            case n => n
          }
        }
        innerConvertRef(outerSymbol, toNodeSymbol)(n.nodeMapChildren(convertRef(outerSymbol, toNodeSymbol)))
      }
      def renameField(numberIterator: Iterator[Int])(n: Node): Node = {
        def innerRenameField(numberIterator: Iterator[Int])(n: Node): Node = {
          n match {
            case path@Path(List(fieldSymbol, refSymbol)) => {
              val newFieldSymbol = FieldSymbol("col" + numberIterator.next())(Nil, path.nodeType)
              Path(newFieldSymbol :: refSymbol :: Nil).nodeTyped(path.nodeType)
            }
            case n => n
          }
        }
        innerRenameField(numberIterator)(n.nodeMapChildren(renameField(numberIterator))).nodeTypedOrCopy(n.nodeType)
      }
      def createFakeTableExpansion(outerSymbol: Symbol, columnNodes: List[Node], tableName: String): TableExpansion = {
        def numberStreamSeed(i: Int): Stream[Int] = Stream.cons(i, numberStreamSeed(i + 1))
        val numberIterator: Iterator[Int] = numberStreamSeed(1).toIterator
        val renamedColumnNodes = columnNodes.map(x => renameField(numberIterator)(x.nodeMapChildren(renameField(numberIterator))))
        val productNode = ProductNode(renamedColumnNodes) //.withComputedTypeNoRec
        val columnTypes = renamedColumnNodes.map {
            case path@Path(symbols) => (symbols.head, path.nodeType)
            case literalNode: LiteralNode => (FieldSymbol("col" + numberIterator.next())(Nil, literalNode.nodeType), literalNode.nodeType)
          }.toIndexedSeq
        val structType = StructType(columnTypes)
        val simpleTableIdentitySymbol = SimpleTableIdentitySymbol(null, None.getOrElse("_"), tableName)
        val tableNode = SyntheticTableNode(tableName, simpleTableIdentitySymbol, structType)
        TableExpansion(outerSymbol, tableNode, productNode)
      }
      def convertRecursiveRefInSelectToNewBind(outerSymbol: Symbol, newBind: Bind)(n: Node): Node = {
        def innerConvertRecursiveRefToFakeTableExpansion(outerSymbol: Symbol, newBind: Bind)(n: Node): Node = {
          n match {
            case Pure(Ref(refSymbol), _) if refSymbol == outerSymbol => newBind
            case pure@Pure(ProductNode(columnNodes), _)
              if columnNodes.forall { case Path(symbols) => symbols.last == outerSymbol; case n => true} => newBind
            case n => n
          }
        }
        innerConvertRecursiveRefToFakeTableExpansion(outerSymbol, newBind)(n.nodeMapChildren(convertRecursiveRefInSelectToNewBind(outerSymbol, newBind))).nodeTypedOrCopy(n.nodeType)
      }
      def createWithClauseNode(columnNodes: List[Node], outerBind: Bind): WithClauseNode = {
        val newFakeTableExpansion = createFakeTableExpansion(outerSymbol, columnNodes, "with" + Random.nextInt())

        val newBindSymbol = new AnonSymbol
        val newBindColumns =
          newFakeTableExpansion.table.nodeType.asInstanceOf[CollectionType].
            elementType.asInstanceOf[NominalType].
            structuralView.asInstanceOf[StructType].
            elements.map {
            case (symbol, nodeType) => Select(Ref(newBindSymbol), symbol).nodeTypedOrCopy(nodeType)
          }
        val newBindSelect = ProductNode(newBindColumns) //.withComputedTypeNoRec
        val newBind = new Bind(newBindSymbol, newFakeTableExpansion, Pure(newBindSelect) /*.withComputedTypeNoRec*/)

        val selectAsBind = select.asInstanceOf[Bind]
        val newSelect = selectAsBind.copy(from = convertRecursiveRefInSelectToNewBind(outerSymbol, newBind)(selectAsBind.from))
        val newSelectOfNewSelect =
          if (newBindColumns.length == 1) {
            newSelect.select match {
              case Pure(Ref(refSymbol), _) if refSymbol == newSelect.generator => {
                Pure(
                  ProductNode(
                    Seq(
                      newBindColumns.head.copy(
                        Ref(newSelect.generator),
                        ElementSymbol(1)
                      ).nodeTypedOrCopy(newBindColumns.head.nodeType))
                  )
                )
              }
              case x => x
            }
          }
          else {
            newSelect.select
          }
        val newSelect2 = newSelect.copy(select = newSelectOfNewSelect)

        WithClauseNode(
          new AnonSymbol, newFakeTableExpansion.table,
          Union(outerBind , newSelect2, true))
      }
      def prepareForOneLiteralColumnFrom(bindSymbol: Symbol, bindFrom: Node, bindSelect: Node): Node = {
        val testOne = bindSelect match {
          case Pure(Ref(refSymbol), _) => refSymbol == bindSymbol
          case _ => false
        }
        val testTwo = bindFrom match {
          case Pure(ProductNode(_), _) => None
          case Pure(n, _) => Option(n.nodeType)
          case _ => None
        }
        testTwo.
          filter(_ => testOne).
          map(x => Pure(Select(Ref(bindSymbol), ElementSymbol(1)).nodeTypedOrCopy(x)).withComputedTypeNoRec).
          getOrElse(bindSelect)
      }
      def prepareBindSelectIfBindFromIsWithClauseNode(bindSymbol: Symbol, bindFrom: Node, bindSelect: Node): List[Node] = {
        def assignNodeTypeIfThereIsNoType(columnTypes: Map[Node, Type])(n: Node): Node = {
          def innerAssignNodeTypeIfThereIsNoType(columnTypes: Map[Node, Type])(n: Node): Node = {
            n match {
              case select: Select if columnTypes.contains(select) => {
                select.nodeTypedOrCopy(columnTypes.apply(select))
              }
              case pure: Pure => pure //.withComputedTypeNoRec
              case productNode: ProductNode => productNode //.withComputedTypeNoRec
              case x => x
            }
          }
          innerAssignNodeTypeIfThereIsNoType(columnTypes)(n.nodeMapChildren(assignNodeTypeIfThereIsNoType(columnTypes)))
        }
        val preparedBindSelectIfBindFromIsWithClauseNode =
          bindFrom match {
            case innerWithClauseNode: WithClauseNode => {
              val columnTypes: Map[Node, Type] =
                innerWithClauseNode.nodeType. // CollectionType
                  children.head. // NominalType
                  children.head. // StructType
                  children.
                  zipWithIndex.map {
                  case (eachType, idx) => (Select(Ref(bindSymbol), ElementSymbol(idx + 1)), eachType)
                }.toMap
              assignNodeTypeIfThereIsNoType(columnTypes)(bindSelect)
            }
            case _ => bindSelect
          }
        preparedBindSelectIfBindFromIsWithClauseNode match {
          case Pure(ProductNode(innerColumnNodes), _) => {
            innerColumnNodes.map(x => x.nodeMapChildren(convertRef(outerSymbol, bindSymbol))).toList
          }
          case Pure(innerPath@Path(_), _) => {
            Seq(innerPath).map(x => x.nodeMapChildren(convertRef(outerSymbol, bindSymbol))).toList
          }
        }
      }
      toNode match {
        case bind@Bind(bindSymbol, bindFrom, bindPure) => {
          val newBindPure = prepareForOneLiteralColumnFrom(bindSymbol, bindFrom, bindPure)
          val columnNodes2 = prepareBindSelectIfBindFromIsWithClauseNode(bindSymbol, bindFrom, newBindPure)
          val columnNodes3 = bindFrom match {
            case Pure(ProductNode(fromNodes), _) => {
              fromNodes.zip(columnNodes2).
                map { case (a, b) => b.nodeTypedOrCopy(a.nodeType)}.
                map(x => x.nodeMapChildren(convertRef(outerSymbol, bindSymbol))).toList
            }
            case _ => columnNodes2
          }
          createWithClauseNode(columnNodes3, bind)
        }
      }
    }

    val generator = new AnonSymbol
    val aliased = shaped.encodeRef(generator :: Nil).value
    val fv = f(aliased)
    new WrappingQuery[F, U, C](process(generator, toNode, fv.toNode), fv.shaped)
  }

  /** The total number of elements (i.e. rows). */
  def length: Column[Int] = Library.CountAll.column(toNode)
  /** The total number of elements (i.e. rows). */
  def size = length

  /** The number of distinct elements of the query. */
  def countDistinct: Column[Int] = Library.CountDistinct.column(toNode)

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

  def to[D[_]](implicit ctc: TypedCollectionTypeConstructor[D]): Query[E, U, D] = new Query[E, U, D] {
    val shaped = self.shaped
    def toNode = CollectionCast(self.toNode, ctc)
  }
}

/** The companion object for Query contains factory methods for creating queries. */
object Query {
  /** Lift a scalar value to a Query. */
  def apply[E, U, R](value: E)(implicit unpack: Shape[_ <: FlatShapeLevel, E, U, R]): Query[R, U, Seq] = {
    val shaped = ShapedValue(value, unpack).packedValue
    new WrappingQuery[R, U, Seq](Pure(shaped.toNode), shaped)
  }

  @deprecated("Use Query.apply instead of Query.pure", "2.1")
  def pure[E, U, R](value: E)(implicit unpack: Shape[_ <: FlatShapeLevel, E, U, R]): Query[R, U, Seq] =
    apply[E, U, R](value)

  /** The empty Query. */
  def empty: Query[Unit, Unit, Seq] = new Query[Unit, Unit, Seq] {
    val toNode = shaped.toNode
    def shaped = ShapedValue((), Shape.unitShape[FlatShapeLevel])
  }

  @inline implicit def queryShape[Level >: NestedShapeLevel <: ShapeLevel, T, Q <: QueryBase[_]](implicit ev: Q <:< Rep[T]) = RepShape[Level, Q, T]
}

/** A typeclass for types that can be used as predicates in `filter` calls. */
@implicitNotFound("Type ${T} cannot be a query condition (only Boolean, Column[Boolean] and Column[Option[Boolean]] are allowed")
trait CanBeQueryCondition[-T] extends (T => Column[_])

object CanBeQueryCondition {
  // Using implicits with explicit type annotation here (instead of previously implicit objects)
  // because otherwise they would not be found in this file above this line. 
  // See https://github.com/slick/slick/pull/217
  implicit val BooleanColumnCanBeQueryCondition : CanBeQueryCondition[Column[Boolean]] =
    new CanBeQueryCondition[Column[Boolean]] {
      def apply(value: Column[Boolean]) = value
    }
  implicit val BooleanOptionColumnCanBeQueryCondition : CanBeQueryCondition[Column[Option[Boolean]]] =
    new CanBeQueryCondition[Column[Option[Boolean]]] {
      def apply(value: Column[Option[Boolean]]) = value
    }
  implicit val BooleanCanBeQueryCondition : CanBeQueryCondition[Boolean] =
    new CanBeQueryCondition[Boolean] {
      def apply(value: Boolean) = new LiteralColumn(value)
    }
}

class WrappingQuery[+E, U, C[_]](val toNode: Node, val shaped: ShapedValue[_ <: E, U]) extends Query[E, U, C]

final class BaseJoinQuery[+E1, +E2, U1, U2, C[_]](leftGen: Symbol, rightGen: Symbol, left: Node, right: Node, jt: JoinType, base: ShapedValue[_ <: (E1, E2), (U1, U2)])
    extends WrappingQuery[(E1, E2), (U1,  U2), C](AJoin(leftGen, rightGen, left, right, jt, LiteralNode(true)), base) {
  /** Add a join condition to a join operation. */
  def on[T <: Column[_]](pred: (E1, E2) => T)(implicit wt: CanBeQueryCondition[T]): Query[(E1, E2), (U1, U2), C] =
    new WrappingQuery[(E1, E2), (U1, U2), C](AJoin(leftGen, rightGen, left, right, jt, wt(pred(base.value._1, base.value._2)).toNode), base)
}

/** Represents a database table. Profiles add extension methods to TableQuery
  * for operations that can be performed on tables but not on arbitrary
  * queries, e.g. getting the table DDL. */
class TableQuery[E <: AbstractTable[_]](cons: Tag => E) extends Query[E, E#TableElementType, Seq] {
  lazy val shaped = {
    val baseTable = cons(new BaseTag { base =>
      def taggedAs(path: List[Symbol]): AbstractTable[_] = cons(new RefTag(path) {
        def taggedAs(path: List[Symbol]) = base.taggedAs(path)
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
      List(ValDef(Modifiers(Flag.PARAM), newTermName("tag"), Ident(typeOf[Tag].typeSymbol), EmptyTree)),
      Apply(
        Select(New(TypeTree(e.tpe)), nme.CONSTRUCTOR),
        List(Ident(newTermName("tag")))
      )
    ))
    reify { TableQuery.apply[E](cons.splice) }
  }
}
