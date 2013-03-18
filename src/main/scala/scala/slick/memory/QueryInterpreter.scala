package scala.slick.memory

import org.slf4j.LoggerFactory
import scala.collection.mutable.HashMap
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.util.{SlickLogger, Logging}
import TypeUtil.typeToTypeUtil

/** A query interpreter for the MemoryDriver and for client-side operations
  * that need to be run as part of distributed queries against multiple
  * backends.
  *
  * It uses ScalaType, ProductValue/StructValue and plain Scala collections to
  * represent data. Queries are expected to be in the shape after running all
  * the standard query compiler phases (but not the extra relational phases)
  * and assigning ScalaTypes everywhere.
  *
  * @param db The in-memory database which is used for resolving Tables
  */
class QueryInterpreter(db: HeapBackend#Database) extends Logging {
  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[QueryInterpreter]))
  import QueryInterpreter._

  val scope = new HashMap[Symbol, Any]
  type Coll = Iterable[Any]

  def run(n: Node): Any = {
    logger.debug("Evaluating "+n)
    val res = n match {
      case Ref(sym) =>
        scope.getOrElse(sym, throw new SlickException(s"Symbol $sym not found in scope"))
      case Select(in, field) =>
        val v = run(in)
        field match {
          case ElementSymbol(idx) => v.asInstanceOf[ProductValue].apply(idx-1)
          case (_: AnonSymbol | _: FieldSymbol) => v.asInstanceOf[StructValue].getBySymbol(field)
        }
      case n: StructNode =>
        new StructValue(n.nodeChildren.map(run), n.nodeType.asInstanceOf[StructType].symbolToIndex)
      case ProductNode(ch) =>
        new ProductValue(ch.map(run).toIndexedSeq)
      case Pure(n) => Vector(run(n))
      case t: TableNode =>
        val dbt = db.getTable(t.tableName)
        val acc = t.nodeType.asCollectionType.elementType.asInstanceOf[StructType].symbolToIndex
        dbt.rows.view.map { row => new StructValue(row, acc) }
      case Bind(gen, from, sel) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        fromV.foreach { v =>
          scope(gen) = v
          b ++= run(sel).asInstanceOf[Coll]
        }
        scope.remove(gen)
        b.result()
      case Filter(gen, from, where) =>
        val res = run(from).asInstanceOf[Coll].filter { v =>
          scope(gen) = v
          run(where) match {
            case b: Boolean => b
            case Some(b: Boolean) => b
            case None => false
          }
        }
        scope.remove(gen)
        res
      case First(ch) => run(ch).asInstanceOf[Coll].toIterator.next()
      case SortBy(gen, from, by) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        val ords: IndexedSeq[scala.math.Ordering[Any]] = by.map { case (b, o) =>
          b.nodeType.asInstanceOf[ScalaType[Any]].scalaOrderingFor(o)
        }(collection.breakOut)
        b ++= fromV.toSeq.sortBy { v =>
          scope(gen) = v
          by.map { case (b, _) => run(b) }(collection.breakOut): IndexedSeq[Any]
        }(new scala.math.Ordering[IndexedSeq[Any]] {
          def compare(x: IndexedSeq[Any], y: IndexedSeq[Any]): Int = {
            var i = 0
            while(i < ords.length) {
              val v = ords(i).compare(x(i), y(i))
              if(v != 0) return v
              i += 1
            }
            0
          }
        })
        scope.remove(gen)
        b.result()
      case GroupBy(gen, _, from, by) =>
        val fromV = run(from).asInstanceOf[Coll]
        val grouped = fromV.toSeq.groupBy { v =>
          scope(gen) = v
          run(by)
        }
        scope.remove(gen)
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        grouped.foreach { case (k, vs) =>
          b += new ProductValue(Vector(k, vs))
        }
        b.result()
      case Take(from, num, _) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        b ++= fromV.toIterator.take(num)
        b.result()
      case Drop(from, num, _) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        b ++= fromV.toIterator.drop(num)
        b.result()
      case Library.Sum(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        foldOptionIt(it, opt, num.zero, (a, b) => num.plus(a, b))
      case Library.Avg(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        foldOptionIt(it, opt, num.zero, (a, b) => num.plus(a, b)).map { sum =>
          if(num.isInstanceOf[Fractional[_]]) num.asInstanceOf[Fractional[Any]].div(sum, num.fromInt(coll.size))
          else num.fromInt(num.toInt(sum) / coll.size)
        }
      case Library.Min(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        foldOptionIt(it, opt, num.zero, (a, b) => if(num.lt(b, a)) b else a)
      case Apply(sym, ch) =>
        val chV = ch.map(n => (n.nodeType, run(n)))
        // Use ternary logic for function calls
        if(n.nodeType.isInstanceOf[OptionType]) {
          if(chV.exists { case (t, v) => t.isInstanceOf[OptionType] && (v == None) }) None
          else {
            val chPlainV = chV.map {
              case (t: OptionType, v) => (t.elementType, v.asInstanceOf[Option[Any]].get)
              case other => other
            }
            Some(evalFunction(sym, chPlainV))
          }
        } else evalFunction(sym, chV)
      //case Library.CountAll(ch) => run(ch).asInstanceOf[Coll].size
      case l: LiteralNode => l.value
    }
    logger.debug(s"  Result: $res")
    res
  }

  def evalFunction(sym: Symbol, args: Seq[(Type, Any)]) = sym match {
    case Library.CountAll => args(0)._2.asInstanceOf[Coll].size
    case Library.< => args(0)._1.asInstanceOf[ScalaBaseType[Any]].ordering.lt(args(0)._2, args(1)._2)
    case Library.== => args(0)._2 == args(1)._2
    case Library.+ => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.plus(args(0)._2, args(1)._2)
  }

  def unwrapSingleColumn(coll: Coll, tpe: Type): (Iterator[Any], Type) = tpe.asCollectionType.elementType match {
    case ProductType(Seq(t)) => (coll.iterator.map(_.asInstanceOf[ProductValue](0)), t)
    case t => (coll.iterator, t)
  }

  def foldOptionIt(it: Iterator[Any], opt: Boolean, zero: Any, f: (Any, Any) => Any): Option[Any] = {
    if(!it.hasNext) None
    else if(opt) it.foldLeft(Some(zero): Option[Any]) { (z, b) =>
      for(z <- z; b <- b.asInstanceOf[Option[Any]]) yield f(z, b)
    }
    else Some(it.foldLeft(zero) { (z, b) => f(z, b) })
  }
}

object QueryInterpreter {
  /** The representation for ProductType values in the interpreter */
  class ProductValue(data: IndexedSeq[Any]) extends (Int => Any) {
    def length: Int = data.length
    def apply(idx: Int): Any = data(idx)
    override def toString = "ProductValue("+data.mkString(", ")+")"
  }

  /** The representation for StructType values in the interpreter */
  class StructValue(data: IndexedSeq[Any], symbolToIndex: (Symbol => Int)) extends ProductValue(data) {
    def getBySymbol(sym: Symbol): Any = apply(symbolToIndex(sym))
    override def toString = "StructValue("+data.mkString(", ")+"){"+symbolToIndex+"}"
  }
}
