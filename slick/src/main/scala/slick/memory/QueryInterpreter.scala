package slick.memory

import java.util.regex.Pattern
import org.slf4j.LoggerFactory
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import slick.ast._
import slick.SlickException
import slick.util.{ConstArray, SlickLogger, Logging}
import TypeUtil.typeToTypeUtil

/** A query interpreter for MemoryProfile and for client-side operations
  * that need to be run as part of distributed queries against multiple
  * backends.
  *
  * It uses ScalaType, ProductValue/StructValue and plain Scala collections to
  * represent data. Queries are expected to be in the shape after running all
  * the standard query compiler phases (but not the extra relational phases)
  * and assigning ScalaTypes everywhere.
  *
  * @param db The in-memory database which is used for resolving Tables
  * @param params The query parameters
  */
class QueryInterpreter(db: HeapBackend#Database, params: Any) extends Logging {
  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[QueryInterpreter]))
  import QueryInterpreter._

  val scope = new HashMap[TermSymbol, Any]
  var indent = 0
  type Coll = Iterable[Any]

  def logDebug(msg: String): Unit = {
    logger.debug(Iterator.fill(indent)("  ").mkString("", "", msg))
  }

  def run(n: Node): Any = {
    if(logger.isDebugEnabled) logDebug("Evaluating "+n)
    indent += 1
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
        new StructValue(n.children.toSeq.map(run), n.nodeType.asInstanceOf[StructType].symbolToIndex)
      case ProductNode(ch) =>
        new ProductValue(ch.map(run).toSeq)
      case Pure(n, _) => Vector(run(n))
      case t: TableNode =>
        val dbt = db.getTable(t.tableName)
        val acc = dbt.columnIndexes
        dbt.rows.view.map { row => new StructValue(row, acc) }
      case Bind(gen, from, sel) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.iterableSubstitute.createBuilder[Any]
        fromV.foreach { v =>
          scope(gen) = v
          b ++= run(sel).asInstanceOf[Coll]
        }
        scope.remove(gen)
        b.result()
      case Join(_, _, left, RangeFrom(0), JoinType.Zip, LiteralNode(true)) =>
        val leftV = run(left).asInstanceOf[Coll]
        leftV.zipWithIndex.map { case (l, r) => new ProductValue(Vector(l, r.toLong)) }
      case Join(_, _, left, right, JoinType.Zip, LiteralNode(true)) =>
        val leftV = run(left).asInstanceOf[Coll]
        val rightV = run(right).asInstanceOf[Coll]
        (leftV, rightV).zipped.map { (l, r) => new ProductValue(Vector(l, r)) }
      case Join(leftGen, rightGen, left, right, JoinType.Inner, by) =>
        val res = run(left).asInstanceOf[Coll].flatMap { l =>
          scope(leftGen) = l
          run(right).asInstanceOf[Coll].filter { r =>
            scope(rightGen) = r
            asBoolean(run(by))
          }.map { r =>
            new ProductValue(Vector(l, r))
          }
        }
        scope.remove(leftGen)
        scope.remove(rightGen)
        res
      case Join(leftGen, rightGen, left, right, JoinType.Left, by) =>
        val res = run(left).asInstanceOf[Coll].flatMap { l =>
          scope(leftGen) = l
          val inner = run(right).asInstanceOf[Coll].filter { r =>
            scope(rightGen) = r
            asBoolean(run(by))
          }.map { r =>
            new ProductValue(Vector(l, r))
          }
          if(inner.headOption.isEmpty) Vector(new ProductValue(Vector(l, createNullRow(right.nodeType.asCollectionType.elementType))))
          else inner
        }
        scope.remove(leftGen)
        scope.remove(rightGen)
        res
      case Join(leftGen, rightGen, left, right, JoinType.Right, by) =>
        val res = run(right).asInstanceOf[Coll].flatMap { r =>
          scope(rightGen) = r
          val inner = run(left).asInstanceOf[Coll].filter { l =>
            scope(leftGen) = l
            asBoolean(run(by))
          }.map { l =>
            new ProductValue(Vector(l, r))
          }
          if(inner.headOption.isEmpty) Vector(new ProductValue(Vector(createNullRow(left.nodeType.asCollectionType.elementType), r)))
          else inner
        }
        scope.remove(leftGen)
        scope.remove(rightGen)
        res
      case Join(leftGen, rightGen, left, right, JoinType.Outer, by) =>
        val leftJoinRes = run(left).asInstanceOf[Coll].flatMap { l =>
          scope(leftGen) = l
          val inner = run(right).asInstanceOf[Coll].filter { r =>
            scope(rightGen) = r
            asBoolean(run(by))
          }.map { r =>
            new ProductValue(Vector(l, r))
          }
          if(inner.headOption.isEmpty) Vector(new ProductValue(Vector(l, createNullRow(right.nodeType.asCollectionType.elementType))))
          else inner
        }
        scope.remove(leftGen)
        scope.remove(rightGen)
        val emptyRightRes = run(right).asInstanceOf[Coll].filter { r =>
          scope(rightGen) = r
          run(left).asInstanceOf[Coll].find { l =>
            scope(leftGen) = l
            asBoolean(run(by))
          }.isEmpty
        }.map { r => new ProductValue(Vector(createNullRow(left.nodeType.asCollectionType.elementType), r)) }
        scope.remove(leftGen)
        scope.remove(rightGen)
        leftJoinRes ++ emptyRightRes
      case Filter(gen, from, where) =>
        val res = run(from).asInstanceOf[Coll].filter { v =>
          scope(gen) = v
          asBoolean(run(where))
        }
        scope.remove(gen)
        res
      case First(ch) => run(ch).asInstanceOf[Coll].toIterator.next()
      case Distinct(gen, from, on) =>
        val fromV = run(from).asInstanceOf[Coll]
        val seen = mutable.HashSet[Any]()
        val res = fromV.filter { v =>
          scope(gen) = v
          val onV = run(on)
          if(seen contains onV) false
          else {
            seen += onV
            true
          }
        }
        scope.remove(gen)
        res
      case SortBy(gen, from, by) =>
        val fromV = run(from).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.iterableSubstitute.createBuilder[Any]
        val ords: IndexedSeq[scala.math.Ordering[Any]] = by.toSeq.map { case (b, o) =>
          b.nodeType.asInstanceOf[ScalaType[Any]].scalaOrderingFor(o)
        }
        b ++= fromV.toSeq.sortBy { v =>
          scope(gen) = v
          by.toSeq.map { case (b, _) => run(b) }: IndexedSeq[Any]
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
      case GroupBy(gen, from, by, _) =>
        val fromV = run(from).asInstanceOf[Coll]
        val grouped = new HashMap[Any, ArrayBuffer[Any]]()
        fromV.foreach { v =>
          scope(gen) = v
          grouped.getOrElseUpdate(run(by), new ArrayBuffer[Any]()) += v
        }
        scope.remove(gen)
        val b = from.nodeType.asCollectionType.cons.iterableSubstitute.createBuilder[Any]
        grouped.foreach { case (k, vs) => b += new ProductValue(Vector(k, vs)) }
        b.result()
      case Take(from, num) =>
        val fromV = run(from).asInstanceOf[Coll]
        val numV = run(num).asInstanceOf[Long]
        val b = from.nodeType.asCollectionType.cons.iterableSubstitute.createBuilder[Any]
        b ++= fromV.toIterator.take(numV.toInt)
        b.result()
      case Drop(from, num) =>
        val fromV = run(from).asInstanceOf[Coll]
        val numV = run(num).asInstanceOf[Long]
        val b = from.nodeType.asCollectionType.cons.iterableSubstitute.createBuilder[Any]
        b ++= fromV.toIterator.drop(numV.toInt)
        b.result()
      case Union(left, right, all) =>
        val leftV = run(left).asInstanceOf[Coll]
        val rightV = run(right).asInstanceOf[Coll]
        if(all) leftV ++ rightV
        else leftV ++ {
          val s = leftV.toSet
          rightV.filter(e => !s.contains(e))
        }
      case GetOrElse(ch, default) =>
        run(ch).asInstanceOf[Option[Any]].getOrElse(default())
      case OptionApply(ch) =>
        Option(run(ch))
      case c: IfThenElse =>
        val opt = n.nodeType.asInstanceOf[ScalaType[_]].nullable
        val take = c.ifThenClauses.find { case (pred, _) => asBoolean(run(pred)) }
        take match {
          case Some((_, r)) =>
            val res = run(r)
            if(opt && !r.nodeType.asInstanceOf[ScalaType[_]].nullable) Option(res)
            else res
          case _ =>
            val res = run(c.elseClause)
            if(opt && !c.elseClause.nodeType.asInstanceOf[ScalaType[_]].nullable) Option(res)
            else res
        }
      case QueryParameter(extractor, _, _) =>
        extractor(params)
      case Library.SilentCast(ch) =>
        val chV = run(ch)
        (ch.nodeType, n.nodeType) match {
          case (OptionType(tpe), tpe2) if tpe == tpe2 => chV.asInstanceOf[Option[Any]].get
          case (tpe, OptionType(tpe2)) if tpe == tpe2 => Option(chV)
        }
      case Library.Exists(coll) =>
        !run(coll).asInstanceOf[Coll].isEmpty
      case Library.IfNull(cond, default) =>
        val condV = run(cond)
        if((condV.asInstanceOf[AnyRef] eq null) || condV == None) {
          val defaultV = run(default)
          if(n.nodeType.isInstanceOf[OptionType] && !default.nodeType.isInstanceOf[OptionType]) Some(defaultV)
          else defaultV
        } else if(n.nodeType.isInstanceOf[OptionType] && !cond.nodeType.isInstanceOf[OptionType]) Some(condV)
        else condV
      case Library.In(what, where) =>
        val whatV = run(what)
        val whereV = run(where)
        val whatOpt = what.nodeType.isInstanceOf[OptionType]
        if(whatOpt && (whatV.asInstanceOf[AnyRef].eq(null) || whatV == None)) None
        else {
          val whatBase = if(whatOpt) whatV.asInstanceOf[Option[Any]].get else whatV
          where.nodeType match {
            case ProductType(elTypes) =>
              val p = whereV.asInstanceOf[ProductValue]
              0.until(elTypes.length).iterator.map { i =>
                if(elTypes(i).isInstanceOf[OptionType]) {
                  p(i).asInstanceOf[Option[Any]] match {
                    case Some(v) => whatBase == v
                    case None => false
                  }
                } else whatBase == p(i)
              } contains true
            case ct: CollectionType =>
              val (els, singleType) = unwrapSingleColumn(whereV.asInstanceOf[Coll], ct)
              (if(singleType.isInstanceOf[OptionType])
                els.map(_.asInstanceOf[Option[Any]] match {
                  case Some(v) => whatBase == v
                  case None => false
                })
              else els.map(whatBase.==)) contains true
          }
        }
      case Library.Sum(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        reduceOptionIt[Any](it, opt, identity, (a, b) => num.plus(a, b))
      case Library.Avg(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        reduceOptionIt[(Int, Any)](it, opt, (1, _), { case ((ai, a), (bi, b)) => (ai + bi, num.plus(a, b)) }).map { case (count, sum) =>
          if(num.isInstanceOf[Fractional[_]]) num.asInstanceOf[Fractional[Any]].div(sum, num.fromInt(count))
          else num.fromInt(num.toInt(sum) / count)
        }
      case Library.Min(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (ord, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaBaseType[Any]].ordering, true)
          case t => (t.asInstanceOf[ScalaBaseType[Any]].ordering, false)
        }
        reduceOptionIt[Any](it, opt, identity, (a, b) => if(ord.lt(b, a)) b else a)
      case Library.Max(ch) =>
        val coll = run(ch).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (ord, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaBaseType[Any]].ordering, true)
          case t => (t.asInstanceOf[ScalaBaseType[Any]].ordering, false)
        }
        reduceOptionIt[Any](it, opt, identity, (a, b) => if(ord.gt(b, a)) b else a)
      case Library.==(ch, LiteralNode(null)) =>
        val chV = run(ch)
        chV == null || chV.asInstanceOf[Option[_]].isEmpty
      case Apply(sym, ch) =>
        val chV = ch.map(n => (n.nodeType, run(n)))
        logDebug("[chV: "+chV.mkString(", ")+"]")
        if(n.nodeType.isInstanceOf[OptionType]) {
          if(chV.exists { case (t, v) => t.isInstanceOf[OptionType] && (v == None) }) None
          else {
            val chPlainV = chV.map {
              case (t: OptionType, v) => (t.elementType, v.asInstanceOf[Option[Any]].get)
              case other => other
            }
            logDebug("[chPlainV: "+chPlainV.mkString(", ")+"]")
            Some(evalFunction(sym, chPlainV.toSeq, n.nodeType.asOptionType.elementType))
          }
        } else evalFunction(sym, chV.toSeq, n.nodeType)
      //case Library.CountAll(ch) => run(ch).asInstanceOf[Coll].size
      case l: LiteralNode => l.value
      case CollectionCast(ch, _) => run(ch)
      case Subquery(ch, _) => run(ch)
    }
    indent -= 1
    if(logger.isDebugEnabled) logDebug("Result: "+res)
    res
  }

  def evalFunction(sym: TermSymbol, args: Seq[(Type, Any)], retType: Type) = sym match {
    case Library.== => args(0)._2 == args(1)._2
    case Library.< => args(0)._1.asInstanceOf[ScalaBaseType[Any]].ordering.lt(args(0)._2, args(1)._2)
    case Library.<= => args(0)._1.asInstanceOf[ScalaBaseType[Any]].ordering.lteq(args(0)._2, args(1)._2)
    case Library.> => args(0)._1.asInstanceOf[ScalaBaseType[Any]].ordering.gt(args(0)._2, args(1)._2)
    case Library.>= => args(0)._1.asInstanceOf[ScalaBaseType[Any]].ordering.gteq(args(0)._2, args(1)._2)
    case Library.+ => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.plus(args(0)._2, args(1)._2)
    case Library.- => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.minus(args(0)._2, args(1)._2)
    case Library.* => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.times(args(0)._2, args(1)._2)
    case Library.% => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.asInstanceOf[Integral[Any]].rem(args(0)._2, args(1)._2)
    case Library.Abs => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.abs(args(0)._2)
    case Library.And => args(0)._2.asInstanceOf[Boolean] && args(1)._2.asInstanceOf[Boolean]
    case Library.Cast =>
      val v = args(0)._2
      (args(0)._1, retType) match {
        case (a, b) if a == b => v
        case (_, ScalaBaseType.stringType) => v.toString
        case (_, ScalaBaseType.intType) => v.toString.toInt
        case (_, ScalaBaseType.longType) => v.toString.toLong
        case (_, ScalaBaseType.byteType) => v.toString.toByte
        case (_, ScalaBaseType.shortType) => v.toString.toShort
        case (_, ScalaBaseType.doubleType) => v.toString.toDouble
        case (_, ScalaBaseType.floatType) => v.toString.toFloat
        case (_, ScalaBaseType.bigDecimalType) => BigDecimal(v.toString)
      }
    case Library.Ceiling =>
      val t = args(0)._1.asInstanceOf[ScalaNumericType[Any]]
      t.fromDouble(scala.math.ceil(t.toDouble(args(0)._2)))
    case Library.Concat => args.iterator.map(_._2.toString).mkString
    case Library.CountAll => args(0)._2.asInstanceOf[Coll].size
    case Library.Count =>
      val CollectionType(_, elType) = args(0)._1
      val coll = args(0)._2.asInstanceOf[Coll]
      (elType match {
        case ProductType(_) => coll
        case _ => coll.iterator.filter(v => v != null && v != None)
      }).size
    case Library.Database => ""
    case Library.Degrees =>
      val t = args(0)._1.asInstanceOf[ScalaNumericType[Any]]
      t.fromDouble(scala.math.toDegrees(t.toDouble(args(0)._2)))
    case Library.Floor =>
      val t = args(0)._1.asInstanceOf[ScalaNumericType[Any]]
      t.fromDouble(scala.math.floor(t.toDouble(args(0)._2)))
    case Library.LCase => args(0)._2.asInstanceOf[String].toLowerCase
    case Library.Length => args(0)._2.asInstanceOf[String].length
    case Library.Like =>
      val pat = compileLikePattern(args(1)._2.toString, if(args.length > 2) Some(args(2)._2.toString.charAt(0)) else None)
      val mat = pat.matcher(args(0)._2.toString())
      mat.matches()
    case Library.LTrim =>
      val s = args(0)._2.asInstanceOf[String]
      val len = s.length
      var start = 0
      while(start < len && s.charAt(start) == ' ') start += 1
      if(start == 0) s else s.substring(start)
    case Library.Not => !args(0)._2.asInstanceOf[Boolean]
    case Library.Or => args(0)._2.asInstanceOf[Boolean] || args(1)._2.asInstanceOf[Boolean]
    case Library.Pi => scala.math.Pi
    case Library.Radians =>
      val t = args(0)._1.asInstanceOf[ScalaNumericType[Any]]
      t.fromDouble(scala.math.toRadians(t.toDouble(args(0)._2)))
    case Library.RTrim =>
      val s = args(0)._2.asInstanceOf[String]
      var len = s.length
      while(len > 0 && s.charAt(len-1) == ' ') len -= 1
      if(len == s.length) s else s.substring(0, len)
    case Library.Sign => args(0)._1.asInstanceOf[ScalaNumericType[Any]].numeric.signum(args(0)._2)
    case Library.Trim => args(0)._2.asInstanceOf[String].trim
    case Library.UCase => args(0)._2.asInstanceOf[String].toUpperCase
    case Library.User => ""
    case Library.Substring if args.size == 2 => 
      args(0)._2.asInstanceOf[String].substring(args(1)._2.asInstanceOf[Int])
    case Library.Repeat if args.size == 2
      => args(0)._2.asInstanceOf[String] * args(1)._2.asInstanceOf[Int]
    case Library.Substring if args.size == 3 => args(0)._2.asInstanceOf[String].
      substring(args(1)._2.asInstanceOf[Int], args(2)._2.asInstanceOf[Int])
    case Library.Replace => args(0)._2.asInstanceOf[String].
      replace(args(1)._2.asInstanceOf[String], args(2)._2.asInstanceOf[String])
    case Library.Reverse => args(0)._2.asInstanceOf[String].reverse
    case Library.IndexOf => args(0)._2.asInstanceOf[String].indexOf(args(1)._2.asInstanceOf[String])
    case Library.StartsWith => args(0)._2.asInstanceOf[String].startsWith(args(1)._2.asInstanceOf[String])
    case Library.EndsWith => args(0)._2.asInstanceOf[String].endsWith(args(1)._2.asInstanceOf[String])
  }

  def unwrapSingleColumn(coll: Coll, tpe: Type): (Iterator[Any], Type) = tpe.asCollectionType.elementType match {
    case ProductType(ConstArray(t)) => (coll.iterator.map(_.asInstanceOf[ProductValue](0)), t)
    case StructType(ConstArray((_, t))) => (coll.iterator.map(_.asInstanceOf[StructValue](0)), t)
    case t => (coll.iterator, t)
  }

  def reduceOptionIt[T](it: Iterator[Any], opt: Boolean, map: Any => T, reduce: (T, T) => T): Option[T] = {
    if(!it.hasNext) None
    else {
      val it2 = if(opt) it.collect { case Some(b) => b} else it
      var res: T = null.asInstanceOf[T]
      var first = true
      it2.foreach { b =>
        if(first) {
          first = false
          res = map(b)
        } else res = reduce(res, map(b))
      }
      if(first) None else Some(res)
    }
  }

  def createNullRow(tpe: Type): Any = tpe match {
    case t: ScalaType[_] => if(t.nullable) None else null
    case StructType(el) =>
      new StructValue(el.toSeq.map{ case (_, tpe) => createNullRow(tpe) },
        el.toSeq.zipWithIndex.map{ case ((sym, _), idx) => (sym, idx) }(collection.breakOut): Map[TermSymbol, Int])
    case ProductType(el) =>
      new ProductValue(el.toSeq.map(tpe => createNullRow(tpe)))
  }

  def asBoolean(v: Any) = v match {
    case b: Boolean => b
    case Some(b: Boolean) => b
    case None => false
    case null => false
  }

  def compileLikePattern(s: String, escape: Option[Char]): Pattern = {
    val b = new StringBuilder append '^'
    val len = s.length
    val esc = escape.getOrElse('\u0000')
    var i = 0
    while(i < len) {
      s.charAt(i) match {
        case e if e == esc =>
          i += 1
          b.append(Pattern.quote(String.valueOf(s.charAt(i))))
        case '%' => b.append(".*")
        case c => b.append(Pattern.quote(String.valueOf(c)))
      }
      i += 1
    }
    Pattern.compile(b.append('$').toString)
  }
}

object QueryInterpreter {
  /** The representation for ProductType values in the interpreter */
  class ProductValue(private val data: IndexedSeq[Any]) extends (Int => Any) {
    def length: Int = data.length
    def apply(idx: Int): Any = data(idx)
    override def toString = "ProductValue("+data.mkString(", ")+")"
    override def equals(other: Any) = other match {
      case p: ProductValue => data == p.data
      case _ => false
    }
    override def hashCode = data.hashCode()
  }

  /** The representation for StructType values in the interpreter */
  class StructValue(data: IndexedSeq[Any], symbolToIndex: (TermSymbol => Int)) extends ProductValue(data) {
    def getBySymbol(sym: TermSymbol): Any = apply(symbolToIndex(sym))
    override def toString = "StructValue("+data.mkString(", ")+"){"+symbolToIndex+"}"
  }
}
