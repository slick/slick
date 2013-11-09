package scala.slick.memory

import org.slf4j.LoggerFactory
import scala.collection.mutable.{ArrayBuffer}
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.util.{SlickLogger, Logging}
import TypeUtil.typeToTypeUtil
import java.util.regex.Pattern

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
  * @param params The query parameters
  */
class QueryInterpreter(db: HeapBackend#Database, params: Any) extends Logging {
  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[QueryInterpreter]))
  import QueryInterpreter._

  var indent = 0
  type Coll = Iterable[Any]

  def logDebug(msg: String) {
    logger.debug(Iterator.fill(indent)("  ").mkString("", "", msg))
  }

  type Scope = collection.immutable.HashMap[Symbol,Any]

  /** interpret a node */
  final def run(n: Node) = runScoped(n, new Scope())
  /** interpet this node passing along a scope
   *  (use only this method in the recursive interpretation, not run)
   */
  protected def runScoped(n: Node, scope: Scope): Any = {
    if(logger.isDebugEnabled) logDebug("Evaluating "+n)
    indent += 1
    val res = n match {
      case Ref(sym) =>
        scope.getOrElse(sym, throw new SlickException(s"Symbol $sym not found in scope"))
      case Select(in, field) =>
        val v = runScoped(in, scope)
        field match {
          case ElementSymbol(idx) => v.asInstanceOf[ProductValue].apply(idx-1)
          case (_: AnonSymbol | _: FieldSymbol) => v.asInstanceOf[StructValue].getBySymbol(field)
        }
      case n: StructNode =>
        new StructValue(n.nodeChildren.map(runScoped(_,scope)), n.nodeType.asInstanceOf[StructType].symbolToIndex)
      case ProductNode(ch) =>
        new ProductValue(ch.map(runScoped(_,scope)).toIndexedSeq)
      case Pure(n, _) => Vector(runScoped(n, scope))
      case t: TableNode =>
        val dbt = db.getTable(t.tableName)
        val acc = dbt.columnIndexes
        dbt.rows.view.map { row => new StructValue(row, acc) }
      case Bind(gen, from, sel) =>
        val fromV = runScoped(from, scope).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        fromV.foreach { v =>
          b ++= runScoped(sel, scope+(gen->v)).asInstanceOf[Coll]
        }
        b.result()
      case Join(_, _, left, RangeFrom(0), JoinType.Zip, LiteralNode(true)) =>
        val leftV = runScoped(left, scope).asInstanceOf[Coll]
        leftV.zipWithIndex.map { case (l, r) => new ProductValue(Vector(l, r)) }
      case Join(_, _, left, right, JoinType.Zip, LiteralNode(true)) =>
        val leftV = runScoped(left, scope).asInstanceOf[Coll]
        val rightV = runScoped(right, scope).asInstanceOf[Coll]
        (leftV, rightV).zipped.map { (l, r) => new ProductValue(Vector(l, r)) }
      case Join(leftGen, rightGen, left, right, joinType@(JoinType.Inner|JoinType.Left), by) =>
        runScoped(left, scope).asInstanceOf[Coll].flatMap { l =>
          val inner = runScoped(right, scope + (leftGen->l)).asInstanceOf[Coll].filter { r =>
            asBoolean(runScoped(by, scope + (leftGen->l) + (rightGen->r)))
          }.map { r =>
            new ProductValue(Vector(l, r))
          }
          if(joinType == JoinType.Left && inner.headOption.isEmpty) Vector(new ProductValue(Vector(l, createNullRow(right.nodeType.asCollectionType.elementType))))
          else inner
        }
      case Join(leftGen, rightGen, left, right, JoinType.Right, by) =>
        runScoped(right, scope).asInstanceOf[Coll].flatMap { r =>
          val inner = runScoped(left, scope + (rightGen->r)).asInstanceOf[Coll].filter { l =>
            asBoolean(runScoped(by, scope + (leftGen->l) + (rightGen->r)))
          }.map { l =>
            new ProductValue(Vector(l, r))
          }
          if(inner.headOption.isEmpty) Vector(new ProductValue(Vector(createNullRow(left.nodeType.asCollectionType.elementType), r)))
          else inner
        }
      case Filter(gen, from, where) =>
        runScoped(from, scope).asInstanceOf[Coll].filter { v =>
          asBoolean(runScoped(where, scope + (gen -> v)))
        }
      case First(ch) => runScoped(ch, scope).asInstanceOf[Coll].toIterator.next()
      case SortBy(gen, from, by) =>
        val fromV = runScoped(from, scope).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        val ords: IndexedSeq[scala.math.Ordering[Any]] = by.map { case (b, o) =>
          b.nodeType.asInstanceOf[ScalaType[Any]].scalaOrderingFor(o)
        }(collection.breakOut)
        b ++= fromV.toSeq.sortBy { v =>
          by.map { case (b, _) => runScoped(b, scope + (gen -> v)) }(collection.breakOut): IndexedSeq[Any]
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
        b.result()
      case GroupBy(gen, from, by) =>
        val fromV = runScoped(from, scope).asInstanceOf[Coll]
        val grouped = new collection.mutable.HashMap[Any, ArrayBuffer[Any]]()
        fromV.foreach { v =>
          grouped.getOrElseUpdate(runScoped(by, scope + (gen -> v)), new ArrayBuffer[Any]()) += v
        }
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        grouped.foreach { case (k, vs) => b += new ProductValue(Vector(k, vs)) }
        b.result()
      case Take(from, num) =>
        val fromV = runScoped(from, scope).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        b ++= fromV.toIterator.take(num)
        b.result()
      case Drop(from, num) =>
        val fromV = runScoped(from, scope).asInstanceOf[Coll]
        val b = from.nodeType.asCollectionType.cons.canBuildFrom()
        b ++= fromV.toIterator.drop(num)
        b.result()
      case Union(left, right, all, _, _) =>
        val leftV = runScoped(left, scope).asInstanceOf[Coll]
        val rightV = runScoped(right, scope).asInstanceOf[Coll]
        if(all) leftV ++ rightV
        else leftV ++ {
          val s = leftV.toSet
          rightV.filter(e => !s.contains(e))
        }
      case GetOrElse(ch, default) =>
        runScoped(ch, scope).asInstanceOf[Option[Any]].getOrElse(default())
      case OptionApply(ch) =>
        Option(runScoped(ch, scope))
      case ConditionalExpr(clauses, elseClause) =>
        val opt = n.nodeType.asInstanceOf[ScalaType[_]].nullable
        val take = clauses.find { case IfThen(pred, _) => asBoolean(runScoped(pred, scope)) }
        take match {
          case Some(IfThen(_, r)) =>
            val res = runScoped(r, scope)
            if(opt && !r.nodeType.asInstanceOf[ScalaType[_]].nullable) Option(res)
            else res
          case _ =>
            val res = runScoped(elseClause, scope)
            if(opt && !elseClause.nodeType.asInstanceOf[ScalaType[_]].nullable) Option(res)
            else res
        }
      case QueryParameter(extractor, _) =>
        extractor(params)
      case Library.Exists(coll) =>
        !runScoped(coll, scope).asInstanceOf[Coll].isEmpty
      case Library.IfNull(cond, default) =>
        val condV = runScoped(cond, scope)
        if((condV.asInstanceOf[AnyRef] eq null) || condV == None) {
          val defaultV = runScoped(default, scope)
          if(n.nodeType.isInstanceOf[OptionType] && !default.nodeType.isInstanceOf[OptionType]) Some(defaultV)
          else defaultV
        } else if(n.nodeType.isInstanceOf[OptionType] && !cond.nodeType.isInstanceOf[OptionType]) Some(condV)
        else condV
      case Library.In(what, where) =>
        val whatV = runScoped(what, scope)
        val whereV = runScoped(where, scope)
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
        val coll = runScoped(ch, scope).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        foldOptionIt(it, opt, num.zero, (a, b) => num.plus(a, b))
      case Library.Avg(ch) =>
        val coll = runScoped(ch, scope).asInstanceOf[Coll]
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
        val coll = runScoped(ch, scope).asInstanceOf[Coll]
        val (it, itType) = unwrapSingleColumn(coll, ch.nodeType)
        val (num, opt) = itType match {
          case t: ScalaOptionType[_] => (t.elementType.asInstanceOf[ScalaNumericType[Any]].numeric, true)
          case t => (t.asInstanceOf[ScalaNumericType[Any]].numeric, false)
        }
        foldOptionIt(it, opt, num.zero, (a, b) => if(num.lt(b, a)) b else a)
      case Apply(sym, ch) =>
        val chV = ch.map(n => (n.nodeType, runScoped(n, scope)))
        logDebug("[chV: "+chV.mkString(", ")+"]")
        // Use ternary logic for function calls
        if(n.nodeType.isInstanceOf[OptionType]) {
          if(chV.exists { case (t, v) => t.isInstanceOf[OptionType] && (v == None) }) None
          else {
            val chPlainV = chV.map {
              case (t: OptionType, v) => (t.elementType, v.asInstanceOf[Option[Any]].get)
              case other => other
            }
            logDebug("[chPlainV: "+chPlainV.mkString(", ")+"]")
            Some(evalFunction(sym, chPlainV, n.nodeType.asOptionType.elementType))
          }
        } else evalFunction(sym, chV, n.nodeType)
      //case Library.CountAll(ch) => runScoped(ch, scope).asInstanceOf[Coll].size
      case l: LiteralNode => l.value
    }
    indent -= 1
    if(logger.isDebugEnabled) logDebug("Result: "+res)
    res
  }

  def evalFunction(sym: Symbol, args: Seq[(Type, Any)], retType: Type) = sym match {
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
        case ProductType(_) =>
          coll.iterator.filter { p =>
            val v = p.asInstanceOf[ProductValue].apply(0)
            v != null && v != None
          }
        case _ =>
          coll.iterator.filter(v => v != null && v != None)
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

  def createNullRow(tpe: Type): Any = tpe match {
    case t: ScalaType[_] => if(t.nullable) None else null
    case StructType(el) =>
      new StructValue(el.map{ case (_, tpe) => createNullRow(tpe) }(collection.breakOut),
        el.zipWithIndex.map{ case ((sym, _), idx) => (sym, idx) }(collection.breakOut): Map[Symbol, Int])
    case ProductType(el) =>
      new ProductValue(el.map(tpe => createNullRow(tpe))(collection.breakOut))
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
  class StructValue(data: IndexedSeq[Any], symbolToIndex: (Symbol => Int)) extends ProductValue(data) {
    def getBySymbol(sym: Symbol): Any = apply(symbolToIndex(sym))
    override def toString = "StructValue("+data.mkString(", ")+"){"+symbolToIndex+"}"
  }
}
