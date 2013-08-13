package scala.slick.mongodb


import org.slf4j.LoggerFactory
import scala.collection.mutable.HashMap
import scala.slick.ast._
import scala.slick.SlickException
import scala.slick.util.{SlickLogger, Logging}
import TypeUtil.typeToTypeUtil
import java.util.regex.Pattern
import com.mongodb.casbah.Imports._

/**
  * A query interpreter for the MongoDB driver
  *
  * Compiles things down to Mongo DBObject
  * @param db The in-memory database which is used for resolving Tables
  * @param params The query parameters
  */

class QueryInterpreter(db: MongoBackend#Database, params: Any) extends Logging {
  override protected[this] lazy val logger = new SlickLogger(LoggerFactory.getLogger(classOf[QueryInterpreter]))
  import QueryInterpreter._

  val scope = new HashMap[Symbol, Any]
  var indent = 0
  type Coll = Iterable[Any]

  def logDebug(msg: String) {
    if (logger.isDebugEnabled) logger.debug(Iterator.fill(indent)(" ").mkString("", "", msg))
  }


  def run(n: Node): Any = {
    logDebug("Evaluating " + n)
    indent += 1
    val res = n match {
      /** A reference to a Symbol */
      case Ref(sym) =>
        scope.getOrElse(sym, throw new SlickException(s"Symbol $sym not found in scope"))
      /** An expression that selects a field in another expression. */
      case Select(in, field) =>
        val v = run(in)
        field match {
          case ElementSymbol(idx) => v.asInstanceOf[ProductValue].apply(idx-1)
          case (_: AnonSymbol | _: FieldSymbol) => v.asInstanceOf[StructValue].getBySymbol(field) // TODO - I believe this is where i patch in toDbObject...
        }
      /** An expression that represents a structure, i.e. a conjunction where the
        * individual components have Symbols associated with them. */
      case n: StructNode =>
        new StructValue(n.nodeChildren.map(run), n.nodeType.asInstanceOf[StructType].symbolToIndex)
      /** An expression that represents a conjunction of expressions. */
      case ProductNode(ch) =>
        new ProductValue(ch.map(run).toIndexedSeq)
      /** An expression that represents a plain value lifted into a Query. */
      case Pure(n) => Vector(run(n)) // pure is a plain value lifted into a query
      /** Base class for table nodes. Direct and lifted embedding have different
        * implementations of this class. */
      case t: TableNode =>
        val dbt = db.getTable(t.tableName)
        throw new SlickException("Can't currently handle 'run' on a TableNode as there is no interpretable structure to return from the schemaless MongoDB.")
      /** A .flatMap call of type
        * (CollectionType(c, _), CollectionType(_, u)) => CollectionType(c, u). */
      case Bind(gen, from, sel) =>
        // TODO - as a flatMap, I believe this needs to run as a mongodb query.
        ???
      /** A join expression of type
        * (CollectionType(c, t), CollectionType(_, u)) => CollecionType(c, (t, u)). */
      case j: Join =>
        throw new SlickException("MongoDB Driver cannot presently handle joins.")
      /** A .filter call of type
        * (CollectionType(c, t), Boolean) => CollectionType(c, t). */
      case Filter(gen, from, where) =>
        // TODO - this needs to run as a mongodb query.
        ???
      /** Get the first element of a collection. For client-side operations only.
        * In MongoDB , we should be able to run this on the server as a findOne */
      case First(ch) =>
        // TODO - translate to a MongoDB findOne
        ???
      /** A .sortBy call of type
        * (CollectionType(c, t), _) => CollectionType(c, t). */
      case SortBy(gen, from, by) =>
        // TODO - translate to a MongoDB Cursor sort.
        ???
      /** A .groupBy call. */
      case GroupBy(gen, _, from, by) =>
        throw new SlickException("MongoDB Driver cannot presently handle aggregation operations such as 'GROUP BY' / '.group()' ")
      /** A .take call. */
      case Take(from, num, _) =>
        // TODO - translate to an appropriate MongoDB Cursor operation
        ???
      /** A .drop call. */
      case Drop(from, num, _) =>
        // TODO - translate to an appropriate MongoDB Cursor operation
        ???
      /** A union of type
        * (CollectionType(c, t), CollectionType(_, t)) => CollectionType(c, t). */
      case Union(left, right, all, _, _) =>
        throw new SlickException("MongoDB Driver cannot presently handle unions.")
       /** no docs on what this element represents but one can guess from the name... */
      case GetOrElse(child /* : Node */, default) =>
        // TODO - this is probably going to actually get pulled out of a DBObject result. We need to retool how we approach it.
        run(child).asInstanceOf[Option[Any]].getOrElse(default())
      case OptionApply(child /* : Node */) =>
        Option(run(child))
      /** A conditional expression; all clauses should be IfThen nodes */
      case ConditionalExpr(clauses, elseClause) =>
        /** I'm fairly sure this would need to compile down to an aggregation framework / v8 statement... */
        throw new SlickException("MongoDB Driver cannot presently handle IF/THEN conditional clauses.")
      /** A parameter from a QueryTemplate which gets turned into a bind variable. */
      case QueryParameter(extractor, _) =>
        // TODO - do we use/support queryparamters in the mongo support?
        extractor(params)
      case Library.Exists(coll) =>
        //  TODO - Subquery EXISTS. Not sure we can support this...
        throw new SlickException("MongoDB Driver cannot handle subqueries and related operations such as 'EXISTS'")
      case Library.IfNull(cond, default) =>
        // TODO - Translate to a MongoDB Query result getOrElse...
        ???
      case Library.In(what, where) =>
        // TODO - translate to a MongoDB $in query...
        val whatV = run(what)
        val whereV = run(where)
        val whatOpt = what.nodeType.isInstanceOf[OptionType]
        if (whatOpt && (whatV.asInstanceOf[AnyRef].eq(null) || whatV == None)) None
        else {
          val whatBase = if (whatOpt) whatV.asInstanceOf[Option[Any]].get else whatV
          where.nodeType match {
            case ProductType(elTypes) =>
              val p = whereV.asInstanceOf[ProductValue]
              0.until(elTypes.length).iterator.map { i =>
                if (elTypes(i).isInstanceOf[OptionType]) {
                  p(i).asInstanceOf[Option[Any]] match {
                    case Some(v) => whatBase == v
                    case None => false
                  }
                } else whatBase == p(i)
              } contains true
            case ct: CollectionType =>
              // todo - decode this
              val (els, singleType) = unwrapSingleColumn(whereV, ct)
              (if (singleType.isInstanceOf[OptionType])
                els.map(_.asInstanceOf[Option[Any]] match {
                  case Some(v) => whatBase == v
                  case None => false
                })
              else els.map(whatBase.==)) contains true
          }
        }
      case Library.Sum(child) =>
        throw new SlickException("MongoDB Driver cannot presently handle aggregation operations such as 'SUM' / '.sum()' ")
      case Library.Avg(child) =>
        throw new SlickException("MongoDB Driver cannot presently handle aggregation operations such as 'AVG' / '.avg()' ")
      case Library.Min(child) =>
        // requires M/R or aggregation in mongodb. *sigh*
        throw new SlickException("MongoDB Driver cannot presently handle aggregation operations such as 'MIN' / '.min()' ")
      case Library.Max(child) =>
        // requires M/R or aggregation in mongodb. *sigh*
        throw new SlickException("MongoDB Driver cannot presently handle aggregation operations such as 'MAX' / '.max()' ")
      /** A function call expression.
        * this includes operators like <, > that we can translate to MongoDB queries  */
      case Apply(sym, child) =>
        val childV = child.map(n => (n.nodeType, run(n)))
        logDebug("[ childV: " + childV.mkString(", ") + "]")
        // use ternary logic for function calls
        if (n.nodeType.isInstanceOf[OptionType]) {
          if (childV.exists { case (t, v) => t.isInstanceOf[OptionType] && (v == None) }) None
          else {
            // TODO - we probably don't need this kind of unwrapping logic for remote (server) run ops
            val childPlainV = childV.map {
              case (t: OptionType, v) => (t.elementType, v.asInstanceOf[Option[Any]].get)
              case other => other
            }
            logDebug("[ childPlainV: " + childPlainV.mkString(", ") + " ]")
            Some(evalFunction(sym, childPlainV, n.nodeType.asOptionType.elementType))
          }
        } else evalFunction(sym, childV, n.nodeType)
      /** A literal value expression. */
      case l: LiteralNode =>
        l.value
    }
    indent -= 1
    logDebug("Result: " + res)
    res
  }

  /** evaluate certain function calls, returning key-value pairs for a MongoDB Query.
    * TODO : Need left side input to be the KEY not a fetched value? (see run loop section ) */
  def evalFunction(sym: Symbol, args: Seq[(Type, Any)], retType: Type) = {
    val lF: String = (args(0)._2).toString /* HACK ... TODO - Fix me. */
    val lT = args(0)._1
    val rV = args(1)._2
    val rT = args(1)._1
    sym match {
      case Library.== => MongoDBObject(lF -> rV)
      case Library.< => lF $lt rV
      case Library.<= => lF $lte rV
      case Library.> => lF $gt rV
      case Library.>= => lF $gte rV
      case Library.And =>
        // This is a SQL and so like WHERE foo = "bar" and x = "y"... Accrue a single MongoDBObject
        // TODO - Fill me in. Not sure how to extract field names from both sides of this. my brain hurts.
        logDebug("[ *AND* Args: " + args.mkString(", ") + " ]")
        ???
      case Library.Or =>
        // This is a SQL or so like WHERE foo = "bar" or x = "y"... Accrue a single MongoDBObject
        // TODO - Fill me in. Not sure how to extract field names from both sides of this. my brain hurts.
        logDebug("[ *OR* Args: " + args.mkString(", ") + " ]")
        ???
      case Library.Not =>
        // This is a SQL not so like WHERE foo = "bar" AND NOT x > 3.14 ... Accrue a single MongoDBObject
        // TODO - Fill me in. Not sure how to extract field names from both sides of this. my brain hurts.
        logDebug("[ *NOT* Args: " + args.mkString(", ") + " ]")
        ???
      case Library.+ | Library.- | Library.* | Library.% | Library.Abs | Library.Ceiling | Library.Degrees | Library.Floor |
           Library.Radians | Library.Sign | Library.Pi  =>
        // todo - we should be able to evaluate these client side.
        throw new SlickException("Mathematical Literal Operations (+, -, *, %, etc) are not supported in MongoDB Driver.")
      case Library.Cast =>
        // todo - though we don't support CAST in Mongo, we can emulate it client side *VERY* easily,
        // but need some basic infrastructure for the type conversions
        ???
      case Library.Concat | Library.LCase | Library.Length | Library.LTrim | Library.RTrim | Library.Trim | Library.UCase =>
        // TODO - We should be able to emulate these client side, but should we?
        ???
      case Library.CountAll /* Count(*) */ =>
        // TODO - we can emulate this by selecting the query and running a cursor count
        ???
      case Library.User | Library.Database =>
        throw new SlickException("JDBC Specific operations such as 'USER' and 'DATABASE' not supported by MongoDB driver.")
      case Library.Like =>
        throw new SlickException("SQL LIKE statements are unsupported by MongoDB. Please use a regular expression for similar behavior.")
        // TODO - RegEx support!
    }
  }

  def unwrapSingleColumn(value: Any, tpe: Type): (Iterator[Any], Type) =
    tpe.asCollectionType.elementType match {
      case ProductType(Seq(t)) =>
        logDebug(s"[ProductType] UNWRAP SINGLE COLUMN '$value' to '$tpe'")
        ???
      case t =>
        logDebug(s"[t] UNWRAP SINGLE COLUMN '$value' to '$tpe'")
        ???
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
  }

  /** The representation for StructType values in the interpreter */
  class StructValue(data: IndexedSeq[Any], symbolToIndex: (Symbol => Int)) extends ProductValue(data) {
    def getBySymbol(sym: Symbol): Any = apply(symbolToIndex(sym))
    override def toString = "StructValue("+data.mkString(", ")+"){"+symbolToIndex+"}"
  }

}
