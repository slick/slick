package scala.slick.driver

import java.sql.Types
import scala.slick.SlickException
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.ast.Util._
import scala.slick.ast.ExtraUtil._

/**
 * SLICK driver for <a href="http://www.hsqldb.org/">HyperSQL</a>
 * (starting with version 2.0).
 * 
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 * <ul>
 *   <li><code>Sequence.curr</code> to get the current value of a sequence is
 *     not supported by Hsqldb. Trying to generate SQL code which uses this
 *     feature throws a SlickException.</li>
 * </ul>
 * 
 * @author szeiger
 */
trait HsqldbDriver extends ExtendedDriver { driver =>

  override val typeMapperDelegates = new TypeMapperDelegates
  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder[_] = new SequenceDDLBuilder(seq)

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {
    override protected val scalarFrom = Some("(VALUES (0))")
    override protected val concatOperator = Some("||")

    override protected def toComprehension(n: Node, liftExpression: Boolean = false) =
      super.toComprehension(n, liftExpression) match {
        case c @ Comprehension(from, _, None, orderBy, Some(sel), _, _) if !orderBy.isEmpty && hasRowNumber(sel) =>
          // Hsqldb supports only Oracle-style ROWNUM (applied before ORDER BY),
          // so we pull the SELECT clause with the ROWNUM up into a new query
          val paths = findPaths(from.map(_._1).toSet, sel).map(p => (p, new AnonSymbol)).toMap
          //println(paths.map { case (p: Select, _) => Path.toString(p) })
          val inner = c.copy(select = Some(Pure(StructNode(paths.toIndexedSeq.map { case (n,s) => (s,n) }))))
          val gen = new AnonSymbol
          val newSel = sel.replace {
            case s: Select => paths.get(s).fold(s) { sym => Select(Ref(gen), sym) }
          }
          Comprehension(Seq((gen, inner)), select = Some(newSel))
        case c => c
      }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {

      case c @ ConstColumn(v: String) if v ne null =>
        /* Hsqldb treats string literals as type CHARACTER and pads them with
         * spaces in some expressions, so we cast all string literals to
         * VARCHAR. The length is only 16M instead of 2^31-1 in order to leave
         * enough room for concatenating strings (which extends the size even if
         * it is not needed). */
        if(c.typeMapper(driver).sqlType == Types.CHAR) super.expr(c, skipParens)
        else {
          b += "cast("
          super.expr(c)
          b += " as varchar(16777216))"
        }
      case RowNumber(_) => b += "rownum()"

      /* Hsqldb uses the SQL:2008 syntax for NEXTVAL */
      case Library.NextValue(SequenceNode(name)) => b += "(next value for " += quoteIdentifier(name) += ")"

      case Library.CurrentValue(_*) => throw new SlickException("Hsqldb does not support CURRVAL")

      case _ => super.expr(c, skipParens)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
      case (Some(t), None) => b += " LIMIT " += t
      case (None, Some(d)) => b += " OFFSET " += d
      case _ =>
    }
  }

  class TypeMapperDelegates extends super.TypeMapperDelegates {
    override val byteArrayTypeMapperDelegate = new ByteArrayTypeMapperDelegate {
      override val sqlTypeName = "LONGVARBINARY"
    }
    override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate {
      override def sqlType = java.sql.Types.BINARY
      override def sqlTypeName = "BINARY(16)"
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def createIndex(idx: Index) = {
      if(idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because Hsqldb does not allow a FOREIGN KEY CONSTRAINT to
         * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
         * CONSTRAINT. */
        val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
        sb append "CONSTRAINT " append quoteIdentifier(idx.name) append " UNIQUE("
        addIndexColumnList(idx.on, sb, idx.table.tableName)
        sb append ")"
        sb.toString
      } else super.createIndex(idx)
    }
  }

  class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      import seq.integral._
      val increment = seq._increment.getOrElse(one)
      val desc = increment < zero
      val start = seq._start.getOrElse(if(desc) -1 else 1)
      val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
      seq._increment.foreach { b append " INCREMENT BY " append _ }
      seq._minValue.foreach { b append " MINVALUE " append _ }
      seq._maxValue.foreach { b append " MAXVALUE " append _ }
      /* The START value in Hsqldb defaults to 0 instead of the more
       * conventional 1/-1 so we rewrite it to make 1/-1 the default. */
      if(start != 0) b append " START WITH " append start
      if(seq._cycle) b append " CYCLE"
      new DDL {
        val createPhase1 = Iterable(b.toString)
        val createPhase2 = Nil
        val dropPhase1 = Nil
        val dropPhase2 = Iterable("DROP SEQUENCE " + quoteIdentifier(seq.name))
      }
    }
  }
}

object HsqldbDriver extends HsqldbDriver
