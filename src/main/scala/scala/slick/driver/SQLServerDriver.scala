package scala.slick.driver

import scala.slick.ql._
import scala.slick.ast._
import scala.slick.SLICKException
import java.sql.{Timestamp, Time, Date}
import scala.slick.session.{PositionedParameters, PositionedResult, ResultSetType}
import scala.slick.util.ValueLinearizer

/**
 * SLICK driver for Microsoft SQL Server.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 *
 * <ul>
 *   <li>Sequences are not supported because SQLServer does not have this
 *     feature.</li>
 *   <li>There is limited support for take() and drop() modifiers on
 *     subqueries. Due to the way these modifiers have to be encoded for SQL
 *     Server, they only work on top-level queries or sub-queries of simple
 *     COUNT(*) top-level queries.</li>
 * </ul>
 *
 * @author szeiger
 */
trait SQLServerDriver extends ExtendedDriver { driver =>

  override val typeMapperDelegates = new TypeMapperDelegates
  override def buildTableDDL(table: Table[_]): DDL = new DDLBuilder(table).buildDDL
  override def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)

  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "BIT"
    case java.sql.Types.BLOB => "IMAGE"
    case java.sql.Types.CLOB => "TEXT"
    case _ => super.mapTypeName(tmd)
  }

  protected val dummyOrdering = Seq[(Node, Ordering)]((Comprehension(select = Some(ConstColumn.NULL)), Ordering()))

  class QueryBuilder(ast: Node, linearizer: ValueLinearizer[_]) extends super.QueryBuilder(ast, linearizer) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("+")
    override protected val useIntForBoolean = true

    /*TODO
    val hasTakeDrop = !query.typedModifiers[TakeDrop].isEmpty
    val hasDropOnly = query.typedModifiers[TakeDrop] match {
      case TakeDrop(None, Some(_)) :: _ => true
      case _ => false
    }
    val isCountAll = query.reified match {
      case ColumnOps.CountAll(_) => true
      case _ => false
    }
    */
    val hasTakeDrop = false //--
    val hasDropOnly = false //--
    val isCountAll = false //--

    /*TODO
    override def buildSelect(b: SQLBuilder): Unit = {
      /* Rename at top level if we need to wrap with TakeDrop code */
      innerBuildSelect(b, hasTakeDrop)
      insertAllFromClauses()
    }
    */

    /*TODO
    override protected def innerBuildSelectNoRewrite(rename: Boolean) {
      query.typedModifiers[TakeDrop] match {
        case TakeDrop(Some(t), Some(d)) :: _ =>
          b += "WITH T AS (SELECT TOP " += (t+d) += ' '
          expr(query.reified)
          //TODO fromSlot = b.createSlot
          appendClauses()
          b += ") SELECT "
          addCopyColumns()
          b += " FROM T WHERE \"c0r\" BETWEEN " += (d+1) += " AND " += (t+d)
          if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
        case TakeDrop(Some(t), None) :: _ =>
          b += "WITH T AS (SELECT TOP " += t += ' '
          expr(query.reified)
          //TODO fromSlot = b.createSlot
          appendClauses()
          b += ") SELECT "
          addCopyColumns()
          b += " FROM T WHERE \"c0r\" BETWEEN 1 AND " += t
          if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
        case TakeDrop(None, Some(d)) :: _ =>
          b += "WITH T AS (SELECT "
          expr(query.reified)
          //TODO fromSlot = b.createSlot
          appendClauses()
          b += ") SELECT "
          addCopyColumns()
          b += " FROM T WHERE \"c0r\" > " += d
          if(!isCountAll) b += " ORDER BY \"c0r\" ASC"
        case _ =>
          super.innerBuildSelectNoRewrite(rename)
      }
    }
    */

    def addCopyColumns() {
      //TODO
      /*
      if(isCountAll) b += "count(*)"
      else if(maxColumnPos == 0) b += "*"
      else b.sep(1 to maxColumnPos, ",")(i => b += "\"c" += i += "\"")
      */
    }

    /*TODO
    override protected def expr(c: Node, rename: Boolean, topLevel: Boolean): Unit = {
      c match {
        /* Convert proper BOOLEANs which should be returned from a SELECT
         * statement into pseudo-boolean BIT values 1 and 0 */
        case c: Column[_] if topLevel && !rename && b == selectSlot && c.typeMapper(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
          b += "case when "
          innerExpr(c)
          b += " then 1 else 0 end"
        case _ => super.expr(c, rename, topLevel)
      }
      if(topLevel && hasTakeDrop) {
        b += ",ROW_NUMBER() OVER ("
        appendOrderClause()
        if(query.typedModifiers[Ordering].isEmpty) b += "ORDER BY (SELECT NULL)"
        b += ") AS \"c0r\""
      }
    }
    */

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {

      //TODO case ColumnOps.CountAll(q) if(hasTakeDrop) => b += "*"; localTableName(q)
      case _ => super.expr(c, skipParens)
    }

    override protected def appendClauses(): Unit = {
      appendConditions()
      /*TODO
      appendGroupClause()
      appendHavingConditions()
      if(!hasDropOnly) appendOrderClause()
      */
    }

    /*
    override protected def buildProperTakeDrop(from: Node, take: Option[Int], drop: Option[Int]) = {
      val (newFrom, appendDummyOrdering) = from match {
        case c @ Comprehension(_, _, o, _) =>
          (if(o.isEmpty) c.copy(orderBy = dummyOrdering) else c, false)
        case n => (n, true)
      }
      buildComprehension(newFrom, true)
      if(appendDummyOrdering) b += " order by (select null)"
      buildFetchOffsetClause(take, drop)
    }

    override protected def buildFetchOffsetClause(take: Option[Long], drop: Option[Long]) = building(OtherPart) {
      if(take.isDefined || drop.isDefined) {
        b += " offset " += drop.getOrElse(0) += " row"
        take.foreach{ t => b += " fetch next " += t += " row only" }
      }
    }*/

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc) {
        b += "case when ("
        expr(n)
        b += ") is null then 1 else 0 end,"
      } else if(o.nulls.first && o.direction.desc) {
        b += "case when ("
        expr(n)
        b += ") is null then 0 else 1 end,"
      }
      expr(n)
      if(o.direction.desc) b += " desc"
    }

    /* Move COUNT(*) into subqueries even if they have TakeDrop modifiers.
     * It will be treated specially there to make it work. */
    override protected def rewriteCountStarQuery(q: Query[_, _]) = true
  }

  class DDLBuilder(table: Table[_]) extends super.DDLBuilder(table) {
    override protected def createColumnDDLBuilder(c: RawNamedColumn) = new ColumnDDLBuilder(c)

    protected class ColumnDDLBuilder(column: RawNamedColumn) extends super.ColumnDDLBuilder(column) {
      override protected def appendOptions(sb: StringBuilder) {
        if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
        if(notNull) sb append " NOT NULL"
        if(primaryKey) sb append " PRIMARY KEY"
        if(autoIncrement) sb append " IDENTITY"
      }
    }
  }

  class TypeMapperDelegates extends super.TypeMapperDelegates {
    override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
    override val byteTypeMapperDelegate = new ByteTypeMapperDelegate
    override val dateTypeMapperDelegate = new DateTypeMapperDelegate
    override val timestampTypeMapperDelegate = new TimestampTypeMapperDelegate
    override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate {
      override def sqlTypeName = "UNIQUEIDENTIFIER"
    }
    /* SQL Server does not have a proper BOOLEAN type. The suggested workaround is
     * BIT with constants 1 and 0 for TRUE and FALSE. */
    class BooleanTypeMapperDelegate extends super.BooleanTypeMapperDelegate {
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }
    /* Selecting a straight Date or Timestamp literal fails with a NPE (probably
     * because the type information gets lost along the way), so we cast all Date
     * and Timestamp values to the proper type. This work-around does not seem to
     * be required for Time values. */
    class DateTypeMapperDelegate extends super.DateTypeMapperDelegate {
      override def valueToSQLLiteral(value: Date) = "{fn convert({d '" + value + "'}, DATE)}"
    }
    class TimestampTypeMapperDelegate extends super.TimestampTypeMapperDelegate {
      /* TIMESTAMP in SQL Server is a data type for sequence numbers. What we
       * want here is DATETIME. */
      override def sqlTypeName = "DATETIME"
      override def valueToSQLLiteral(value: Timestamp) = "{fn convert({ts '" + value + "'}, DATETIME)}"
    }
    /* SQL Server's TINYINT is unsigned, so we use SMALLINT instead to store a signed byte value.
     * The JDBC driver also does not treat signed values correctly when reading bytes from result
     * sets, so we read as Short and then convert to Byte. */
    class ByteTypeMapperDelegate extends super.ByteTypeMapperDelegate {
      override def sqlTypeName = "SMALLINT"
      //def setValue(v: Byte, p: PositionedParameters) = p.setByte(v)
      //def setOption(v: Option[Byte], p: PositionedParameters) = p.setByteOption(v)
      override def nextValue(r: PositionedResult) = r.nextShort.toByte
      //def updateValue(v: Byte, r: PositionedResult) = r.updateByte(v)
    }
  }
}

object SQLServerDriver extends SQLServerDriver
