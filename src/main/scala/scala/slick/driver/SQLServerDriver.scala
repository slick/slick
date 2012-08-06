package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.SlickException
import java.sql.{Timestamp, Date}
import scala.slick.session.PositionedResult

/**
 * SLICK driver for Microsoft SQL Server.
 *
 * <p>This driver implements the ExtendedProfile with the following
 * limitations:</p>
 *
 * <ul>
 *   <li>Sequences are not supported because SQLServer does not have this
 *     feature.</li>
 * </ul>
 *
 * @author szeiger
 */
trait SQLServerDriver extends ExtendedDriver { driver =>

  override val typeMapperDelegates = new TypeMapperDelegates
  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def mapTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "BIT"
    case java.sql.Types.BLOB => "IMAGE"
    case java.sql.Types.CLOB => "TEXT"
    case java.sql.Types.DOUBLE => "FLOAT(53)"
    case java.sql.Types.FLOAT => "FLOAT(24)"
    case _ => super.mapTypeName(tmd)
  }

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("+")
    override protected val useIntForBoolean = true

    case object StarAndRowNum extends NullaryNode { override def toString = "StarAndRowNum" }
    case object RowNum extends NullaryNode { override def toString = "RowNum" }

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case StarAndRowNum => b += "*, row_number() over(order by (select 1))"
      case RowNum => b += "row_number() over(order by (select 1))"
      case _ => super.expr(c, skipParens)
    }

    override protected def buildSelectModifiers(c: Comprehension) {
      if(!c.orderBy.isEmpty) b += "top 100 percent "
    }

    override protected def buildComprehension(c: Comprehension) {
      scanJoins(c.from)
      if(c.fetch.isDefined || c.offset.isDefined) {
        val r = newSym
        val rn = symbolName(r)
        val tn = symbolName(newSym)
        val c2 = makeSelectPageable(c, r)
        b += "select top "
        (c.fetch, c.offset) match {
          case (Some(t), Some(d)) => b += (d+t)
          case (Some(t), None   ) => b += t
          case (None,    _      ) => b += "100 percent"
        }
        b += " "
        c2.select match {
          case Some(Pure(StructNode(ch))) =>
            b.sep(ch.filter { case (_, x) => x != RowNum }, ", ") {
              case (sym, StarAndRowNum) => b += "*"
              case (sym, _) => b += symbolName(sym)
            }
          case o => throw new SlickException("Unexpected node "+o+" in SELECT slot of "+c)
        }
        b += " from ("
        super.buildComprehension(c2)
        b += ") " += tn += " where " += rn
        (c.fetch, c.offset) match {
          case (Some(t), Some(d)) => b += " between " += (d+1L) += " and " += (t+d)
          case (Some(t), None   ) => b += " between 1 and " += t
          case (None,    Some(d)) => b += " > " += d
          case _ => throw new SlickException("Unexpected empty fetch/offset")
        }
        b += " order by " += rn
      }
      else super.buildComprehension(c)
    }

    /** Create aliases for all selected rows (unless it is a "select *" query),
      * add a RowNum column, and remove FETCH and OFFSET clauses. The SELECT
      * clause of the resulting Comprehension always has the shape
      * Some(Pure(StructNode(_))). */
    protected def makeSelectPageable(c: Comprehension, rn: AnonSymbol): Comprehension = c.select match {
      case Some(Pure(StructNode(ch))) =>
        c.copy(select = Some(Pure(StructNode(ch :+ (rn -> RowNum)))), fetch = None, offset = None)
      case Some(Pure(ProductNode(ch))) =>
        c.copy(select = Some(Pure(StructNode(ch.toIndexedSeq.map(n => newSym -> n) :+ (rn -> RowNum)))), fetch = None, offset = None)
      case Some(Pure(n)) =>
        c.copy(select = Some(Pure(StructNode(IndexedSeq(newSym -> n, rn -> RowNum)))), fetch = None, offset = None)
      case None =>
        // should not happen at the outermost layer, so copying an extra row does not matter
        c.copy(select = Some(Pure(StructNode(IndexedSeq(rn -> StarAndRowNum)))), fetch = None, offset = None)
    }

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
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " IDENTITY"
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
