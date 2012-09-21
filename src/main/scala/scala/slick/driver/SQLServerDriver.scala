package scala.slick.driver

import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.jdbc.PositionedResult
import scala.slick.util.MacroSupport.macroSupportInterpolation
import java.sql.{Timestamp, Date}
import scala.slick.profile.{SqlProfile, Capability}

/**
 * Slick driver for Microsoft SQL Server.
 *
 * This driver implements the [[scala.slick.driver.ExtendedProfile]]
 * ''without'' the following capabilities:
 *
 * <ul>
 *   <li>[[scala.slick.driver.JdbcProfile.capabilities.returnInsertOther]]:
 *     When returning columns from an INSERT operation, only a single column
 *     may be specified which must be the table's AutoInc column.</li>
 *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequence]]:
 *     Sequences are not supported because SQLServer does not have this
 *     feature.</li>
 * </ul>
 *
 * @author szeiger
 */
trait SQLServerDriver extends ExtendedDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcProfile.capabilities.returnInsertOther
    - SqlProfile.capabilities.sequence
  )

  override val typeMapperDelegates = new TypeMapperDelegates
  override def createQueryBuilder(input: QueryBuilderInput): QueryBuilder = new QueryBuilder(input)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: TypeMapperDelegate[_]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "BIT"
    case java.sql.Types.BLOB => "IMAGE"
    case java.sql.Types.CLOB => "TEXT"
    case java.sql.Types.DOUBLE => "FLOAT(53)"
    case java.sql.Types.FLOAT => "FLOAT(24)"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(input: QueryBuilderInput) extends super.QueryBuilder(input) with RowNumberPagination {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("+")
    override protected val useIntForBoolean = true

    override protected def buildSelectModifiers(c: Comprehension) {
      (c.fetch, c.offset) match {
        case (Some(t), Some(d)) => b"top ${d+t} "
        case (Some(t), None   ) => b"top $t "
        case (None,    _      ) => if(!c.orderBy.isEmpty) b"top 100 percent "
      }
    }

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc)
        b"case when ($n) is null then 1 else 0 end,"
      else if(o.nulls.first && o.direction.desc)
        b"case when ($n) is null then 0 else 1 end,"
      expr(n)
      if(o.direction.desc) b" desc"
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
