package scala.slick.driver

import java.util.UUID
import scala.slick.ql._
import scala.slick.session.{PositionedResult, PositionedParameters}
import scala.slick.ast.Node
import scala.slick.util.ValueLinearizer

class PostgresDriver extends ExtendedDriver { driver =>

  override val typeMapperDelegates = new TypeMapperDelegates
  override def createQueryBuilder(node: Node, vl: ValueLinearizer[_]): QueryBuilder = new QueryBuilder(node, vl)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new DDLBuilder(table).buildDDL

  class QueryBuilder(ast: Node, linearizer: ValueLinearizer[_]) extends super.QueryBuilder(ast, linearizer) {
    override protected val concatOperator = Some("||")

    override protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = (take, drop) match {
      case (Some(t), Some(d)) => b += " LIMIT " += t += " OFFSET " += d
      case (Some(t), None) => b += " LIMIT " += t
      case (None, Some(d)) => b += " OFFSET " += d
      case _ =>
    }
  }

  class DDLBuilder(table: AbstractBasicTable[_]) extends super.DDLBuilder(table) {
    override protected def createColumnDDLBuilder(c: RawNamedColumn) = new ColumnDDLBuilder(c)

    protected class ColumnDDLBuilder(column: RawNamedColumn) extends super.ColumnDDLBuilder(column) {
      override def appendColumn(sb: StringBuilder) {
        sb append quoteIdentifier(column.name) append ' '
        if(autoIncrement) {
          sb append "SERIAL"
          autoIncrement = false
        }
        else sb append sqlType
        appendOptions(sb)
      }
    }
  }

  class TypeMapperDelegates extends super.TypeMapperDelegates {
    /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
    override val byteTypeMapperDelegate = new ByteTypeMapperDelegate {
      override def sqlTypeName = "SMALLINT"
    }
    override val byteArrayTypeMapperDelegate = new ByteArrayTypeMapperDelegate {
      override val sqlTypeName = "BYTEA"
    }
    override val uuidTypeMapperDelegate = new UUIDTypeMapperDelegate {
      override def setValue(v: UUID, p: PositionedParameters) = p.setObject(v, sqlType)
      override def setOption(v: Option[UUID], p: PositionedParameters) = p.setObjectOption(v, sqlType)
      override def nextValue(r: PositionedResult) = r.nextObject().asInstanceOf[UUID]
      override def updateValue(v: UUID, r: PositionedResult) = r.updateObject(v)
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
    }
  }
}

object PostgresDriver extends PostgresDriver
