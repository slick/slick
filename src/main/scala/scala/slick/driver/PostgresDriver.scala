package scala.slick.driver

import java.util.UUID
import scala.slick.lifted._
import scala.slick.jdbc.{PositionedParameters, PositionedResult}
import scala.slick.ast.{SequenceNode, Library, FieldSymbol, Node}
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.compiler.CompilerState
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.Invoker

/**
 * Slick driver for PostgreSQL.
 *
 * This driver implements all capabilities of the
 * [[scala.slick.driver.ExtendedProfile]].
 *
 * Notes:
 *
 * <ul>
 *   <li>[[scala.slick.profile.RelationalProfile.capabilities.typeBlob]]:
 *   The default implementation of the <code>Blob</code> type uses the
 *   database type <code>lo</code> and the stored procedure
 *   <code>lo_manage</code>, both of which are provided by the "lo"
 *   extension in PostgreSQL.</li>
 * </ul>
 *
 * @author szeiger
 */
trait PostgresDriver extends JdbcDriver { driver =>

  override def getTables: Invoker[MTable] = MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)

  override def defaultSqlTypeName(tmd: JdbcType[_]): String = tmd.sqlType match {
    case java.sql.Types.BLOB => "lo"
    case java.sql.Types.DOUBLE => "DOUBLE PRECISION"
    /* PostgreSQL does not have a TINYINT type, so we use SMALLINT instead. */
    case java.sql.Types.TINYINT => "SMALLINT"
    case _ => super.defaultSqlTypeName(tmd)
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None,    Some(d)) => b" offset $d"
      case _ =>
    }

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name)) => b"nextval('$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval('$name')"
      case _ => super.expr(n, skipParens)
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override def createPhase1 = super.createPhase1 ++ columns.flatMap {
      case cb: ColumnDDLBuilder => cb.createLobTrigger(table.tableName)
    }
    override def dropPhase1 = {
      val dropLobs = columns.flatMap {
        case cb: ColumnDDLBuilder => cb.dropLobTrigger(table.tableName)
      }
      if(dropLobs.isEmpty) super.dropPhase1
      else Seq("delete from "+quoteIdentifier(table.tableName)) ++ dropLobs ++ super.dropPhase1
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
      if(autoIncrement && !customSqlType) sb append "SERIAL"
      else sb append sqlType
      autoIncrement = false
      appendOptions(sb)
    }

    def lobTrigger(tname: String) =
      quoteIdentifier(tname+"__"+quoteIdentifier(column.name)+"_lob")

    def createLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "create trigger "+lobTrigger(tname)+" before update or delete on "+
        quoteIdentifier(tname)+" for each row execute procedure lo_manage("+quoteIdentifier(column.name)+")"
      ) else None

    def dropLobTrigger(tname: String): Option[String] =
      if(sqlType == "lo") Some(
        "drop trigger "+lobTrigger(tname)+" on "+quoteIdentifier(tname)
      ) else None
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val uuidJdbcType = new UUIDJdbcType

    class ByteArrayJdbcType extends super.ByteArrayJdbcType {
      override val sqlType = java.sql.Types.BINARY
      override val sqlTypeName = "BYTEA"
      override def setOption(v: Option[Array[Byte]], p: PositionedParameters) = v match {
        case Some(a) => p.setBytes(a)
        case None => p.setNull(sqlType)
      }
    }

    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlTypeName = "UUID"
      override def setValue(v: UUID, p: PositionedParameters) = p.setObject(v, sqlType)
      override def setOption(v: Option[UUID], p: PositionedParameters) = p.setObjectOption(v, sqlType)
      override def nextValue(r: PositionedResult) = r.nextObject().asInstanceOf[UUID]
      override def updateValue(v: UUID, r: PositionedResult) = r.updateObject(v)
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
      override def hasLiteralForm = true
    }
  }
}

object PostgresDriver extends PostgresDriver
