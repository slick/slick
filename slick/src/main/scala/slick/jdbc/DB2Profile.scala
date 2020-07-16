package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import java.time.Instant
import java.util.UUID

import scala.concurrent.ExecutionContext

import slick.ast.*
import slick.basic.Capability
import slick.compiler.{CompilerState, Phase, RewriteBooleans}
import slick.dbio.*
import slick.jdbc.meta.MTable
import slick.lifted.*
import slick.relational.RelationalCapabilities
import slick.util.QueryInterpolator.queryInterpolator

/** Slick profile for IBM DB2 UDB.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.reverse]]:
  *     This String function is not available in DB2.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if generated
  *     keys should be returned. Otherwise the operation is performmed
  *     natively on the server side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
  *     DB2 doesn't have booleans, so Slick maps to SMALLINT instead.
  *     Other JDBC drivers like MySQL map TINYINT(1) back to a Scala
  *     Boolean. DB2 maps SMALLINT to an Integer and that's how it shows
  *     up in the JDBC meta data, thus the original type is lost.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     DB2 does not have a BYTE type.</li>
  * </ul>
  *
  * Note: The DB2 JDBC driver has problems with quoted identifiers. Columns
  * which are returned from inserts must not require quoted names (in
  * particular, they must not contain lower-case characters or be equal to a
  * reserved word), otherwise a bug in the DB2 JDBC driver triggers a SQL
  * Error -206 (SQLState 42703).
  */
trait DB2Profile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport {

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.reverse
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.supportsByte
    - JdbcCapabilities.booleanMetaData
  )

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false
  override protected val invokerMutateType: ResultSetType = ResultSetType.ScrollSensitive

  override protected def computeQueryCompiler =
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      + Phase.rewriteBooleans)
  override val columnTypes: DB2JdbcTypes = new DB2JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new DB2QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new DB2TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder =
    new DB2ColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder = new DB2SequenceDDLBuilder(seq)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE"))).map(_.filter(_.name.schema.filter(_ == "SYSTOOLS").isEmpty))

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.TINYINT => "SMALLINT" // DB2 has no smaller binary integer type
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  override val scalarFrom: Some[String] = Some("sysibm.sysdummy1")

  class DB2QueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val hasPiFunction = false
    override protected val hasRadDegConversion = false
    override protected val pi = "decfloat(3.1415926535897932384626433832)"

    override def expr(c: Node): Unit = c match {
      case RowNumber(by) =>
        b += "row_number() over("
        if(!by.isEmpty) buildOrderByClause(by)
        b += ")"
      case Library.IfNull(l, r) =>
        /* DB2 does not support IFNULL so we use COALESCE instead */
        b += "coalesce("; expr(l, true); b += ","; expr(r, true); b += ")"
      case Library.NextValue(SequenceNode(name)) => b += "(next value for " += quoteIdentifier(name) += ")"
      case Library.CurrentValue(SequenceNode(name)) => b += "(prevval for " += quoteIdentifier(name) += ")"
      case Library.User() => b += "current user"
      case Library.Database() => b += "current server"
      case Library.CountAll(LiteralNode(1)) => b"count(*)"
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.SilentCast, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.IfNull, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case c@Comprehension(_, _, _, Some(n @ Apply(Library.IfNull, _)), _, _, _, _, _, _, _) =>
        super.expr(c.copy(where = Some(RewriteBooleans.rewriteFakeBooleanEqOne(n))))
      case _ => super.expr(c)
    }

    override protected def buildOrdering(n: Node, o: Ordering): Unit = {
      /* DB2 does not have explicit NULLS FIST/LAST clauses. Nulls are
       * sorted after non-null values by default. */
      if(o.nulls.first && !o.direction.desc) {
        b += "case when ("
        expr(n, false)
        b += ") is null then 0 else 1 end,"
      } else if(o.nulls.last && o.direction.desc) {
        b += "case when ("
        expr(n, false)
        b += ") is null then 1 else 0 end,"
      }
      expr(n, false)
      if(o.direction.desc) b += " desc"
    }

    override protected def buildForUpdateClause(forUpdate: Boolean) = {
      super.buildForUpdateClause(forUpdate)
      if(forUpdate) {
        b" with RS "
      }
    }
  }

  class DB2TableDDLBuilder(table: Table[_]) extends TableDDLBuilder(table) {
    override protected def createIndex(idx: Index) = {
      if(idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because DB2 does not allow a FOREIGN KEY CONSTRAINT to
         * reference columns which have a UNIQUE INDEX but not a nominal UNIQUE
         * CONSTRAINT. */
        val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
        sb append "CONSTRAINT " append quoteIdentifier(idx.name) append " UNIQUE("
        addIndexColumnList(idx.on, sb, idx.table.tableName)
        sb append ")"
        sb.toString
      } else super.createIndex(idx)
    }

    //For compatibility with all versions of DB2
    //http://stackoverflow.com/questions/3006999/sql-query-to-truncate-table-in-ibm-db2
    override def truncateTable = s"DELETE FROM ${quoteTableName(tableNode)}"

    override def createIfNotExistsPhase = {
      //
      Iterable(
        "begin\n"
      + "declare continue handler for sqlstate '42710' begin end; \n"
      + ((createPhase1 ++ createPhase2).map{s =>
        "execute immediate '"+ s.replaceAll("'", """\\'""") + " ';"
      }.mkString("\n"))
      + "\nend")
    }

    override def dropIfExistsPhase = {
      //
      Iterable(
        "begin\n"
      + "declare continue handler for sqlstate '42704' begin end; \n"
      + ((dropPhase1 ++ dropPhase2).map{s =>
        "execute immediate '"+ s.replaceAll("'", """\\'""") + " ';"
      }.mkString("\n"))
      + "\nend")
    }
  }

  class DB2ColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    override def appendColumn(sb: StringBuilder): Unit = {
      val qname = quoteIdentifier(column.name)
      sb append qname append ' '
      appendType(sb)
      appendOptions(sb)
      if(jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
        sb append " constraint "+quoteIdentifier(column.name+"__bool")+" check (" append qname append " in (0, 1))"
      }
    }
  }

  class DB2SequenceDDLBuilder(seq: Sequence[_]) extends SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      val b = new StringBuilder append "create sequence " append quoteIdentifier(seq.name)
      b append " as " append jdbcTypeFor(seq.tpe).sqlTypeName(None)
      seq._start.foreach { b append " start with " append _ }
      seq._increment.foreach { b append " increment by " append _ }
      seq._minValue.foreach { b append " minvalue " append _ }
      seq._maxValue.foreach { b append " maxvalue " append _ }
      if(seq._cycle) b append " cycle"
      DDL(b.toString, "drop sequence " + quoteIdentifier(seq.name))
    }
  }

  class DB2JdbcTypes extends JdbcTypes {
    override val booleanJdbcType: DB2BooleanJdbcType = new DB2BooleanJdbcType
    override val uuidJdbcType: DB2UUIDJdbcType = new DB2UUIDJdbcType
    override val instantType: DB2InstantJdbcType = new DB2InstantJdbcType

    class DB2UUIDJdbcType extends UUIDJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(16) FOR BIT DATA"
      override def hasLiteralForm = true
      override def valueToSQLLiteral(value: UUID): String =
        "x'" + value.toString.replace("-", "") + "'"
      override def sqlType = java.sql.Types.VARBINARY
    }

    /* DB2 does not have a proper BOOLEAN type. The suggested workaround is
     * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
    class DB2BooleanJdbcType extends BooleanJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }

    class DB2InstantJdbcType extends InstantJdbcType {
      // Can't use Timestamp as the type here as subject to 2 hours DST loss each year
      override def sqlType : Int = {
        java.sql.Types.VARCHAR
      }
      override def setValue(v: Instant, p: PreparedStatement, idx: Int) : Unit = {
        p.setString(idx, v.toString)
      }
      override def getValue(r: ResultSet, idx: Int) : Instant = {
        r.getString(idx) match {
          case null => null
          case instantStr => Instant.parse(instantStr)
        }
      }
      override def updateValue(v: Instant, r: ResultSet, idx: Int) : Unit = {
        r.updateString(idx, v.toString)
      }
      override def valueToSQLLiteral(value: Instant) = {
        s"'${value.toString}'"
      }
    }


  }
}

object DB2Profile extends DB2Profile
