package slick.driver

import java.sql.Types
import scala.concurrent.ExecutionContext
import slick.SlickException
import slick.dbio._
import slick.lifted._
import slick.ast._
import slick.util.MacroSupport.macroSupportInterpolation
import slick.profile.{SqlProfile, Capability, RelationalProfile}
import slick.compiler.{Phase, CompilerState}
import slick.model.Model
import slick.jdbc.meta.MTable
import slick.jdbc.{JdbcModelBuilder, Invoker}

/** Slick driver for <a href="http://www.hsqldb.org/">HyperSQL</a>
  * (starting with version 2.0).
  *
  * This driver implements the [[slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.profile.SqlProfile.capabilities.sequenceCurr]]:
  *     <code>Sequence.curr</code> to get the current value of a sequence is
  *     not supported by Hsqldb. Trying to generate SQL code which uses this
  *     feature throws a SlickException.</li>
  *   <li>[[slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if generated
  *     keys should be returned. Otherwise the operation is performmed
  *     natively on the server side.</li>
  * </ul>
  */
trait HsqldbDriver extends JdbcDriver { driver =>

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - SqlProfile.capabilities.sequenceCurr
    - JdbcProfile.capabilities.insertOrUpdate
  )

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createTableNamer(mTable: MTable): TableNamer = new TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
      override def catalog = super.catalog.filter(_ != "PUBLIC") // remove default catalog
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override protected def computeQueryCompiler = super.computeQueryCompiler + Phase.specializeParameters
  override val columnTypes = new JdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder[_] = new SequenceDDLBuilder(seq)

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  override val scalarFrom = Some("(VALUES (0))")

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) with OracleStyleRowNum {
    override protected val concatOperator = Some("||")

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case l @ LiteralNode(v: String) if (v ne null) && jdbcTypeFor(l.tpe).sqlType != Types.CHAR =>
        /* Hsqldb treats string literals as type CHARACTER and pads them with
         * spaces in some expressions, so we cast all string literals to
         * VARCHAR. The length is only 16M instead of 2^31-1 in order to leave
         * enough room for concatenating strings (which extends the size even if
         * it is not needed). */
        b"cast("
        super.expr(c)
        b" as varchar(16777216))"
      /* Hsqldb uses the SQL:2008 syntax for NEXTVAL */
      case Library.NextValue(SequenceNode(name)) => b"(next value for `$name)"
      case Library.CurrentValue(_*) => throw new SlickException("Hsqldb does not support CURRVAL")
      case RowNumber(_) => b"rownum()" // Hsqldb uses Oracle ROWNUM semantics but needs parens
      case _ => super.expr(c, skipParens)
    }

    override protected def buildJoin(j: Join): Unit = {
      /* Re-balance inner joins to the left because Hsqldb does not supported RHS nesting. Paths
       * into joined views have already been mapped to unique identifiers at this point, so we can
       * safely rearrange views. */
      j match {
        case Join(ls, rs, l, Join(ls2, rs2, l2, r2, JoinType.Inner, on2), JoinType.Inner, on) =>
          val on3 = (on, on2) match {
            case (a, LiteralNode(true)) => a
            case (LiteralNode(true), b) => b
            case (a, b) => Apply(Library.And, Vector(a, b))(UnassignedType)
          }
          buildJoin(Join(rs, rs2, Join(ls, ls2, l, l2, JoinType.Inner, LiteralNode(true)), r2, JoinType.Inner, on3))
        case j => super.buildJoin(j)
      }
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $t offset $d"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None, Some(d)   ) => b"\noffset $d"
      case _ =>
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType {
      override def sqlTypeName(size: Option[RelationalProfile.ColumnOption.Length]) = "LONGVARBINARY"
    }
    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlType = java.sql.Types.BINARY
      override def sqlTypeName(size: Option[RelationalProfile.ColumnOption.Length]) = "BINARY(16)"
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
      DDL(b.toString, "DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }
}

object HsqldbDriver extends HsqldbDriver
