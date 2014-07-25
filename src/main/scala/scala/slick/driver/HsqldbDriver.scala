package scala.slick.driver

import java.sql.Types
import scala.slick.SlickException
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.profile.{SqlProfile, Capability}
import scala.slick.compiler.{Phase, CompilerState}
import scala.slick.model.Model
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.Invoker

/** Slick driver for <a href="http://www.hsqldb.org/">HyperSQL</a>
  * (starting with version 2.0).
  *
  * This driver implements the [[scala.slick.driver.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[scala.slick.profile.SqlProfile.capabilities.sequenceCurr]]:
  *     <code>Sequence.curr</code> to get the current value of a sequence is
  *     not supported by Hsqldb. Trying to generate SQL code which uses this
  *     feature throws a SlickException.</li>
  *   <li>[[scala.slick.driver.JdbcProfile.capabilities.insertOrUpdate]]:
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

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean = true)(implicit session: Backend#Session) extends super.ModelBuilder(mTables, ignoreInvalidDefaults){
    override def Table = new Table(_){
      override def schema = super.schema.filter(_ != "PUBLIC") // remove default schema
      override def catalog = super.catalog.filter(_ != "PUBLIC") // remove default catalog
    }
  }

  override def createModel(tables: Option[Seq[MTable]] = None, ignoreInvalidDefaults: Boolean = true)
                          (implicit session: Backend#Session)
                          : Model
    = new ModelBuilder(tables.getOrElse(defaultTables), ignoreInvalidDefaults).model

  override def defaultTables(implicit session: Backend#Session)
    = MTable.getTables(None, None, None, Some(Seq("TABLE"))).list

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
    override protected val supportsEmptyJoinConditions = false

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

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None, Some(d)   ) => b" offset $d"
      case _ =>
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val byteArrayJdbcType = new ByteArrayJdbcType {
      override val sqlTypeName = "LONGVARBINARY"
    }
    override val uuidJdbcType = new UUIDJdbcType {
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
      DDL(b.toString, "DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }
}

object HsqldbDriver extends HsqldbDriver
