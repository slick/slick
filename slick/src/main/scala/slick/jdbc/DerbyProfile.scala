package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import java.time.Instant
import java.util.UUID

import scala.concurrent.ExecutionContext

import slick.SlickException
import slick.ast.*
import slick.ast.TypeUtil.*
import slick.basic.Capability
import slick.compiler.{CompilerState, Phase, RewriteBooleans}
import slick.dbio.*
import slick.jdbc.meta.MTable
import slick.relational.RelationalCapabilities
import slick.sql.SqlCapabilities
import slick.util.QueryInterpolator.queryInterpolator

/** Slick profile for Derby/JavaDB.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.functionDatabase]]:
  *     <code>Functions.database</code> is not available in Derby. Slick
  *     will return an empty string instead.</li>
  *   <li>[[slick.relational.RelationalCapabilities.replace]],
  *     [[slick.relational.RelationalCapabilities.reverse]]:
  *     These String functions are not available in Derby.</li>
  *   <li>[[slick.relational.RelationalCapabilities.pagingNested]]:
  *     See <a href="https://issues.apache.org/jira/browse/DERBY-5911"
  *     target="_parent">DERBY-5911</a>.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequenceCurr]]:
  *     <code>Sequence.curr</code> to get the current value of a sequence is
  *     not supported by Derby. Trying to generate SQL code which uses this
  *     feature throws a SlickException.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequenceCycle]]:
  *     Sequence cycling is supported but does not conform to SQL:2008
  *     semantics. Derby cycles back to the START value instead of MINVALUE or
  *     MAXVALUE.</li>
  *   <li>[[slick.relational.RelationalCapabilities.zip]]:
  *     Ordered sub-queries and window functions with orderings are currently
  *     not supported by Derby. These are required by <code>zip</code> and
  *     <code>zipWithIndex</code>. Trying to generate SQL code which uses this
  *     feature causes the DB to throw an exception. We do not prevent these
  *     queries from being generated because we expect future Derby versions to
  *     support them with the standard SQL:2003 syntax (see
  *     <a href="http://wiki.apache.org/db-derby/OLAPRowNumber" target="_parent"
  *     >http://wiki.apache.org/db-derby/OLAPRowNumber</a>).</li>
  *   <li>[[slick.relational.RelationalCapabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side because there
  *     is no native support for them (but there is work in progress: see
  *     <a href="https://issues.apache.org/jira/browse/DERBY-3155"
  *     target="_parent" >DERBY-3155</a>).</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
  *     Derby <= 10.6 doesn't have booleans, so Slick maps to SMALLINT instead.
  *     Other jdbc drivers like MySQL map TINYINT(1) back to a Scala
  *     Boolean. Derby maps SMALLINT to an Integer and that's how it shows
  *     up in the jdbc meta data, thus the original type is lost.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     Derby doesn't have a corresponding type for Byte.
  *     SMALLINT is used instead and mapped to Short in the Slick model.</li>
  *   <li>[[slick.relational.RelationalCapabilities.repeat]]:
  *     There's not builtin string function repeat in Derby.
  *     <a href="https://db.apache.org/derby/docs/10.10/ref/rrefsqlj29026.html" target="_parent"
  *     >https://db.apache.org/derby/docs/10.10/ref/rrefsqlj29026.html</a></li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnMultipleInsertKey]]:
  *     Derby's driver is unable to properly return the generated key for insert statements with multiple values.
  *     (<a href="https://issues.apache.org/jira/browse/DERBY-3609" target="_parent"></a>DERBY-3609</li>)
  * </ul>
  */
trait DerbyProfile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport {

  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities -
      RelationalCapabilities.functionDatabase -
      RelationalCapabilities.pagingNested -
      JdbcCapabilities.returnInsertOther -
      SqlCapabilities.sequenceCurr -
      // Cycling is broken in Derby. It cycles to the start value instead of min or max
      SqlCapabilities.sequenceCycle -
      RelationalCapabilities.zip -
      RelationalCapabilities.joinFull -
      JdbcCapabilities.insertOrUpdate -
      JdbcCapabilities.insertOrUpdateWithPrimaryKeyOnly -
      RelationalCapabilities.replace -
      RelationalCapabilities.reverse -
      JdbcCapabilities.booleanMetaData -
      JdbcCapabilities.supportsByte -
      RelationalCapabilities.repeat -
      JdbcCapabilities.returnMultipleInsertKey

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext)
    extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {

    override def createTableNamer(mTable: MTable): TableNamer = new DerbyTableNamer(mTable)
    class DerbyTableNamer(mTable: MTable) extends TableNamer(mTable) {
      override def schema = super.schema.filter(_ != "APP") // remove default schema
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)
                                 (implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] =
    MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    new JdbcSchemaActionExtensionMethodsImpl(schema) {
      override def createIfNotExists: ProfileAction[Unit, NoStream, Effect.Schema] =
        new SimpleJdbcProfileAction[Unit]("schema.createIfNotExists", schema.createIfNotExistsStatements.toVector) {
          def run(ctx: JdbcBackend#JdbcActionContext, sql: Vector[String]): Unit = {
            import java.sql.SQLException
            for (s <- sql) try {
              ctx.session.withPreparedStatement(s)(_.execute)
            } catch {
              //<value> '<value>' already exists in <value> '<value>'.
              case e: SQLException if e.getSQLState.equals("X0Y32") => ()
              case e: Throwable                                     => throw e
            }
          }
        }

      override def dropIfExists: ProfileAction[Unit, NoStream, Effect.Schema] =
        new SimpleJdbcProfileAction[Unit]("schema.dropIfExists", schema.dropIfExistsStatements.toVector) {
          def run(ctx: JdbcBackend#JdbcActionContext, sql: Vector[String]): Unit = {
            import java.sql.SQLException
            for (s <- sql) try {
              ctx.session.withPreparedStatement(s)(_.execute)
            } catch {
              //'<value>' cannot be performed on '<value>' because it does not exist.
              case e: SQLException if e.getSQLState.equals("42Y55") => ()
              case e: Throwable                                     => throw e
            }
          }
        }
    }

  override protected def computeQueryCompiler =
    super.computeQueryCompiler + Phase.rewriteBooleans + Phase.specializeParameters
  override val columnTypes: DerbyJdbcTypes = new DerbyJdbcTypes
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new DerbyQueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[?]): TableDDLBuilder = new DerbyTableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): ColumnDDLBuilder =
    new DerbyColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[?]): SequenceDDLBuilder = new DerbySequenceDDLBuilder(seq)

  override def defaultSqlTypeName(tmd: JdbcType[?], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.BOOLEAN => "SMALLINT"
    /* Derby does not have a TINYINT type, so we use SMALLINT instead. */
    case java.sql.Types.TINYINT => "SMALLINT"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  override val scalarFrom: Some[String] = Some("sysibm.sysdummy1")

  class DerbyQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val concatOperator: Some[String] = Some("||")
    override protected val supportsTuples = false
    override protected val supportsLiteralGroupBy = true
    override protected val quotedJdbcFns: Some[Vector[Library.JdbcFunction]] = Some(Vector(Library.User))

    override protected def buildForUpdateClause(forUpdate: Boolean) = {
      super.buildForUpdateClause(forUpdate)
      if (forUpdate) {
        b" with RS "
      }
    }

    override def expr(c: Node): Unit = c match {
      case Library.Cast(ch*) =>
        /* Work around DERBY-2072 by casting numeric values first to CHAR and
         * then to VARCHAR. */
        val (toVarchar, tn) = {
          val tn =
            (if(ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
            else jdbcTypeFor(c.nodeType).sqlTypeName(None)).toLowerCase
          if(tn == "varchar") (true, columnTypes.stringJdbcType.sqlTypeName(None))
          else if(tn.startsWith("varchar")) (true, tn)
          else (false, tn)
        }
        if(toVarchar && jdbcTypeFor(ch.head.nodeType).isInstanceOf[NumericTypedType])
          b"trim(cast(cast(${ch.head} as char(30)) as $tn))"
        else b"cast(${ch.head} as $tn)"
      case Library.IfNull(l, r) =>
        /* Derby does not support IFNULL so we use COALESCE instead,
         * and it requires NULLs to be casted to a suitable type */
        b"coalesce(cast($l as ${jdbcTypeFor(c.nodeType).sqlTypeName(None)}),!$r)"
      case Library.SilentCast(LiteralNode(None)) :@ JdbcType(ti, _) if currentPart == SelectPart =>
        // Cast NULL to the correct type
        b"cast(null as ${ti.sqlTypeName(None)})"
      case LiteralNode(None) :@ JdbcType(ti, _) if currentPart == SelectPart =>
        // Cast NULL to the correct type
        b"cast(null as ${ti.sqlTypeName(None)})"
      case (c @ LiteralNode(v)) :@ JdbcType(ti, option) if currentPart == SelectPart =>
        /* The Derby embedded driver has a bug (DERBY-4671) which results in a
         * NullPointerException when using bind variables in a SELECT clause.
         * This should be fixed in Derby 10.6.1.1. The workaround is to add an
         * explicit type annotation (in the form of a CAST expression). */
        if(c.volatileHint || !ti.hasLiteralForm) {
          b"cast("
          b +?= { (p, idx, _) =>
            if (option) ti.setOption(v.asInstanceOf[Option[Any]], p, idx) else ti.setValue(v, p, idx)
          }
          b" as ${ti.sqlTypeName(None)})"
        } else super.expr(c)
      case Library.NextValue(SequenceNode(name)) => b"(next value for `$name)"
      case Library.CurrentValue(_*) => throw new SlickException("Derby does not support CURRVAL")
      case Union(left, right, all) =>
        b"\{"
        buildFrom(left, None, skipParens = true)
        if(all) b"\nunion all " else b"\nunion "
        b"\["
        buildFrom(right, None, skipParens = true)
        b"\]"
        b"\}"
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.SilentCast, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.IfNull, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case c@Comprehension(_, _, _, Some(n @ Apply(Library.IfNull, _)), _, _, _, _, _, _, _) =>
        super.expr(c.copy(where = Some(RewriteBooleans.rewriteFakeBooleanEqOne(n))))
      case _ => super.expr(c)
    }
  }

  class DerbyTableDDLBuilder(table: Table[?])
    extends TableDDLBuilder(table)
      with TableDDLBuilder.UniqueIndexAsConstraint {
    override def dropIfExistsPhase = dropPhase2 // Derby does not support "DROP IF EXISTS"
    override def createIfNotExistsPhase = createPhase1 ++ createPhase2
  }

  class DerbyColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " GENERATED BY DEFAULT AS IDENTITY"
      if( unique ) sb append " UNIQUE"
    }
  }

  class DerbySequenceDDLBuilder[T](seq: Sequence[T])
    extends SequenceDDLBuilder.BuiltInSupport.OverrideActualStart(seq)
      with SequenceDDLBuilder.BuiltInSupport.IncrementBy
      with SequenceDDLBuilder.BuiltInSupport.StartWith

  class DerbyJdbcTypes extends JdbcTypes {
    override val booleanJdbcType: DerbyBooleanJdbcType = new DerbyBooleanJdbcType
    override val uuidJdbcType: DerbyUUIDJdbcType = new DerbyUUIDJdbcType
    override val instantType: DerbyInstantJdbcType = new DerbyInstantJdbcType

    /* Derby does not have a proper BOOLEAN type. The suggested workaround is
     * SMALLINT with constants 1 and 0 for TRUE and FALSE. */
    class DerbyBooleanJdbcType extends BooleanJdbcType {
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }

    class DerbyUUIDJdbcType extends UUIDJdbcType {
      override def sqlType = java.sql.Types.BINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(16) FOR BIT DATA"
      override def valueToSQLLiteral(value: UUID): String =
        "x'" + value.toString.replace("-", "") + "'"
    }

    class DerbyInstantJdbcType extends InstantJdbcType {
      // Derby has no timestamp with timezone type and so using strings as timestamps are
      // susceptible to DST gaps twice a year
      override def sqlType: Int = java.sql.Types.VARCHAR
      override def setValue(v: Instant, p: PreparedStatement, idx: Int): Unit = {
        p.setString(idx, v.toString)
      }
      override def getValue(r: ResultSet, idx: Int): Instant = {
        r.getString(idx) match {
          case null => null
          case instantString => Instant.parse(instantString)
        }
      }
      override def updateValue(v: Instant, r: ResultSet, idx: Int): Unit = {
        r.updateString(idx, v.toString)
      }
      override def valueToSQLLiteral(value: Instant) = {
        s"'${value.toString}'"
      }
    }
  }
}

object DerbyProfile extends DerbyProfile
