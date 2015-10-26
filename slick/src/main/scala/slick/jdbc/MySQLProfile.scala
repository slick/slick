package slick.jdbc

import com.typesafe.config.Config

import scala.concurrent.ExecutionContext

import slick.SlickException
import slick.ast._
import slick.ast.Util._
import slick.ast.TypeUtil._
import slick.basic.Capability
import slick.compiler.{Phase, ResolveZipJoins, CompilerState}
import slick.jdbc.meta.{MPrimaryKey, MColumn, MTable}
import slick.lifted._
import slick.model.Model
import slick.relational.{RelationalProfile, RelationalCapabilities}
import slick.sql.SqlCapabilities
import slick.util.{SlickLogger, GlobalConfig, ConstArray}
import slick.util.MacroSupport.macroSupportInterpolation
import slick.util.ConfigExtensionMethods.configExtensionMethods

/** Slick profile for MySQL.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnInsertOther]]:
  *     When returning columns from an INSERT operation, only a single column
  *     may be specified which must be the table's AutoInc column.</li>
  *   <li>[[slick.sql.SqlCapabilities.sequenceLimited]]:
  *     Non-cyclic sequence may not have an upper limit.</li>
  *   <li>[[slick.relational.RelationalCapabilities.joinFull]]:
  *     Full outer joins are emulated because there is not native support
  *     for them.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.nullableNoDefault]]:
  *     Nullable columns always have NULL as a default according to the SQL
  *     standard. Consequently MySQL treats no specifying a default value
  *     just as specifying NULL and reports NULL as the default value.
  *     Some other dbms treat queries with no default as NULL default, but
  *     distinguish NULL from no default value in the meta data.</li>
  * </ul>
  *
  * Sequences are supported through an emulation which requires the schema to
  * be created by Slick. You can also use an existing schema with your own
  * sequence emulation if you provide for each sequence ''s'' a pair of
  * functions <code>s_nextval</code> and <code>s_currval</code>.
  *
  * The default type for strings of unlimited length is "TEXT", falling back to
  * "VARCHAR(254)" if a `Default` or `PrimaryKey` column option is set. This can be
  * changed by overriding <code>slick.jdbc.MySQLProfile.defaultStringType</code>
  * in application.conf.
  *
  * Note: Slick 3.2 also checks the old config path "slick.driver.MySQL" that was used
  * by Slick 3.0 and 3.1, and logs a warning if anything is found there. Values from the
  * old path are *not* used anymore. This deprecation warning will be removed in a
  * future version.
  */
trait MySQLProfile extends JdbcProfile { profile =>
  import MySQLProfile.{RowNum, RowNumGen}

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - JdbcCapabilities.returnInsertOther
    - SqlCapabilities.sequenceLimited
    - RelationalCapabilities.joinFull
    - JdbcCapabilities.nullableNoDefault
  )

  override protected[this] def loadProfileConfig: Config = {
    if(!GlobalConfig.profileConfig("slick.driver.MySQL").entrySet().isEmpty)
      SlickLogger[MySQLProfile].warn("The config key 'slick.driver.MySQL' is deprecated and not used anymore. Use 'slick.jdbc.MySQLProfile' instead.")
    super.loadProfileConfig
  }

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder = new PrimaryKeyBuilder(tableBuilder, meta) {
      // TODO: this needs a test
      override def name = super.name.filter(_ != "PRIMARY")
    }
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      override def default = meta.columnDef.map((_,tpe)).collect{
        case (v,"String")    => Some(Some(v))
        case ("1","Boolean") => Some(Some(true))
        case ("0","Boolean") => Some(Some(false))
      }.getOrElse{
        val d = super.default
        if(meta.nullable == Some(true) && d == None){
          Some(None)
        } else d
      }
      override def length: Option[Int] = {
        val l = super.length
        if(tpe == "String" && varying && l == Some(65535)) None
        else l
      }
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override val columnTypes = new JdbcTypes
  override protected def computeQueryCompiler = super.computeQueryCompiler.replace(new MySQLResolveZipJoins) - Phase.fixRowNumberOrdering
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder[_] = new SequenceDDLBuilder(seq)

  override def quoteIdentifier(id: String) = '`' + id + '`'

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length]) match {
        case Some(l) => if(l.varying) s"VARCHAR(${l.length})" else s"CHAR(${l.length})"
        case None => defaultStringType match {
          case Some(s) => s
          case None =>
            if(sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Default[_]]).isDefined ||
               sym.flatMap(_.findColumnOption[ColumnOption.PrimaryKey.type]).isDefined)
              "VARCHAR(254)" else "TEXT"
        }
      }
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  protected lazy val defaultStringType = profileConfig.getStringOpt("defaultStringType")

  class MySQLResolveZipJoins extends ResolveZipJoins {
    // MySQL does not support ROW_NUMBER() but you can manually increment a variable in the SELECT
    // clause to emulate it. See http://stackoverflow.com/a/1895127/458687 for an example.
    // According to http://dev.mysql.com/doc/refman/5.0/en/user-variables.html this should not be
    // relied on but it is the generally accepted solution and there is no better way.
    override def transformZipWithIndex(s1: TermSymbol, ls: TermSymbol, from: Node,
                                       defs: ConstArray[(TermSymbol, Node)], offset: Long, p: Node): Node = {
      val countSym = new AnonSymbol
      val j = Join(new AnonSymbol, new AnonSymbol,
        Bind(ls, from, Pure(StructNode(defs))),
        Bind(new AnonSymbol, Pure(StructNode(ConstArray.empty)),
          Pure(StructNode(ConstArray(new AnonSymbol -> RowNumGen(countSym, offset-1))))),
        JoinType.Inner, LiteralNode(true))
      var first = true
      Subquery(Bind(s1, j, p.replace {
        case Select(Ref(s), ElementSymbol(2)) if s == s1 =>
          val r = RowNum(countSym, first)
          first = false
          r
        case r @ Ref(s) if s == s1 => r.untyped
      }), Subquery.Default).infer()
    }
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsCast = false
    override protected val parenthesizeNestedRHSJoin = true
    override protected val quotedJdbcFns = Some(Nil)

    override def expr(n: Node, skipParens: Boolean = false): Unit = n match {
      case Library.Cast(ch) :@ JdbcType(ti, _) =>
        val tn = if(ti == columnTypes.stringJdbcType) "VARCHAR" else ti.sqlTypeName(None)
        b"\({fn convert(!${ch},$tn)}\)"
      case Library.NextValue(SequenceNode(name)) => b"`${name + "_nextval"}()"
      case Library.CurrentValue(SequenceNode(name)) => b"`${name + "_currval"}()"
      case RowNum(sym, true) => b"(@`$sym := @`$sym + 1)"
      case RowNum(sym, false) => b"@`$sym"
      case RowNumGen(sym, init) => b"@`$sym := $init"
      case _ => super.expr(n, skipParens)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $d,$t"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None,    Some(d)) => b"\nlimit $d,18446744073709551615"
      case _ =>
    }

    override protected def buildOrdering(n: Node, o: Ordering) {
      if(o.nulls.last && !o.direction.desc)
        b"isnull($n),"
      else if(o.nulls.first && o.direction.desc)
        b"isnull($n) desc,"
      expr(n)
      if(o.direction.desc) b" desc"
    }
  }

  class UpsertBuilder(ins: Insert) extends super.UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult = {
      val start = buildInsertStart
      val update = softNames.map(n => s"$n=VALUES($n)").mkString(", ")
      new InsertBuilderResult(table, s"$start values $allVars on duplicate key update $update", syms)
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected def dropForeignKey(fk: ForeignKey) = {
      "ALTER TABLE " + table.tableName + " DROP FOREIGN KEY " + fk.name
    }
    override protected def dropPrimaryKey(pk: PrimaryKey): String = {
      "ALTER TABLE " + table.tableName + " DROP PRIMARY KEY"
    }
  }

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      else if(sqlType.toUpperCase == "TIMESTAMP") sb append " NULL"
      if(autoIncrement) sb append " AUTO_INCREMENT"
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      import seq.integral._
      val sqlType = profile.jdbcTypeFor(seq.tpe).sqlTypeName(None)
      val t = sqlType + " not null"
      val increment = seq._increment.getOrElse(one)
      val desc = increment < zero
      val minValue = seq._minValue getOrElse (if(desc) fromInt(java.lang.Integer.MIN_VALUE) else one)
      val maxValue = seq._maxValue getOrElse (if(desc) fromInt(-1) else fromInt(java.lang.Integer.MAX_VALUE))
      val start = seq._start.getOrElse(if(desc) maxValue else minValue)
      val beforeStart = start - increment
      if(!seq._cycle && (seq._minValue.isDefined && desc || seq._maxValue.isDefined && !desc))
        throw new SlickException("Sequences with limited size and without CYCLE are not supported by MySQLProfile's sequence emulation")
      val incExpr = if(seq._cycle) {
        if(desc) "if(id-"+(-increment)+"<"+minValue+","+maxValue+",id-"+(-increment)+")"
        else "if(id+"+increment+">"+maxValue+","+minValue+",id+"+increment+")"
      } else {
        "id+("+increment+")"
      }
      DDL(
        Iterable(
          "create table " + quoteIdentifier(seq.name + "_seq") + " (id " + t + ")",
          "insert into " + quoteIdentifier(seq.name + "_seq") + " values (" + beforeStart + ")",
          "create function " + quoteIdentifier(seq.name + "_nextval") + "() returns " + sqlType + " begin update " +
            quoteIdentifier(seq.name + "_seq") + " set id=last_insert_id(" + incExpr + "); return last_insert_id(); end",
          "create function " + quoteIdentifier(seq.name + "_currval") + "() returns " + sqlType + " begin " +
            "select max(id) into @v from " + quoteIdentifier(seq.name + "_seq") + "; return @v; end"),
        Iterable(
          "drop function " + quoteIdentifier(seq.name + "_currval"),
          "drop function " + quoteIdentifier(seq.name + "_nextval"),
          "drop table " + quoteIdentifier(seq.name + "_seq"))
      )
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val stringJdbcType = new StringJdbcType {
      override def valueToSQLLiteral(value: String) = if(value eq null) "NULL" else {
        val sb = new StringBuilder
        sb append '\''
        for(c <- value) c match {
          case '\'' => sb append "\\'"
          case '"' => sb append "\\\""
          case 0 => sb append "\\0"
          case 26 => sb append "\\Z"
          case '\b' => sb append "\\b"
          case '\n' => sb append "\\n"
          case '\r' => sb append "\\r"
          case '\t' => sb append "\\t"
          case '\\' => sb append "\\\\"
          case _ => sb append c
        }
        sb append '\''
        sb.toString
      }
    }

    import java.util.UUID

    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlType = java.sql.Types.BINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "BINARY(16)"

      override def valueToSQLLiteral(value: UUID): String =
        "x'"+value.toString.replace("-", "")+"'"
    }
  }
}

object MySQLProfile extends MySQLProfile {
  final case class RowNum(sym: AnonSymbol, inc: Boolean) extends NullaryNode with SimplyTypedNode {
    type Self = RowNum
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }

  final case class RowNumGen(sym: AnonSymbol, init: Long) extends NullaryNode with SimplyTypedNode {
    type Self = RowNumGen
    def buildType = ScalaBaseType.longType
    def rebuild = copy()
  }
}
