package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, LocalDateTime}

import scala.concurrent.ExecutionContext

import slick.SlickException
import slick.ast.*
import slick.ast.TypeUtil.*
import slick.ast.Util.*
import slick.basic.Capability
import slick.compiler.{CompilerState, Phase, ResolveZipJoins}
import slick.dbio.DBIO
import slick.jdbc.meta.{MColumn, MPrimaryKey, MTable}
import slick.lifted.*
import slick.relational.{RelationalCapabilities, RelationalProfile}
import slick.sql.SqlCapabilities
import slick.util.{ConstArray, GlobalConfig, SlickLogger}
import slick.util.ConfigExtensionMethods.configExtensionMethods
import slick.util.QueryInterpolator.queryInterpolator

import com.typesafe.config.Config

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
trait MySQLProfile extends JdbcProfile with JdbcActionComponent.MultipleRowsPerStatementSupport { profile =>
  import MySQLProfile.{RowNum, RowNumGen}

  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities -
      JdbcCapabilities.returnInsertOther -
      SqlCapabilities.sequenceLimited -
      RelationalCapabilities.joinFull -
      JdbcCapabilities.nullableNoDefault -
      JdbcCapabilities.distinguishesIntTypes //https://github.com/slick/slick/pull/1735

  override protected[this] def loadProfileConfig: Config = {
    if(!GlobalConfig.profileConfig("slick.driver.MySQL").entrySet().isEmpty)
      SlickLogger[MySQLProfile].warn(
        "The config key 'slick.driver.MySQL' is deprecated and not used anymore. Use 'slick.jdbc.MySQLProfile' instead."
      )
    super.loadProfileConfig
  }

  class MySQLModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext)
    extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey]): PrimaryKeyBuilder =
      new MySQLPrimaryKeyBuilder(tableBuilder, meta)
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder =
      new MySQLColumnBuilder(tableBuilder, meta)
    override def createTableNamer(meta: MTable): TableNamer = new MySQLTableNamer(meta)

    class MySQLPrimaryKeyBuilder(tableBuilder: TableBuilder, meta: Seq[MPrimaryKey])
      extends PrimaryKeyBuilder(tableBuilder, meta) {
      // TODO: this needs a test
      override def name = super.name.filter(_ != "PRIMARY")
    }

    class MySQLColumnBuilder(tableBuilder: TableBuilder, meta: MColumn) extends ColumnBuilder(tableBuilder, meta) {
      override def default = meta.columnDef.map((_,tpe)).collect{
        case (v,"String")    => Some(Some(v))
        case ("1"|"b'1'", "Boolean") => Some(Some(true))
        case ("0"|"b'0'", "Boolean") => Some(Some(false))
        case ( v , "scala.math.BigDecimal") => Some( Some( scala.math.BigDecimal(v) ) )
      }.getOrElse{
        val d = super.default
        if(meta.nullable.contains(true) && d.isEmpty){
          Some(None)
        } else d
      }
      override def length: Option[Int] = {
        val l = super.length
        if(tpe == "String" && varying && l.contains(65535)) None
        else l
      }
    }

    //Reference: https://github.com/slick/slick/issues/1419
    class MySQLTableNamer(meta: MTable) extends TableNamer(meta){
      override def schema = meta.name.catalog
      override def catalog = meta.name.schema
    }

    //https://dev.mysql.com/doc/connector-j/8.0/en/connector-j-reference-type-conversions.html
    import scala.reflect.{classTag, ClassTag}
    override def jdbcTypeToScala(jdbcType: Int, typeName: String = ""): ClassTag[?] = {
      import java.sql.Types.*
      jdbcType match{
        case TINYINT if typeName.contains("UNSIGNED")  =>  classTag[Short]
        case SMALLINT                                  =>  classTag[Int]
        case INTEGER if typeName.contains("UNSIGNED")  =>  classTag[Long]
        /*
         * Currently java.math.BigInteger/scala.math.BigInt isn't supported as a default datatype,
         * so this is currently out of scope.
         */
//        case BIGINT  if typeName.contains("UNSIGNED")  =>  classTag[BigInt]
        case _ => super.jdbcTypeToScala(jdbcType, typeName)
      }
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)
                                 (implicit ec: ExecutionContext): JdbcModelBuilder =
    new MySQLModelBuilder(tables, ignoreInvalidDefaults)
  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] ={
    for {
      catalog <- SimpleJdbcAction(_.session.conn.getCatalog)
      mTables <- MTable.getTables(Some(catalog), None, Some("%"), Some(Seq("TABLE")))
    } yield mTables
  }

  override val columnTypes: MySQLJdbcTypes = new MySQLJdbcTypes
  override protected def computeQueryCompiler =
    super.computeQueryCompiler.replace(new MySQLResolveZipJoins) - Phase.fixRowNumberOrdering
  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new MySQLQueryBuilder(n, state)
  override def createUpsertBuilder(node: Insert): InsertBuilder = new MySQLUpsertBuilder(node)
  override def createTableDDLBuilder(table: Table[?]): TableDDLBuilder = new MySQLTableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): ColumnDDLBuilder =
    new MySQLColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[?]): SequenceDDLBuilder = new MySQLSequenceDDLBuilder(seq)

  override def quoteIdentifier(id: String) = s"`$id`"

  override def defaultSqlTypeName(tmd: JdbcType[?], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length]) match {
        case Some(l) => if(l.varying){
            //http://dev.mysql.com/doc/refman/5.7/en/string-type-overview.html
            if(l.length <= 65535 ) s"VARCHAR(${l.length})"
            else if(l.length <= 16777215 ) "MEDIUMTEXT"
            else "LONGTEXT"
          } else s"CHAR(${l.length})"
        case None => defaultStringType match {
          case Some(s) => s
          case None =>
            if(sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Default[?]]).isDefined ||
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

  class MySQLQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val supportsCast = false
    override protected val parenthesizeNestedRHSJoin = true
    override protected val quotedJdbcFns: Some[Nil.type] = Some(Nil)

    override def expr(n: Node): Unit = n match {
      case Library.Cast(ch) :@ JdbcType(ti, _) =>
        val tn =
          if (ti == columnTypes.stringJdbcType)
            "VARCHAR"
          else if (ti == columnTypes.bigDecimalJdbcType)
            "DECIMAL"
          else
            ti.sqlTypeName(None)
        b"\({fn convert(!$ch,$tn)}\)"
      case Library.NextValue(SequenceNode(name)) => b"`${name + "_nextval"}()"
      case Library.CurrentValue(SequenceNode(name)) => b"`${name + "_currval"}()"
      case RowNum(sym, true) => b"(@`$sym := @`$sym + 1)"
      case RowNum(sym, false) => b"@`$sym"
      case RowNumGen(sym, init) => b"@`$sym := $init"
      case Union(left, right, all) =>
        b"\{"
        buildFrom(left, None, false)
        if (all) b"\nunion all " else b"\nunion "
        buildFrom(right, None, false)
        b"\}"
      case Apply(Library.In, children) if children.exists(_.isInstanceOf[Union]) =>
        b"\("
        if(children.length == 1) {
          b"${Library.In.name} ${children.head}"
        } else b.sep(children, " " + Library.In.name + " ") {
          case u @ Union(_,_,_) =>
            b"(select t.* from ("
            expr(u, skipParens)
            b") t)"
          case node => expr(node)
        }
        b"\)"
      case _ => super.expr(n)
    }

    override protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b"\nlimit $d,$t"
      case (Some(t), None   ) => b"\nlimit $t"
      case (None,    Some(d)) => b"\nlimit $d,18446744073709551615"
      case _ =>
    }

    override protected def buildOrdering(n: Node, o: Ordering): Unit = {
      if(o.nulls.last && !o.direction.desc)
        b"isnull($n),"
      else if(o.nulls.first && o.direction.desc)
        b"isnull($n) desc,"
      expr(n, false)
      if(o.direction.desc) b" desc"
    }

    // Override default DELETE FROM syntax in order to produce a more efficient
    // DELETE query for MySQL.
    //
    // Slick cannot directly handle multi-table DELETEs, i.e., using JOIN or
    // USING, but it can handle subqueries in the WHERE clause of a DELETE.
    // This is good except for the fact that MySQL doesn't know how to
    // optimize such semi-join subqueries to joins in single-table DELETE
    // queries. However, if the DELETE query is a multi-table DELETE, even if
    // on a single table, then something in MySQL kicks in and optimizes the
    // subquery to a more efficient JOIN. Further reading:
    //
    // - http://mysqlserverteam.com/multi-table-trick
    // - https://mariadb.com/kb/en/mariadb/semi-join-subquery-optimizations
    //
    override protected def buildDeleteFrom(tableName: String): Unit = {
      b"delete $tableName from $tableName"
    }
  }

  class MySQLUpsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult = {
      val start = buildInsertStart
      val names = syms.filter(!_.existsColumnOption[ColumnOption.AutoInc.type]).map(s => quoteIdentifier(s.name))
      val update =
        if (names.isEmpty) ""
        else s"on duplicate key update ${names.map(n => s"$n=VALUES($n)").mkString(", ")}"
      def makeValues(size: Int) =
        (1 to size).map(_ => allVars).mkString(",")
      def makeSql(size: Int) = s"$start values ${makeValues(size)} $update"
      new InsertBuilderResult(table, makeSql(1), syms) {
        override def buildMultiRowInsert(size: Int) = makeSql(size)
      }
    }
  }

  class MySQLTableDDLBuilder(table: Table[?]) extends TableDDLBuilder(table) {
    override protected def dropForeignKey(fk: ForeignKey) = {
      "ALTER TABLE " + quoteTableName(table.tableNode) + " DROP FOREIGN KEY " + quoteIdentifier(fk.name)
    }
    override protected def dropPrimaryKey(pk: PrimaryKey): String = {
      "ALTER TABLE " + quoteTableName(table.tableNode) + " DROP PRIMARY KEY"
    }
  }

  class MySQLColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      else if(sqlType.toUpperCase == "TIMESTAMP") sb append " NULL"
      if(autoIncrement) sb append " AUTO_INCREMENT"
      if(primaryKey) sb append " PRIMARY KEY"
      if( unique ) sb append " UNIQUE"
    }
  }

  class MySQLSequenceDDLBuilder[T](seq: Sequence[T]) extends SequenceDDLBuilder {
    override def buildDDL: DDL = {
      import seq.integral.*
      val sqlType = profile.jdbcTypeFor(seq.tpe).sqlTypeName(None)
      val t = sqlType + " not null"
      val increment = seq._increment.getOrElse(one)
      val desc = increment < zero
      val minValue = seq._minValue getOrElse (if(desc) fromInt(java.lang.Integer.MIN_VALUE) else one)
      val maxValue = seq._maxValue getOrElse (if(desc) fromInt(-1) else fromInt(java.lang.Integer.MAX_VALUE))
      val start = seq._start.getOrElse(if(desc) maxValue else minValue)
      val beforeStart = start - increment
      if(!seq._cycle && (seq._minValue.isDefined && desc || seq._maxValue.isDefined && !desc))
        throw new SlickException(
          "Sequences with limited size and without CYCLE are not supported by MySQLProfile's sequence emulation"
        )
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
            quoteIdentifier(seq.name + "_seq") +
            " set id=last_insert_id(" + incExpr + "); return last_insert_id(); end",
          "create function " + quoteIdentifier(seq.name + "_currval") + "() returns " + sqlType + " begin " +
            "select max(id) into @v from " + quoteIdentifier(seq.name + "_seq") + "; return @v; end"),
        Iterable(
          "drop function " + quoteIdentifier(seq.name + "_currval"),
          "drop function " + quoteIdentifier(seq.name + "_nextval"),
          "drop table " + quoteIdentifier(seq.name + "_seq"))
      )
    }
  }

  class MySQLJdbcTypes extends JdbcTypes {

    @inline
    private[this] def stringToMySqlString(value : String) : String = {
      value match {
        case null => "NULL"
        case _ =>
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

    override val stringJdbcType = new StringJdbcType {
      override def valueToSQLLiteral(value: String) : String = {
        stringToMySqlString(value)
      }
    }

    import java.util.UUID

    override val uuidJdbcType = new UUIDJdbcType {
      override def sqlType = java.sql.Types.BINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "BINARY(16)"

      override def valueToSQLLiteral(value: UUID): String =
        "x'"+value.toString.replace("-", "")+"'"
    }

    override val instantType : InstantJdbcType = new InstantJdbcType {
      override def sqlType : Int = {
        /**
         * [[Instant]] will be persisted as a [[java.sql.Types.VARCHAR]] in order to
         * avoid losing precision, because MySQL stores [[java.sql.Types.TIMESTAMP]] with
         * second precision, while [[Instant]] stores it with a millisecond one.
         */
        java.sql.Types.VARCHAR
      }
      override def setValue(v: Instant, p: PreparedStatement, idx: Int) : Unit = {
        p.setString(idx, if (v == null) null else v.toString)
      }
      override def getValue(r: ResultSet, idx: Int) : Instant = {
        r.getString(idx) match {
          case null => null
          case iso8601String => Instant.parse(iso8601String)
        }
      }
      override def updateValue(v: Instant, r: ResultSet, idx: Int) = {
        r.updateString(idx, if (v == null) null else v.toString)
      }
      override def valueToSQLLiteral(value: Instant) : String = {
        stringToMySqlString(value.toString)
      }
    }

    override val localDateTimeType : LocalDateTimeJdbcType = new LocalDateTimeJdbcType {
      override def sqlType : Int = {
        /**
         * [[LocalDateTime]] will be persisted as a [[java.sql.Types.VARCHAR]] in order to
         * avoid losing precision, because MySQL stores [[java.sql.Types.TIMESTAMP]] with
         * second precision, while [[LocalDateTime]] stores it with a millisecond one.
         */
        java.sql.Types.VARCHAR
      }
      override def setValue(v: LocalDateTime, p: PreparedStatement, idx: Int) : Unit = {
        p.setString(idx, if (v == null) null else v.toString)
      }
      override def getValue(r: ResultSet, idx: Int) : LocalDateTime = {
        r.getString(idx) match {
          case null => null
          case iso8601String => LocalDateTime.parse(iso8601String)
        }
      }
      override def updateValue(v: LocalDateTime, r: ResultSet, idx: Int) = {
        r.updateString(idx, if (v == null) null else v.toString)
      }
      override def valueToSQLLiteral(value: LocalDateTime) : String = {
        stringToMySqlString(value.toString)
      }
    }
  }
}

object MySQLProfile extends MySQLProfile {
  final case class RowNum(sym: AnonSymbol, inc: Boolean) extends NullaryNode with SimplyTypedNode {
    type Self = RowNum
    override def self = this
    override def buildType: ScalaNumericType[Long] = ScalaBaseType.longType
    def rebuild = copy()
  }

  final case class RowNumGen(sym: AnonSymbol, init: Long) extends NullaryNode with SimplyTypedNode {
    type Self = RowNumGen
    override def self = this
    override def buildType: ScalaNumericType[Long] = ScalaBaseType.longType
    def rebuild = copy()
  }
}
