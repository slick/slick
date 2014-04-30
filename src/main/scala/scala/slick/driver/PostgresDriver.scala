package scala.slick.driver

import java.util.UUID
import java.sql.{PreparedStatement, ResultSet}
import scala.slick.lifted._
import scala.slick.ast._
import scala.slick.util.MacroSupport.macroSupportInterpolation
import scala.slick.compiler.CompilerState
import scala.slick.jdbc.meta.MTable
import scala.slick.jdbc.{Invoker, JdbcType}

/** Slick driver for PostgreSQL.
  *
  * This driver implements all capabilities of [[scala.slick.driver.JdbcProfile]].
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
  */
trait PostgresDriver extends JdbcDriver { driver =>

  override def getTables: Invoker[MTable] = MTable.getTables(None, None, None, Some(Seq("TABLE")))

  override val columnTypes = new JdbcTypes
  override val simple: SimpleQL with Implicits = new SimpleQL with Implicits {}
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

  trait SimpleQL extends super.SimpleQL {
    type InheritingTable = driver.InheritingTable
    type UnaryAggFuncPartsBasic[T,R] = driver.UnaryAggFuncPartsBasic[T,R]
    type BinaryAggFuncPartsBasic[T,R] = driver.BinaryAggFuncPartsBasic[T,R]
    type TernaryAggFuncPartsBasic[T,R] = driver.TernaryAggFuncPartsBasic[T,R]
  }

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val concatOperator = Some("||")
    override protected val supportsEmptyJoinConditions = false

    override protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = (fetch, offset) match {
      case (Some(t), Some(d)) => b" limit $t offset $d"
      case (Some(t), None   ) => b" limit $t"
      case (None,    Some(d)) => b" offset $d"
      case _ =>
    }

    override def expr(n: Node, skipParens: Boolean = false) = n match {
      case Library.NextValue(SequenceNode(name)) => b"nextval('$name')"
      case Library.CurrentValue(SequenceNode(name)) => b"currval('$name')"
      case c: AggFuncInputs =>
        if (c.modifier.isDefined) b"${c.modifier.get} "
        b.sep(c.aggParams, ",")(expr(_, true))
        if (c.orderBy.nonEmpty) buildOrderByClause(c.orderBy)
      case _ => super.expr(n, skipParens)
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override protected val columns: Iterable[ColumnDDLBuilder] = {
      (if(table.isInstanceOf[InheritingTable]) {
        val hColumns = table.asInstanceOf[InheritingTable].inherited.create_*.toSeq.map(_.name.toLowerCase)
        table.create_*.filterNot(s => hColumns.contains(s.name.toLowerCase))
      } else table.create_*)
        .map(fs => createColumnDDLBuilder(fs, table))
    }
    override protected val primaryKeys: Iterable[PrimaryKey] = {
      if(table.isInstanceOf[InheritingTable]) {
        val hTable = table.asInstanceOf[InheritingTable].inherited
        val hPrimaryKeys = hTable.primaryKeys.map(pk => PrimaryKey(table.tableName + "_" + pk.name, pk.columns))
        hTable.create_*.find(_.options.contains(ColumnOption.PrimaryKey))
          .map(s => PrimaryKey(table.tableName + "_PK", IndexedSeq(Select(tableNode, s))))
          .map(Iterable(_) ++ hPrimaryKeys ++ table.primaryKeys)
          .getOrElse(hPrimaryKeys ++ table.primaryKeys)
      } else table.primaryKeys
    }

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

    override protected def createTable: String = {
      if(table.isInstanceOf[InheritingTable]) {
        val hTable = table.asInstanceOf[InheritingTable].inherited
        val hTableNode = hTable.toNode.asInstanceOf[TableExpansion].table.asInstanceOf[TableNode]
        s"${super.createTable} inherits (${quoteTableName(hTableNode)})"
      } else super.createTable
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
    }

    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlTypeName = "UUID"
      override def setValue(v: UUID, p: PreparedStatement, idx: Int) = p.setObject(idx, v, sqlType)
      override def getValue(r: ResultSet, idx: Int) = r.getObject(idx).asInstanceOf[UUID]
      override def updateValue(v: UUID, r: ResultSet, idx: Int) = r.updateObject(idx, v)
      override def valueToSQLLiteral(value: UUID) = "'" + value + "'"
      override def hasLiteralForm = true
    }
  }

  /*****************************************************************************************
   *                        additional feature support related
   *****************************************************************************************/
  /**
   * pg inherits support, for usage pls see [[com.typesafe.slick.testkit.tests.PgFeatureTests]]
   */
  trait InheritingTable { sub: Table[_] =>
    val inherited: Table[_]
  }

  /**
   * pg aggregate function support, usage:
   * {{{
   *  object AggregateLibrary {
   *    val StringAgg = new SqlFunction("string_agg")
   *  }
   *  case class StringAgg(delimiter: String) extends UnaryAggFuncPartsBasic[String, String](AggregateLibrary.StringAgg, List(LiteralNode(delimiter)))
   *  ...
   *  col1 ^: StringAgg(",").forDistinct().orderBy(col1 desc)
   * }}}
   */
  final case class AggFuncInputs(aggParams: Seq[Node], modifier: Option[String] = None, orderBy: Seq[(Node, Ordering)] = Nil) extends SimplyTypedNode {
    type Self = AggFuncInputs
    val nodeChildren = aggParams ++ orderBy.map(_._1)
    protected[this] def nodeRebuild(ch: IndexedSeq[Node]): Self = {
      val newAggParams = ch.slice(0, aggParams.length)
      val orderByOffset = aggParams.length
      val newOrderBy = ch.slice(orderByOffset, orderByOffset + orderBy.length)
      copy(aggParams = newAggParams,
        orderBy = (orderBy, newOrderBy).zipped.map { case ((_, o), n) => (n, o) })
    }
    protected def buildType = aggParams(0).nodeType
    override def toString = "AggFuncInputs"
  }

  ///
  trait AggFuncParts {
    def aggFunc: FunctionSymbol
    def params: Seq[Node]
    def modifier: Option[String]
    def ordered: Option[Ordered]
  }
  protected sealed class AggFuncPartsImpl(
    val aggFunc: FunctionSymbol,
    val params: Seq[Node] = Nil,
    val modifier: Option[String] = None,
    val ordered: Option[Ordered] = None
    ) extends AggFuncParts

  ///
  trait UnaryAggFunction[T,R] { parts: AggFuncParts =>
    def ^:[P1,PR](expr: Column[P1])(implicit tm: JdbcType[R], om: OptionMapperDSL.arg[T,P1]#to[R,PR]): Column[PR] = {
      val aggParams = expr.toNode +: params
      om.column(aggFunc, AggFuncInputs(aggParams, modifier, ordered.map(_.columns).getOrElse(Nil)))
    }
  }
  class UnaryAggFuncPartsBasic[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil) extends AggFuncPartsImpl(aggFunc, params) with UnaryAggFunction[T,R] {
    def forDistinct() = new UnaryAggFuncPartsWithModifier[T,R](aggFunc, params, Some("DISTINCT"))
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, None, Some(ordered)) with UnaryAggFunction[T,R]
  }
  class UnaryAggFuncPartsWithModifier[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil, modifier: Option[String] = None)
                  extends AggFuncPartsImpl(aggFunc, params, modifier) with UnaryAggFunction[T,R] {
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, modifier, Some(ordered)) with UnaryAggFunction[T,R]
  }

  ///
  trait BinaryAggFunction[T,R] { parts: AggFuncParts =>
    def ^:[P1,P2,PR](expr: (Column[P1], Column[P2]))(implicit tm: JdbcType[R], om: OptionMapperDSL.arg[T,P1]#arg[T,P2]#to[R,PR]): Column[PR] = {
      val aggParams = expr._1.toNode +: expr._2.toNode +: params
      om.column(aggFunc, AggFuncInputs(aggParams, modifier, ordered.map(_.columns).getOrElse(Nil)))
    }
  }
  class BinaryAggFuncPartsBasic[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil) extends AggFuncPartsImpl(aggFunc, params) with BinaryAggFunction[T,R] {
    def forDistinct() = new BinaryAggFuncPartsWithModifier[T,R](aggFunc, params, Some("DISTINCT"))
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, None, Some(ordered)) with BinaryAggFunction[T,R]
  }
  class BinaryAggFuncPartsWithModifier[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil, modifier: Option[String] = None)
                   extends AggFuncPartsImpl(aggFunc, params, modifier) with BinaryAggFunction[T,R] {
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, modifier, Some(ordered)) with BinaryAggFunction[T,R]
  }

  ///
  trait TernaryAggFunction[T,R] { parts: AggFuncParts =>
    def ^:[P1,P2,P3,PR](expr: (Column[P1], Column[P2], Column[P3]))(
      implicit tm: JdbcType[R], om: OptionMapperDSL.arg[T,P1]#arg[T,P2]#arg[T,P3]#to[R,PR]): Column[PR] = {
        val aggParams = expr._1.toNode +: expr._2.toNode +: expr._3.toNode +: params
        om.column(aggFunc, AggFuncInputs(aggParams, modifier, ordered.map(_.columns).getOrElse(Nil)))
      }
  }
  class TernaryAggFuncPartsBasic[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil) extends AggFuncPartsImpl(aggFunc, params) with TernaryAggFunction[T,R] {
    def forDistinct() = new TernaryAggFuncPartsWithModifier[T,R](aggFunc, params, Some("DISTINCT"))
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, None, Some(ordered)) with TernaryAggFunction[T,R]
  }
  class TernaryAggFuncPartsWithModifier[T,R](aggFunc: FunctionSymbol, params: Seq[Node] = Nil, modifier: Option[String] = None)
                    extends AggFuncPartsImpl(aggFunc, params, modifier) with TernaryAggFunction[T,R] {
    def orderBy(ordered: Ordered) = new AggFuncPartsImpl(aggFunc, params, modifier, Some(ordered)) with TernaryAggFunction[T,R]
  }
}

object PostgresDriver extends PostgresDriver
