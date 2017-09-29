package slick.jdbc

import java.util.UUID

import scala.concurrent.ExecutionContext

import java.sql.{Array => _, _}

import slick.SlickException
import slick.ast._
import slick.compiler.{CompilerState, Phase}
import slick.dbio._
import slick.jdbc.meta.{MColumn, MTable}
import slick.lifted._
import slick.model.ForeignKeyAction
import slick.relational.{RelationalCapabilities, ResultConverter, RelationalProfile}
import slick.basic.Capability
import slick.util.ConstArray
import slick.util.MacroSupport.macroSupportInterpolation

/** Slick profile for Oracle.
  *
  * This profile implements [[slick.jdbc.JdbcProfile]]
  * ''without'' the following capabilities:
  *
  * <ul>
  *   <li>[[slick.relational.RelationalCapabilities.foreignKeyActions]]:
  *     Foreign key actions ''Cascade'', ''SetNull'' and ''NoAction'' are
  *     directly supported for ''onDelete''. ''Restrict'' and ''SetDefault''
  *     are ignored (i.e. equals to ''NoAction''). No ''onUpdate'' actions are
  *     supported but specifying ''Cascade'' adds the option INITIALLY DEFERRED
  *     to the foreign key constraint, thus allowing you to perform the
  *     cascading update manually before committing the current transaction.
  *     Other ''onUpdate'' actions are ignored.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertOrUpdate]]:
  *     InsertOrUpdate operations are emulated on the client side if generated
  *     keys should be returned. Otherwise the operation is performmed
  *     natively on the server side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
  *     Oracle doesn't have booleans, so Slick maps to CHAR instead and
  *     that's how it appears in model and generated code.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.distinguishesIntTypes]]:
  *     Oracle doesn't distinguish integer types and Slick uses NUMBER,
  *     which is always mapped back to BigDecimal in model and generated code.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     Oracle does not have a BYTE type.</li>
  * </ul>
  *
  * Note: The Oracle JDBC driver has problems with quoted identifiers. Columns
  * which are returned from inserts must not require quoted names (in
  * particular, they must not contain lower-case characters or be equal to a
  * reserved word), otherwise a bug in the Oracle JDBC driver triggers an
  * ORA-00904 error. The same issue arises when trying to update such a column
  * in a mutable result set.
  *
  * Updating Blob values in updatable result sets is not supported.
  */
trait OracleProfile extends JdbcProfile {

  override protected def computeCapabilities: Set[Capability] = (super.computeCapabilities
    - RelationalCapabilities.foreignKeyActions
    - JdbcCapabilities.insertOrUpdate
    - JdbcCapabilities.booleanMetaData
    - JdbcCapabilities.distinguishesIntTypes
    - JdbcCapabilities.supportsByte
  )

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  trait ColumnOptions extends super.ColumnOptions {
    def AutoIncSequenceName(name: String) = OracleProfile.ColumnOption.AutoIncSequenceName(name)
    def AutoIncTriggerName(name: String) = OracleProfile.ColumnOption.AutoIncTriggerName(name)
  }

  override val columnOptions: ColumnOptions = new ColumnOptions {}

  class ModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext) extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder = new ColumnBuilder(tableBuilder, meta) {
      override def tpe = meta.sqlType match {
        case 101 => "Double"
        case _ => super.tpe
      }
      override def rawDefault = super.rawDefault.map(_.stripSuffix(" ")).map{
        case "null" => "NULL"
        case v => v
      }
    }
  }

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean)(implicit ec: ExecutionContext): JdbcModelBuilder =
    new ModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables(implicit ec: ExecutionContext): DBIO[Seq[MTable]] = {
    for {
      user <- SimpleJdbcAction(_.session.metaData.getUserName)
      mtables <- MTable.getTables(None, Some(user), None, Some(Seq("TABLE")))
    } yield mtables
  }

  override protected def computeQueryCompiler =
    (super.computeQueryCompiler.addAfter(Phase.removeTakeDrop, Phase.expandSums)
      .replace(Phase.resolveZipJoinsRownumStyle)
      - Phase.fixRowNumberOrdering
      + Phase.rewriteBooleans + new RemoveSubqueryOrdering)

  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[_]): TableDDLBuilder = new TableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[_]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[_]): SequenceDDLBuilder[_] = new SequenceDDLBuilder(seq)
  override val columnTypes = new JdbcTypes

  val blobBufferSize = 4096

  override def defaultSqlTypeName(tmd: JdbcType[_], sym: Option[FieldSymbol]): String = tmd.sqlType match {
    case java.sql.Types.VARCHAR =>
      val size = sym.flatMap(_.findColumnOption[RelationalProfile.ColumnOption.Length])
      size.fold("VARCHAR2(254)")(l => if(l.varying) s"VARCHAR2(${l.length})" else s"CHAR(${l.length})")
    case java.sql.Types.INTEGER => "NUMBER(10)"
    case java.sql.Types.BIGINT => "NUMBER(19)"
    case java.sql.Types.SMALLINT => "NUMBER(5)"
    case java.sql.Types.TINYINT => "NUMBER(3)"
    case java.sql.Types.DOUBLE => "BINARY_DOUBLE"
    case java.sql.Types.FLOAT => "BINARY_FLOAT"
    case _ => super.defaultSqlTypeName(tmd, sym)
  }

  override val scalarFrom = Some("sys.dual")

  class QueryBuilder(tree: Node, state: CompilerState) extends super.QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator = Some("||")
    override protected val hasPiFunction = false
    /* Oracle officially supports {fn degrees} and {fn radians} but
     * Statement.execute throws a NullPointerException when you try to use
     * these functions. */
    override protected val hasRadDegConversion = false

    override def expr(c: Node, skipParens: Boolean = false): Unit = c match {
      case RowNumber(_) => b"rownum"
      case Library.NextValue(SequenceNode(name)) => b += quoteIdentifier(name) += ".nextval"
      case Library.CurrentValue(SequenceNode(name)) => b += quoteIdentifier(name) += ".currval"
      case Library.Database() => b += "ORA_DATABASE_NAME"
      case Library.Repeat(s, n) => b"RPAD($s, LENGTH($s)*$n, $s)"
      case Library.==(left: ProductNode, right: ProductNode) => //TODO
        b"\("
        val cols = (left.children zip right.children).force
        b.sep(cols, " and "){ case (l,r) => expr(Library.==.typed[Boolean](l, r)) }
        b"\)"
      case Library.==(l, r) if (l.nodeType != UnassignedType) && jdbcTypeFor(l.nodeType).sqlType == Types.BLOB =>
        b"\(dbms_lob.compare($l, $r) = 0\)"
      case _ => super.expr(c, skipParens)
    }
  }

  class TableDDLBuilder(table: Table[_]) extends super.TableDDLBuilder(table) {
    override val createPhase1 = super.createPhase1 ++ createAutoIncSequences
    override val dropPhase2 = dropAutoIncSequences ++ super.dropPhase2

    def createAutoIncSequences = columns.flatMap { case cb: ColumnDDLBuilder =>
      cb.createSequenceAndTrigger(table)
    }

    def dropAutoIncSequences = columns.flatMap { case cb: ColumnDDLBuilder =>
      cb.dropTriggerAndSequence(table)
    }

    override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder) {
      sb append "constraint " append quoteIdentifier(fk.name) append " foreign key("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
      sb append ") references " append quoteIdentifier(fk.targetTable.tableName) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      sb append ')'
      fk.onDelete match {
        case ForeignKeyAction.Cascade => sb append " on delete cascade"
        case ForeignKeyAction.SetNull => sb append " on delete set null"
        case _ => // do nothing
      }
      if(fk.onUpdate == ForeignKeyAction.Cascade) sb append " initially deferred"
    }

    override protected def createIndex(idx: Index) = {
      if(idx.unique) {
        /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
         * index) because Oracle does not allow a FOREIGN KEY CONSTRAINT to
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

  class ColumnDDLBuilder(column: FieldSymbol) extends super.ColumnDDLBuilder(column) {
    var sequenceName: String = _
    var triggerName: String = _

    override def appendColumn(sb: StringBuilder) {
      val qname = quoteIdentifier(column.name)
      sb append qname append ' '
      appendType(sb)
      appendOptions(sb)
      if(jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
        sb append " check (" append qname append " in (0, 1))"
      }
    }

    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if( unique ) sb append " UNIQUE"
    }

    override protected def handleColumnOption(o: ColumnOption[_]): Unit = o match {
      case OracleProfile.ColumnOption.AutoIncSequenceName(s) => sequenceName = s
      case OracleProfile.ColumnOption.AutoIncTriggerName(s) => triggerName = s
      case _ => super.handleColumnOption(o)
    }

    def createSequenceAndTrigger(t: Table[_]): Iterable[String] = if(!autoIncrement) Nil else {
      val tab = quoteIdentifier(t.tableName)
      val seq = quoteIdentifier(if(sequenceName eq null) t.tableName+"__"+column.name+"_seq" else sequenceName)
      val trg = quoteIdentifier(if(triggerName eq null) t.tableName+"__"+column.name+"_trg" else triggerName)
      val col = quoteIdentifier(column.name)
      Seq(
        s"create sequence $seq start with 1 increment by 1",
        s"create or replace trigger $trg before insert on $tab referencing new as new for each row"+
          s" when (new.$col is null) begin select $seq.nextval into :new.$col from sys.dual; end;"
      )
    }

    def dropTriggerAndSequence(t: Table[_]): Iterable[String] = if(!autoIncrement) Nil else {
      val seq = quoteIdentifier(if(sequenceName eq null) t.tableName+"__"+column.name+"_seq" else sequenceName)
      val trg = quoteIdentifier(if(triggerName eq null) t.tableName+"__"+column.name+"_trg" else triggerName)
      Seq(
        s"drop trigger $trg",
        s"drop sequence $seq"
      )
    }
  }

  class SequenceDDLBuilder[T](seq: Sequence[T]) extends super.SequenceDDLBuilder(seq) {
    override def buildDDL: DDL = {
      val b = new StringBuilder append "create sequence " append quoteIdentifier(seq.name)
      seq._increment.foreach { b append " increment by " append _ }
      seq._minValue.foreach { b append " minvalue " append _ }
      seq._maxValue.foreach { b append " maxvalue " append _ }
      seq._start.foreach { b append " start with " append _ }
      if(seq._cycle) b append " cycle nocache"
      DDL(b.toString, "drop sequence " + quoteIdentifier(seq.name))
    }
  }

  class JdbcTypes extends super.JdbcTypes {
    override val booleanJdbcType = new BooleanJdbcType
    override val blobJdbcType = new BlobJdbcType
    override val byteArrayJdbcType = new ByteArrayJdbcType
    override val stringJdbcType = new StringJdbcType
    override val timeJdbcType = new TimeJdbcType
    override val uuidJdbcType = new UUIDJdbcType

    /* Oracle does not have a proper BOOLEAN type. The suggested workaround is
     * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
    class BooleanJdbcType extends super.BooleanJdbcType {
      override def sqlType = java.sql.Types.CHAR
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
    }

    class BlobJdbcType extends super.BlobJdbcType {
      override def setValue(v: Blob, p: PreparedStatement, idx: Int) = {
        val ob = p.getConnection.createBlob()
        var added = false
        try {
          val out = ob.setBinaryStream(0L)
          try {
            val in = v.getBinaryStream
            try {
              val buf = new Array[Byte](blobBufferSize)
              var cont = true
              while(cont) {
                val len = in.read(buf)
                if(len < 0) cont = false
                else out.write(buf, 0, len)
              }
              p.setBlob(idx, ob)
              added = true
            } finally in.close()
          } finally out.close()
        } finally if(!added) ob.free()
      }
      override def updateValue(v: Blob, r: ResultSet, idx: Int) =
        throw new SlickException("OracleProfile does not support updating Blob values")
    }

    class ByteArrayJdbcType extends super.ByteArrayJdbcType {
      override def updateValue(v: Array[Byte], r: ResultSet, idx: Int) =
        throw new SlickException("OracleProfile does not support updating Blob values")
    }

    class StringJdbcType extends super.StringJdbcType {
      /* Oracle treats an empty String as NULL, so we need to convert it back
       * when reading a null String value from a ResultSet. There is no way
       * to distinguish that from a proper NULL. */
      override def getValue(r: ResultSet, idx: Int) = {
        val v = super.getValue(r, idx)
        if(v eq null) "" else v
      }
    }

    class TimeJdbcType extends super.TimeJdbcType {
      override def sqlType = java.sql.Types.TIMESTAMP
      override def setValue(v: Time, p: PreparedStatement, idx: Int) = p.setTimestamp(idx, new Timestamp(v.getTime))
      override def getValue(r: ResultSet, idx: Int) = {
        val v = r.getTimestamp(idx)
        if(v eq null) null else new Time(v.getTime)
      }
      override def updateValue(v: Time, r: ResultSet, idx: Int) = r.updateTimestamp(idx, new Timestamp(v.getTime))
      override def valueToSQLLiteral(value: Time) = "{ts '"+(new Timestamp(value.getTime).toString)+"'}"
    }

    class UUIDJdbcType extends super.UUIDJdbcType {
      override def sqlTypeName(sym: Option[FieldSymbol]) = "RAW(32)"
      override def valueToSQLLiteral(value: UUID) = {
        val hex = value.toString.replace("-", "").toUpperCase
        s"hextoraw('$hex')"
      }
      override def hasLiteralForm = true
    }
  }

  /* Oracle throws an exception if you try to refer to a :new column in a
   * trigger statement in a PreparedStatement. Since we need to create
   * these statements for the AutoInc emulation, we execute all DDL
   * statements with a non-prepared Statement. */
  override def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    new SchemaActionExtensionMethodsImpl(schema)
  class SchemaActionExtensionMethodsImpl(schema: SchemaDescription) extends super.SchemaActionExtensionMethodsImpl(schema) {
    override def create: ProfileAction[Unit, NoStream, Effect.Schema] = new SimpleJdbcProfileAction[Unit]("schema.create", schema.createStatements.toVector) {
      def run(ctx: Backend#Context, sql: Vector[String]): Unit =
        for(s <- sql) ctx.session.withStatement()(_.execute(s))
    }
    override def drop: ProfileAction[Unit, NoStream, Effect.Schema] = new SimpleJdbcProfileAction[Unit]("schema.drop", schema.dropStatements.toVector) {
      def run(ctx: Backend#Context, sql: Vector[String]): Unit =
        for(s <- sql) ctx.session.withStatement()(_.execute(s))
    }
  }

  override def createOptionResultConverter[T](ti: JdbcType[T], idx: Int): ResultConverter[JdbcResultConverterDomain, Option[T]] =
    if(ti.scalaType == ScalaBaseType.stringType)
      (new OptionResultConverter[String](ti.asInstanceOf[JdbcType[String]], idx) {
        override def read(pr: ResultSet) = {
          val v = ti.getValue(pr, idx)
          if((v eq null) || v.length == 0) None else Some(v)
        }
      }).asInstanceOf[ResultConverter[JdbcResultConverterDomain, Option[T]]]
    else super.createOptionResultConverter(ti, idx)

  // Does not work to get around the ORA-00904 issue when returning columns
  // with lower-case names
  /*trait OracleInsertInvoker[U, RU] { this: AbstractKeysInsertInvoker[U, RU] =>
    override protected def prepared[T](sql: String)(f: PreparedStatement => T)(implicit session: Session) = {
      val columns = MColumn.getColumns(new MQName(None, None, insertResult.table), null).mapResult(_.column).to[Vector]
      val keyIndexes = keyColumns.map { n =>
        val idx = columns.indexOf(n)
        if(idx == -1) throw new SlickException("Column \""+n+"\" not found in table \""+insertResult.table+"\"")
        idx + 1
      }
      session.withPreparedInsertStatement(sql, keyIndexes)(f)
    }
  }*/

  /** Remove ORDER BY from comprehensions that are used as arguments to a
    * scalar function. */
  class RemoveSubqueryOrdering extends Phase {
    val name = "removeSubqueryOrdering"

    def apply(state: CompilerState) =
      state.map { n => ClientSideOp.mapServerSide(n)(n => rewrite(n, false)) }

    def rewrite(n: Node, inScalar: Boolean): Node = n match {
      case n: Comprehension if inScalar && n.orderBy.nonEmpty =>
        val n2 = n.copy(orderBy = ConstArray.empty) :@ n.nodeType
        n2.mapChildren(ch => rewrite(ch, false), keepType = true)
      case Apply(_, _) if !n.nodeType.structural.isInstanceOf[CollectionType] =>
        n.mapChildren(ch => rewrite(ch, true), keepType = true)
      case n =>
        n.mapChildren(ch => rewrite(ch, false), keepType = true)
    }
  }
}

object OracleProfile extends OracleProfile {
  /** Extra column options for OracleProfile */
  object ColumnOption {
    /** Name of the sequence which is generated for an AutoInc column. */
    case class AutoIncSequenceName(name: String) extends ColumnOption[Nothing]

    /** Name of the trigger which is generated for an AutoInc column. */
    case class AutoIncTriggerName(name: String) extends ColumnOption[Nothing]
  }
}
