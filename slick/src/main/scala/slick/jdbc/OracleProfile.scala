package slick.jdbc

import java.sql.{Array as _, *}
import java.time.*
import java.time.format.DateTimeFormatter
import java.util.UUID

import slick.SlickException
import slick.ast.*
import slick.basic.Capability
import slick.compiler.{CompilerState, Phase, RewriteBooleans}
import slick.dbio.*
import slick.jdbc.meta.{MColumn, MTable}
import slick.lifted.*
import slick.model.ForeignKeyAction
import slick.relational.{RelationalCapabilities, RelationalProfile, ResultConverter}
import slick.sql.SqlProfile
import slick.util.ConstArray
import slick.util.QueryInterpolator.queryInterpolator

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
  *     keys should be returned. Otherwise the operation is performed
  *     natively on the server side.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.booleanMetaData]]:
  *     Oracle doesn't have booleans, so Slick maps to CHAR instead and
  *     that's how it appears in model and generated code.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.distinguishesIntTypes]]:
  *     Oracle doesn't distinguish integer types and Slick uses NUMBER,
  *     which is always mapped back to BigDecimal in model and generated code.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.supportsByte]]:
  *     Oracle does not have a BYTE type.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.returnMultipleInsertKey]]:
  *      Oracle returns the last generated key only.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.insertMultipleRowsWithSingleStatement]]:
  *      Oracle doesn't support this feature directly.
  *      There are several alternative ways, but the library doesn't support them, so far.</li>
  *   <li>[[slick.jdbc.JdbcCapabilities.forShare]]:
  *     Oracle does not support SELECT ... FOR SHARE.</li>
  * </ul>
  *
  * Note: The Oracle JDBC driver has problems with quoted identifiers. Columns
  * which are updated in a mutable result set must not require quoted names (in
  * particular, they must not contain lower-case characters or be equal to a
  * reserved word), otherwise a bug in the Oracle JDBC driver triggers an
  * ORA-00904 error.
  *
  * Updating Blob values in updatable result sets is not supported.
  */
trait OracleProfile extends JdbcProfile {
  override protected def computeCapabilities: Set[Capability] =
    super.computeCapabilities -
      RelationalCapabilities.foreignKeyActions -
      JdbcCapabilities.insertOrUpdate -
      JdbcCapabilities.booleanMetaData -
      JdbcCapabilities.distinguishesIntTypes -
      JdbcCapabilities.supportsByte -
      JdbcCapabilities.returnMultipleInsertKey -
      JdbcCapabilities.insertMultipleRowsWithSingleStatement -
      JdbcCapabilities.forShare

  override protected lazy val useServerSideUpsert = true
  override protected lazy val useServerSideUpsertReturning = false

  trait OracleColumnOptions extends SqlColumnOptions {
    def AutoIncSequenceName(name: String) = OracleProfile.ColumnOption.AutoIncSequenceName(name)
    def AutoIncTriggerName(name: String) = OracleProfile.ColumnOption.AutoIncTriggerName(name)
  }

  override val columnOptions: OracleColumnOptions = new OracleColumnOptions {}

  class OracleModelBuilder(mTables: Seq[MTable], ignoreInvalidDefaults: Boolean)
    extends JdbcModelBuilder(mTables, ignoreInvalidDefaults) {
    override def createColumnBuilder(tableBuilder: TableBuilder, meta: MColumn): ColumnBuilder =
      new OracleColumnBuilder(tableBuilder, meta)
    class OracleColumnBuilder(tableBuilder: TableBuilder, meta: MColumn) extends ColumnBuilder(tableBuilder, meta) {
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

  override def createModelBuilder(tables: Seq[MTable], ignoreInvalidDefaults: Boolean): JdbcModelBuilder =
    new OracleModelBuilder(tables, ignoreInvalidDefaults)

  override def defaultTables: DBIO[Seq[MTable]] = {
    for {
      user <- SimpleJdbcAction(_.session.metaData.getUserName)
      mTables <- MTable.getTables(None, Some(user), Some("%"), Some(Seq("TABLE")))
    } yield mTables
  }

  override protected def computeQueryCompiler =
    super.computeQueryCompiler
      .addAfter(Phase.removeTakeDrop, Phase.expandSums)
      .replace(Phase.resolveZipJoinsRownumStyle) -
      Phase.fixRowNumberOrdering +
      Phase.rewriteBooleans +
      new RemoveSubqueryOrdering

  override def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new OracleQueryBuilder(n, state)
  override def createTableDDLBuilder(table: Table[?]): TableDDLBuilder = new OracleTableDDLBuilder(table)
  override def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): OracleColumnDDLBuilder =
    new OracleColumnDDLBuilder(column)
  override def createSequenceDDLBuilder(seq: Sequence[?]): SequenceDDLBuilder = new OracleSequenceDDLBuilder(seq)
  override val columnTypes: OracleJdbcTypes = new OracleJdbcTypes

  val blobBufferSize = 4096

  override def defaultSqlTypeName(tmd: JdbcType[?], sym: Option[FieldSymbol]): String = tmd.sqlType match {
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

  override val scalarFrom: Some[String] = Some("sys.dual")

  class OracleQueryBuilder(tree: Node, state: CompilerState) extends QueryBuilder(tree, state) {
    override protected val supportsTuples = false
    override protected val concatOperator: Some[String] = Some("||")
    override protected val hasPiFunction = false
    /* Oracle officially supports {fn degrees} and {fn radians} but
     * Statement.execute throws a NullPointerException when you try to use
     * these functions. */
    override protected val hasRadDegConversion = false

    override def expr(c: Node): Unit = c match {
      case RowNumber(_) => b"rownum"
      case Library.NextValue(SequenceNode(name)) => b += quoteIdentifier(name) += ".nextval"
      case Library.CurrentValue(SequenceNode(name)) => b += quoteIdentifier(name) += ".currval"
      case Library.Database() => b += "ORA_DATABASE_NAME"
      case Library.Repeat(s, n) => b"RPAD($s, LENGTH($s)*$n, $s)"
      case Library.==(left: ProductNode, right: ProductNode) => //TODO
        b"\("
        val cols = (left.children zip right.children).force
        b.sep(cols, " and "){ case (l,r) => expr(Library.==.typed[Boolean](l, r), false) }
        b"\)"
      case Library.==(l, r) if (l.nodeType != UnassignedType) && jdbcTypeFor(l.nodeType).sqlType == Types.BLOB =>
        b"\(dbms_lob.compare($l, $r) = 0\)"
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.SilentCast, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case RewriteBooleans.ToFakeBoolean(a @ Apply(Library.IfNull, _)) =>
        expr(RewriteBooleans.rewriteFakeBooleanWithEquals(a))
      case c@Comprehension(_, _, _, Some(n @ Apply(Library.IfNull, _)), _, _, _, _, _, _, _) =>
        super.expr(c.copy(where = Some(RewriteBooleans.rewriteFakeBooleanEqOne(n))))
      case u: Union => super.expr(castMismatchedBooleans(u))
      case _ => super.expr(c)
    }

    /* Boolean columns are CHAR(1), but Boolean literals, parameters and expressions are numbers. The
     * columns of the two sides of a union must have the same type, so where one side has a Boolean
     * column and the other a number, cast the column to a number. Positions where both sides are
     * columns, or both are numbers, are left unchanged. */
    protected def castMismatchedBooleans(u: Union): Union = {
      val positions = unionBooleanSql(resultBooleanSql(u.left).map(_._2), resultBooleanSql(u.right).map(_._2))
        .map(_ == Some(MixedBooleanSql))

      Union(castBooleanColumns(u.left, positions), castBooleanColumns(u.right, positions), u.all)
    }

    /** How the Boolean value at each position of a union is represented in SQL, given the
      * representations `l` and `r` of its sides. A position where the sides disagree is
      * `MixedBooleanSql`; `castMismatchedBooleans` casts it to a number. */
    private def unionBooleanSql(l: Seq[Option[BooleanSql]], r: Seq[Option[BooleanSql]]): Seq[Option[BooleanSql]] =
      if (l.length != r.length) l.map(_ => Some(MixedBooleanSql))
      else l.zip(r).map {
        case (None, s) => s
        case (s, None) => s
        case (Some(ls), Some(rs)) => if (ls == rs && ls != MixedBooleanSql) Some(ls) else Some(MixedBooleanSql)
      }

    /* The results of `resultBooleanSql`, computed once per node of the statement. A union or
     * subquery is classified from its sides or source, and the SQL of each nested union classifies
     * its sides again, so without the cache the work would grow exponentially with nesting. */
    private val resultBooleanSqlCache = new java.util.IdentityHashMap[Node, Seq[(Option[TermSymbol], Option[BooleanSql])]]

    /** The Boolean SQL representation of each result column of a union side or subquery, with the
      * column's field symbol if it has one. Non-Boolean columns and NULLs have `None`.
      * `private[slick]` so that a test can check how often it is called. */
    private[slick] def resultBooleanSql(n: Node): Seq[(Option[TermSymbol], Option[BooleanSql])] = {
      val cached = resultBooleanSqlCache.get(n)

      if (cached != null) cached
      else {
        val result = n match {
          case c: Comprehension.Base =>
            selectElements(c.select).map { case (s, e) => (s, booleanSql(e, c)) }
          case Union(left, right, _) =>
            val l = resultBooleanSql(left)

            // A union subquery's columns have the field symbols of its left side
            l.map(_._1).zip(unionBooleanSql(l.map(_._2), resultBooleanSql(right).map(_._2)).map {
              case Some(MixedBooleanSql) => Some(NumericBooleanSql) // castMismatchedBooleans casts it
              case s => s
            })
          case _ => Nil
        }

        resultBooleanSqlCache.put(n, result)
        result
      }
    }

    private def selectElements(select: Node): Seq[(Option[TermSymbol], Node)] = select match {
      case Pure(StructNode(defs), _) => defs.toSeq.map { case (s, n) => (Some(s), n) }
      case Pure(ProductNode(ch), _) => ch.toSeq.map(n => (None, n))
      case Pure(v, _) => Seq((None, v))
      case _ => Nil
    }

    /** How a select element `n` of comprehension `c` represents a Boolean in SQL. Values of other
      * `JdbcType[Boolean]`s than the profile's are written and stored by that type, so they are never
      * cast: one stored as a number is `NumericBooleanSql`, and any other is `None`. */
    private def booleanSql(n: Node, c: Comprehension.Base): Option[BooleanSql] =
      if (!isOracleBoolean(n.nodeType)) {
        if (isNumericOtherBoolean(n.nodeType)) Some(NumericBooleanSql) else None
      }
      else n match {
        /* An inline NULL takes the type of the other side, so it needs no cast. A bound NULL
         * (`volatileHint`, rendered as `?`) is bound as a number by `OracleBooleanJdbcType.setNull`,
         * so it is a number like any other bound value. */
        case l @ LiteralNode(None | null) if !l.volatileHint => None
        case _: LiteralNode | _: QueryParameter | RewriteBooleans.ToFakeBoolean(_) => Some(NumericBooleanSql)
        case Library.Cast(_, LiteralNode(BooleanNumberType)) => Some(NumericBooleanSql)
        case OptionApply(ch) => booleanSql(ch, c)
        case Select(in, field) =>
          pathSource(in, c) match {
            // A column with an `O.SqlType` may not be CHAR(1), e.g. NUMBER(1)
            case Some(_: TableNode) if hasSqlType(field) => Some(MixedBooleanSql)
            case Some(_: TableNode) => Some(CharBooleanSql)
            /* A subquery column that is NULL on every side stays `None`: Oracle accepts it in a union
             * with a number (UnionTest.testUnionOfNullBooleanSubquery) */
            case Some(sub @ (_: Comprehension.Base | _: Union)) =>
              resultBooleanSql(sub).collectFirst { case (Some(`field`), sql) => sql }.getOrElse(Some(MixedBooleanSql))
            case _ => Some(MixedBooleanSql)
          }
        case _ => Some(MixedBooleanSql)
      }

    /** The table, subquery or join in the FROM clause of comprehension `c` that a path refers to.
      * Paths refer to the generators of `c` and `Join`s in it, and to the sides of a join by
      * `ElementSymbol(1)` and `ElementSymbol(2)`. */
    private def pathSource(path: Node, c: Comprehension.Base): Option[Node] = path match {
      case slick.ast.Ref(s) => generatorSource(s, c.sym, c.from)
      case Select(in, ElementSymbol(idx)) =>
        pathSource(in, c).flatMap {
          case j: Join if idx == 1 => Some(j.left)
          case j: Join if idx == 2 => Some(j.right)
          case _ => None
        }
      case _ => None
    }

    /** The source that generator symbol `s` ranges over in a FROM clause */
    private def generatorSource(s: TermSymbol, gen: TermSymbol, from: Node): Option[Node] = from match {
      case _ if gen == s => Some(from)
      case j: Join => generatorSource(s, j.leftGen, j.left).orElse(generatorSource(s, j.rightGen, j.right))
      case _ => None
    }

    private def hasSqlType(field: TermSymbol): Boolean = field match {
      case f: FieldSymbol => f.existsColumnOption[SqlProfile.ColumnOption.SqlType]
      case _ => false
    }

    private def isNumericOtherBoolean(t: Type): Boolean =
      Phase.rewriteBooleans.isBooleanLike(t) && NumericSqlTypes.contains(jdbcTypeFor(t).sqlType)

    private def isOracleBoolean(t: Type): Boolean =
      Phase.rewriteBooleans.isBooleanLike(t) && jdbcTypeFor(t).isInstanceOf[OracleJdbcTypes#OracleBooleanJdbcType]

    /** Cast the Boolean values at the given positions of a union side to numbers, unless they
      * already are numbers. */
    private def castBooleanColumns(n: Node, positions: Seq[Boolean]): Node = n match {
      case c: Comprehension.Base if positions.contains(true) =>
        def cast(e: Node, i: Int) =
          if (positions.lift(i).contains(true) && booleanSql(e, c).exists(_ != NumericBooleanSql))
            Library.Cast.typed(e.nodeType, e, LiteralNode(BooleanNumberType))
          else e
        val select = c.select match {
          case Pure(StructNode(defs), ts) =>
            Pure(StructNode(defs.zipWithIndex.map { case ((s, e), i) => (s, cast(e, i)) }), ts)
          case Pure(ProductNode(ch), ts) => Pure(ProductNode(ch.zipWithIndex.map { case (e, i) => cast(e, i) }), ts)
          case Pure(v, ts) => Pure(cast(v, 0), ts)
          case s => s
        }

        c.copy(select = select) :@ c.nodeType
      case Union(left, right, all) if positions.contains(true) =>
        Union(castBooleanColumns(left, positions), castBooleanColumns(right, positions), all)
      case _ => n
    }
  }

  /** The `java.sql.Types` of numbers */
  private val NumericSqlTypes = {
    import java.sql.Types.*

    Set(BIT, TINYINT, SMALLINT, INTEGER, BIGINT, NUMERIC, DECIMAL, REAL, FLOAT, DOUBLE)
  }

  /** The SQL type to which unions cast Boolean columns */
  private val BooleanNumberType = "NUMBER(1)"

  /** How a Boolean value is represented in Oracle SQL */
  private[slick] sealed trait BooleanSql
  /** A Boolean column: CHAR(1) */
  private[slick] case object CharBooleanSql extends BooleanSql
  /** A Boolean literal, parameter or expression: a number */
  private[slick] case object NumericBooleanSql extends BooleanSql
  /** Different or unknown representations, which are cast to numbers in unions */
  private[slick] case object MixedBooleanSql extends BooleanSql

  class OracleTableDDLBuilder(table: Table[?])
    extends TableDDLBuilder(table)
      with TableDDLBuilder.UniqueIndexAsConstraint {
    override val createPhase1 = super.createPhase1 ++ createAutoIncSequences
    override val dropPhase2 = dropAutoIncSequences ++ super.dropPhase2

    override def createIfNotExistsPhase =
      Iterable(
        s"""
           |BEGIN
           |
           |${
          (createPhase1 ++ createPhase2)
            .map(s => "execute immediate '" + s.replaceAll("'", "''") + " ';")
            .mkString("\n")
        }
           |EXCEPTION
           |    WHEN OTHERS THEN
           |      IF SQLCODE = -955 THEN
           |        NULL; -- suppresses ORA-00955 exception
           |      ELSE
           |         RAISE;
           |      END IF;
           |END; """.stripMargin
      )

    override def dropIfExistsPhase = {
      //http://stackoverflow.com/questions/1799128/oracle-if-table-exists
      Iterable(
        """
BEGIN
""" + dropPhase2.map { s =>
          "execute immediate '" + s.replaceAll("'", """\\'""") + " ';"
        }.mkString("\n") +
          """
EXCEPTION
   WHEN OTHERS THEN
      IF SQLCODE = -942 THEN
        NULL; -- suppresses ORA-00942 exception
      ELSE
         RAISE;
      END IF;
END;
""")
    }

    def createAutoIncSequences = columns.flatMap { case cb: OracleColumnDDLBuilder =>
      cb.createSequenceAndTrigger(table)
    }

    def dropAutoIncSequences = columns.flatMap { case cb: OracleColumnDDLBuilder =>
      cb.dropTriggerAndSequence(table)
    }

    override protected def addForeignKey(fk: ForeignKey, sb: StringBuilder): Unit = {
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
      if (fk.onUpdate == ForeignKeyAction.Cascade) sb append " initially deferred"
    }
  }

  class OracleColumnDDLBuilder(column: FieldSymbol) extends ColumnDDLBuilder(column) {
    var sequenceName: String = _
    var triggerName: String = _

    override def appendColumn(sb: StringBuilder): Unit = {
      val qname = quoteIdentifier(column.name)
      sb append qname append ' '
      appendType(sb)
      appendOptions(sb)
      if(jdbcType.isInstanceOf[JdbcTypes#BooleanJdbcType]) {
        sb append " check (" append qname append " in (0, 1))"
      }
    }

    override protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if( unique ) sb append " UNIQUE"
    }

    override protected def handleColumnOption(o: ColumnOption[?]): Unit = o match {
      case OracleProfile.ColumnOption.AutoIncSequenceName(s) => sequenceName = s
      case OracleProfile.ColumnOption.AutoIncTriggerName(s) => triggerName = s
      case _ => super.handleColumnOption(o)
    }

    def createSequenceAndTrigger(t: Table[?]): Iterable[String] = if(!autoIncrement) Nil else {
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

    def dropTriggerAndSequence(t: Table[?]): Iterable[String] = if(!autoIncrement) Nil else {
      val seq = quoteIdentifier(if(sequenceName eq null) t.tableName+"__"+column.name+"_seq" else sequenceName)
      val trg = quoteIdentifier(if(triggerName eq null) t.tableName+"__"+column.name+"_trg" else triggerName)
      Seq(
        s"drop trigger $trg",
        s"drop sequence $seq"
      )
    }
  }

  class OracleSequenceDDLBuilder[T](seq: Sequence[T])
    extends SequenceDDLBuilder.BuiltInSupport(seq)
      with SequenceDDLBuilder.BuiltInSupport.IncrementBy
      with SequenceDDLBuilder.BuiltInSupport.StartWith {
    override protected def cycleClause = "cycle nocache"
  }

  class OracleJdbcTypes extends JdbcTypes {
    override val booleanJdbcType: OracleBooleanJdbcType = new OracleBooleanJdbcType
    override val blobJdbcType: OracleBlobJdbcType = new OracleBlobJdbcType
    override val byteArrayJdbcType: OracleByteArrayJdbcType = new OracleByteArrayJdbcType
    override val stringJdbcType: OracleStringJdbcType = new OracleStringJdbcType
    override val timeJdbcType: OracleTimeJdbcType = new OracleTimeJdbcType
    override val uuidJdbcType: OracleUUIDJdbcType = new OracleUUIDJdbcType
    override val localDateType: OracleLocalDateJdbcType = new OracleLocalDateJdbcType
    override val localDateTimeType: OracleLocalDateTimeJdbcType = new OracleLocalDateTimeJdbcType
    override val instantType: OracleInstantJdbcType = new OracleInstantJdbcType
    override val offsetTimeType: OracleOffsetTimeJdbcType = new OracleOffsetTimeJdbcType

    /* Oracle does not have a proper BOOLEAN type. The suggested workaround is
     * a constrained CHAR with constants 1 and 0 for TRUE and FALSE. */
    class OracleBooleanJdbcType extends BooleanJdbcType {
      override def sqlType = java.sql.Types.CHAR
      override def sqlTypeName(sym: Option[FieldSymbol]) = "CHAR(1)"
      override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
      /* The driver binds values set with setBoolean as numbers. Bind NULL as a number too, so that a
       * parameter has the same type whether or not it is None, e.g. in a union with a literal. */
      override def setNull(p: PreparedStatement, idx: Int): Unit = p.setNull(idx, java.sql.Types.NUMERIC)
    }

    class OracleBlobJdbcType extends BlobJdbcType {
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
      override def updateValue(v: Blob, r: ResultSet, idx: Int): Nothing =
        throw new SlickException("OracleProfile does not support updating Blob values")
    }

    class OracleByteArrayJdbcType extends ByteArrayJdbcType {
      override def updateValue(v: Array[Byte], r: ResultSet, idx: Int): Nothing =
        throw new SlickException("OracleProfile does not support updating Blob values")
    }

    class OracleStringJdbcType extends StringJdbcType {
      /* Oracle treats an empty String as NULL, so we need to convert it back
       * when reading a null String value from a ResultSet. There is no way
       * to distinguish that from a proper NULL. */
      override def getValue(r: ResultSet, idx: Int) = {
        val v = super.getValue(r, idx)
        if(v eq null) "" else v
      }
    }

    class OracleTimeJdbcType extends TimeJdbcType {
      override def sqlType = java.sql.Types.TIMESTAMP
      override def setValue(v: Time, p: PreparedStatement, idx: Int) = p.setTimestamp(idx, new Timestamp(v.getTime))
      override def getValue(r: ResultSet, idx: Int) = {
        val v = r.getTimestamp(idx)
        if(v eq null) null else new Time(v.getTime)
      }
      override def updateValue(v: Time, r: ResultSet, idx: Int) = r.updateTimestamp(idx, new Timestamp(v.getTime))
      override def valueToSQLLiteral(value: Time) = "{ts '"+ new Timestamp(value.getTime).toString +"'}"
    }

    class OracleUUIDJdbcType extends UUIDJdbcType {
      override def sqlType = java.sql.Types.VARBINARY
      override def sqlTypeName(sym: Option[FieldSymbol]) = "RAW(32)"
      override def valueToSQLLiteral(value: UUID) = {
        val hex = value.toString.replace("-", "").toUpperCase
        s"hextoraw('$hex')"
      }
      override def hasLiteralForm = true
    }

    class OracleLocalDateJdbcType extends LocalDateJdbcType {
      override def hasLiteralForm: Boolean = true
      override def valueToSQLLiteral(value: LocalDate) : String = {
        s"TO_DATE('${value.toString}', 'SYYYY-MM-DD')"
      }
      override def getValue(r: ResultSet, idx: Int): LocalDate = {
        r.getString(idx) match {
          case null => null
          case dateStr => LocalDate.parse(dateStr.substring(0, 10))
        }
      }
    }

    // LocalDateTime and Instant are the 2 types which have no TZ component
    // So, store them at UTC timestamps, otherwise the JDBC layer might attempt to map them
    // and with DST changes, there are some times which will be unrepresentable during the switchover
    class OracleLocalDateTimeJdbcType extends LocalDateTimeJdbcType {
      override def valueToSQLLiteral(value: LocalDateTime) = {
        s"TO_TIMESTAMP('${Timestamp.valueOf(value)}', 'YYYY-MM-DD HH24:MI:SS.FF9')"
      }
    }

    class OracleInstantJdbcType extends InstantJdbcType {
      private[this] val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSSSS x")
      private[this] def serializeTime(v: Instant) : String = formatter.format(instantToUTC(v))
      private[this] def instantToUTC(v: Instant): OffsetDateTime = v.atOffset(ZoneOffset.UTC)

      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(9)"
      override def getValue(r: ResultSet, idx: Int): Instant = {
        val timestamp = r.getTimestamp(idx)
        if (r.wasNull)
          null
        else
          timestamp.toInstant
      }
      override def hasLiteralForm: Boolean = true
      override def valueToSQLLiteral(value: Instant) = {
        s"TO_TIMESTAMP_TZ('${serializeTime(value)}', 'YYYY-MM-DD HH24:MI:SS.FF9 TZH')"
      }
    }

    // No Oracle time type without date component. Add LocalDate.ofEpochDay(0), but ignore it.
    class OracleOffsetTimeJdbcType extends OffsetTimeJdbcType {
      private[this] val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSSSSS Z")
      private[this] def serializeTime(v : OffsetTime) : String = formatter.format(v.atDate(LocalDate.ofEpochDay(0)))
      override def sqlTypeName(sym: Option[FieldSymbol]) = "TIMESTAMP(6) WITH TIME ZONE"
      override def setValue(v: OffsetTime, p: PreparedStatement, idx: Int) = {
        p.setObject(idx, TimestamptzConverter.offsetTimeToTimestamptz(v), -101)
      }
      override def updateValue(v: OffsetTime, r: ResultSet, idx: Int) = {
        r.updateObject(idx, TimestamptzConverter.offsetTimeToTimestamptz(v), -101)
      }
      override def getValue(r: ResultSet, idx: Int): OffsetTime = {
        TimestamptzConverter.timestamptzToOffsetTime(r.getObject(idx))
      }
      override def hasLiteralForm: Boolean = true
      override def valueToSQLLiteral(value: OffsetTime) = {
        s"TO_TIMESTAMP_TZ('${serializeTime(value)}', 'YYYY-MM-DD HH24:MI:SS.FF9 TZH:TZM')"
      }
    }
  }

  /* Oracle throws an exception if you try to refer to a :new column in a
   * trigger statement in a PreparedStatement. Since we need to create
   * these statements for the AutoInc emulation, we execute all DDL
   * statements with a non-prepared Statement. */
  override def createSchemaActionExtensionMethods(schema: SchemaDescription): SchemaActionExtensionMethods =
    new OracleSchemaActionExtensionMethodsImpl(schema)
  class OracleSchemaActionExtensionMethodsImpl(schema: SchemaDescription)
    extends JdbcSchemaActionExtensionMethodsImpl(schema) {

    override def create: ProfileAction[Unit, NoStream, Effect.Schema] =
      new SimpleJdbcProfileAction[Unit]("schema.create", schema.createStatements.toVector) {
        def run(ctx: JdbcBackend#JdbcActionContext, sql: Vector[String]): Unit =
          for (s <- sql) ctx.session.withStatement()(_.execute(s))
      }
    override def drop: ProfileAction[Unit, NoStream, Effect.Schema] =
      new SimpleJdbcProfileAction[Unit]("schema.drop", schema.dropStatements.toVector) {
        def run(ctx: JdbcBackend#JdbcActionContext, sql: Vector[String]): Unit =
          for (s <- sql) ctx.session.withStatement()(_.execute(s))
      }
  }

  override def createOptionResultConverter[T](ti: JdbcType[T],
                                              idx: Int): ResultConverter[ResultSet, PreparedStatement, ResultSet, Option[T]] =
    ti.scalaType match {
      case ScalaBaseType.stringType =>
        new OptionResultConverter[String](ti.asInstanceOf[JdbcType[String]], idx) {
          override def read(pr: ResultSet) = {
            val v = jdbcType.getValue(pr, index)
            if ((v eq null) || v.isEmpty) None else Some(v)
          }
        }.asInstanceOf[ResultConverter[ResultSet, PreparedStatement, ResultSet, Option[T]]]
      case _                        =>
        super.createOptionResultConverter(ti, idx)
    }

  override def createInsertActionExtensionMethods[T](compiled: CompiledInsert): InsertActionExtensionMethods[T] =
    new CountingInsertActionComposerImpl[T](compiled)

  override def createReturningInsertActionComposer[U, QR, RU](compiled: JdbcCompiledInsert,
                                                              keys: Node,
                                                              mux: (U, QR) => RU
                                                             ): ReturningInsertActionComposer[U, RU] =
    new ReturningInsertActionComposerImpl[U, QR, RU](compiled, keys, mux)

  // The driver puts the returned column names into its RETURNING clause unquoted
  override protected def returnedColumnName(name: String): String = quoteIdentifier(name)

  /** Remove ORDER BY from comprehensions that are used as arguments to a
    * scalar function. */
  class RemoveSubqueryOrdering extends Phase {
    val name = "removeSubqueryOrdering"

    def apply(state: CompilerState) =
      state.map { n => ClientSideOp.mapServerSide(n)(n => rewrite(n, inScalar = false)) }

    def rewrite(n: Node, inScalar: Boolean): Node = n match {
      case n: Comprehension.Base if inScalar && n.orderBy.nonEmpty =>
        val n2 = n.copy(orderBy = ConstArray.empty) :@ n.nodeType
        n2.mapChildren(ch => rewrite(ch, inScalar = false), keepType = true)
      case Apply(_, _) if !n.nodeType.structural.isInstanceOf[CollectionType] =>
        n.mapChildren(ch => rewrite(ch, inScalar = true), keepType = true)
      case n =>
        n.mapChildren(ch => rewrite(ch, inScalar = false), keepType = true)
    }
  }
}

object OracleProfile extends OracleProfile with JdbcActionComponent.OneRowPerStatementOnly {
  /** Extra column options for OracleProfile */
  object ColumnOption {
    /** Name of the sequence which is generated for an AutoInc column. */
    case class AutoIncSequenceName(name: String) extends ColumnOption[Nothing]

    /** Name of the trigger which is generated for an AutoInc column. */
    case class AutoIncTriggerName(name: String) extends ColumnOption[Nothing]
  }
}


/**
  * Converts between `TIMESTAMPTZ` and java.time times and back.
  * Oracle jar not on path at compile time (but must be a run time)
  * Use reflection to get access to `TIMESTAMPTZ` class
  */
object TimestamptzConverter {
  private val timestampTZClass = Class.forName("oracle.sql.TIMESTAMPTZ")
  private val timestampTZCtor = timestampTZClass.getConstructor(classOf[Array[Byte]])
  private val timestampTZToBytes = timestampTZClass.getMethod("toBytes")
  private val zoneIdClass = Class.forName("oracle.sql.ZONEIDMAP")
  private val zoneIdGetRegion = zoneIdClass.getMethod("getRegion", classOf[Int])

  private val RegionIdBit = Integer.parseInt("10000000",2)

  // Byte 0: Century, offset is 100 (value - 100 is century)
  // Byte 1: Decade, offset is 100 (value - 100 is decade)
  // Byte 2: Month UTC
  // Byte 3: Day UTC
  // Byte 4: Hour UTC, offset is 1 (value-1 is UTC hour)
  // Byte 5: Minute UTC, offset is 1 (value-1 is UTC Minute)
  // Byte 6: Second, offset is 1 (value-1 is seconds)
  // Byte 7: nanoseconds (most significant bit)
  // Byte 8: nanoseconds
  // Byte 9: nanoseconds
  // Byte 10: nanoseconds (least significant bit)
  // Byte 11: Hour UTC-offset of Timezone, offset is 20 (value-20 is UTC-hour offset)
  // Byte 12: Minute UTC-offset of Timezone, offset is 60 (value-60 is UTC-minute offset)

  def offsetTimeToTimestamptz(attribute: OffsetTime ): Any = {
    val bytes = newTIMESTAMPTZBuffer()
    val utc = attribute.atDate(LocalDate.ofEpochDay(0)).atZoneSameInstant(java.time.ZoneOffset.UTC)
    writeDateTime(bytes, utc)
    val offset = attribute.getOffset
    writeZoneOffset(bytes, offset)

    timestampTZCtor.newInstance(bytes)
  }

  def timestamptzToOffsetTime(dbData: Object) = {
    if (dbData == null)
      null
    else {
      val bytes = timestampTZToBytes.invoke(dbData).asInstanceOf[Array[Byte]]
      val utc = extractUtc(bytes)
      if (isFixedOffset(bytes)) {
        val offset = extractOffset(bytes)
        utc.withOffsetSameInstant(offset).toOffsetTime
      } else {
        val zoneId = extractZoneId(bytes)
        utc.atZoneSameInstant(zoneId).toOffsetDateTime.toOffsetTime
      }
    }
  }

  private def extractUtc(bytes: Array[Byte]) = {
    val year = ((bytes(0).toInt - 100) * 100) + (bytes(1).toInt - 100)
    val month = bytes(2)
    val dayOfMonth = bytes(3)
    val hour = bytes(4) - 1
    val minute = bytes(5) - 1
    val second = bytes(6) - 1
    val nanoOfSecond = ((bytes(7) & 0xFF) << 24) |
      ((bytes(8) & 0xFF) << 16) |
      ((bytes(9) & 0xFF) << 8) |
      bytes(10) & 0xFF
    OffsetDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond, java.time.ZoneOffset.UTC)
  }

  private def isFixedOffset(bytes: Array[Byte]) = (bytes(11) & RegionIdBit) == 0

  private def newTIMESTAMPTZBuffer() = new Array[Byte](13)

  private def writeDateTime(bytes: Array[Byte], utc: ZonedDateTime ): Unit = {
    val year = utc.getYear
    bytes(0) = (year / 100 + 100).toByte
    bytes(1) = (year % 100 + 100).toByte

    bytes(2) = utc.getMonthValue.toByte
    bytes(3) = utc.getDayOfMonth.toByte
    bytes(4) = (utc.getHour + 1).toByte
    bytes(5) = (utc.getMinute + 1).toByte
    bytes(6) = (utc.getSecond + 1).toByte

    val nano = utc.getNano
    val ba = BigInt(nano).toByteArray
    ba.zipWithIndex.foreach{ case (b,i) => bytes(11-ba.length + i) = b }
  }

  private val OFFSET_HOUR = 20
  private val OFFSET_MINUTE = 60
  private def extractOffset(bytes: Array[Byte]) =
    ZoneOffset.ofHoursMinutes(bytes(11) - OFFSET_HOUR, bytes(12) - OFFSET_MINUTE)
  private def writeZoneOffset(bytes: Array[Byte], offset: ZoneOffset ): Unit = {
    val totalMinutes = offset.getTotalSeconds / 60
    bytes(11) = ((totalMinutes / 60) + OFFSET_HOUR).toByte
    bytes(12) = ((totalMinutes % 60) + OFFSET_MINUTE).toByte
  }

  private val highBits = Integer.parseInt("1111111", 2)
  private val lowBits = Integer.parseInt("11111100", 2)
  private def extractZoneId(bytes: Array[Byte]) = {
    // high order bits
    val regionCode: Integer = ((bytes(11) & highBits) << 6) + ((bytes(12) & lowBits) >> 2)
    val regionName = zoneIdGetRegion.invoke(null, regionCode).asInstanceOf[String]
    ZoneId.of(regionName)
  }
  val msb = Integer.parseInt("1111111000000", 2)
  val lsb = Integer.parseInt("111111", 2)
}
