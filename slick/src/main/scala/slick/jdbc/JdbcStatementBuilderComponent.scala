package slick.jdbc

import java.sql.{PreparedStatement, ResultSet}

import scala.collection.mutable
import scala.language.existentials

import slick.SlickException
import slick.ast.*
import slick.ast.TypeUtil.*
import slick.ast.Util.nodeToNodeOps
import slick.compiler.{CodeGen, CompilerState, QueryCompiler, RewriteBooleans}
import slick.lifted.*
import slick.relational.{CompiledMapping, RelationalCapabilities, RelationalProfile, ResultConverter}
import slick.sql.SqlProfile
import slick.util.*
import slick.util.QueryInterpolator.queryInterpolator

trait JdbcStatementBuilderComponent { self: JdbcProfile =>

  // Create the different builders -- these methods should be overridden by profiles as needed
  def createQueryBuilder(n: Node, state: CompilerState): QueryBuilder = new QueryBuilder(n, state)
  def createInsertBuilder(node: Insert): InsertBuilder = new InsertBuilder(node)
  def createUpsertBuilder(node: Insert): InsertBuilder = new UpsertBuilder(node)
  def createCheckInsertBuilder(node: Insert): InsertBuilder = new CheckInsertBuilder(node)
  def createUpdateInsertBuilder(node: Insert): InsertBuilder = new UpdateInsertBuilder(node)
  def createTableDDLBuilder(table: Table[?]): TableDDLBuilder = new TableDDLBuilder(table)
  def createColumnDDLBuilder(column: FieldSymbol, table: Table[?]): ColumnDDLBuilder = new ColumnDDLBuilder(column)
  def createSequenceDDLBuilder(seq: Sequence[?]): SequenceDDLBuilder = new SequenceDDLBuilder.BuiltInSupport(seq)

  class JdbcCompiledInsert(source: Node) {
    class Artifacts(val compiled: Node,
                    val converter: ResultConverter[ResultSet, PreparedStatement, ResultSet, Any],
                    val ibr: InsertBuilderResult) {
      def table: TableNode = ibr.table
      def sql: String = ibr.sql
      def fields: ConstArray[FieldSymbol] = ibr.fields
    }

    protected[this] def compile(compiler: QueryCompiler): Artifacts = {
      val compiled = compiler.run(source).tree
      val ResultSetMapping(_, CompiledStatement(_, ibr: InsertBuilderResult, _), CompiledMapping(conv, _)) =
        compiled: @unchecked
      new Artifacts(compiled, conv.asInstanceOf[ResultConverter[ResultSet, PreparedStatement, ResultSet, Any]], ibr)
    }

    /** The compiled artifacts for standard insert statements. */
    lazy val standardInsert = compile(insertCompiler)

    /** The compiled artifacts for forced insert statements. */
    lazy val forceInsert = compile(forceInsertCompiler)

    /** The compiled artifacts for upsert statements. */
    lazy val upsert = compile(upsertCompiler)

    /** The compiled artifacts for 'check insert' statements. */
    lazy val checkInsert = compile(checkInsertCompiler)

    /** The compiled artifacts for 'update insert' statements. */
    lazy val updateInsert = compile(updateInsertCompiler)

    /** Build a list of columns and a matching `ResultConverter` for retrieving keys of inserted rows. */
    def buildReturnColumns(node: Node): (ConstArray[String], ResultConverter[ResultSet, PreparedStatement, ResultSet, ?], Boolean) = {
      if(!capabilities.contains(JdbcCapabilities.returnInsertKey))
        throw new SlickException("This DBMS does not allow returning columns from INSERT statements")
      val ResultSetMapping(_, CompiledStatement(_, ibr: InsertBuilderResult, _), CompiledMapping(rconv, _)) =
        forceInsertCompiler.run(node).tree
      if(ibr.table.baseIdentity != standardInsert.table.baseIdentity)
        throw new SlickException("Returned key columns must be from same table as inserted columns ("+
          ibr.table.baseIdentity+" != "+standardInsert.table.baseIdentity+")")
      val returnOther = ibr.fields.length > 1 || !ibr.fields.head.options.contains(ColumnOption.AutoInc)
      if(!capabilities.contains(JdbcCapabilities.returnInsertOther) && returnOther)
        throw
          new SlickException(
            "This DBMS allows only a single column to be returned from an INSERT," +
              " and that column must be an AutoInc column."
          )
      (ibr.fields.map(_.name), rconv.asInstanceOf[ResultConverter[ResultSet, PreparedStatement, ResultSet, ?]], returnOther)
    }
  }

  abstract class StatementPart
  case object SelectPart extends StatementPart
  case object FromPart extends StatementPart
  case object WherePart extends StatementPart
  case object HavingPart extends StatementPart
  case object OtherPart extends StatementPart

  /** Create a SQL representation of a literal value. */
  def valueToSQLLiteral(v: Any, tpe: Type): String = {
    val JdbcType(ti, option) = tpe
    if(option) v.asInstanceOf[Option[Any]].fold("null")(ti.valueToSQLLiteral)
    else ti.valueToSQLLiteral(v)
  }

  // Immutable config options (to be overridden by subclasses)
  /** The table name for scalar selects (e.g. "select 42 from DUAL;"), or `None` for
    * scalar selects without a FROM clause ("select 42;"). */
  val scalarFrom: Option[String] = None

  /** Builder for SELECT and UPDATE statements. */
  class QueryBuilder(val tree: Node, val state: CompilerState) extends InterpolationContext { queryBuilder =>

    // Immutable config options (to be overridden by subclasses)
    protected val supportsTuples = true
    protected val supportsCast = true
    protected val concatOperator: Option[String] = None
    protected val hasPiFunction = true
    protected val hasRadDegConversion = true
    protected val parenthesizeNestedRHSJoin = false
    protected val pi = "3.1415926535897932384626433832795"
    protected val alwaysAliasSubqueries = true
    protected val supportsLiteralGroupBy = false
    protected val quotedJdbcFns: Option[Seq[Library.JdbcFunction]] = None // quote all by default

    // Mutable state accessible to subclasses
    protected val b = new SQLBuilder
    protected var currentPart: StatementPart = OtherPart
    val symbolName: SymbolNamer = new QuotingSymbolNamer(Some(state.symbolNamer))
    protected val joins = new mutable.HashMap[TermSymbol, Join]
    protected var currentUniqueFrom: Option[TermSymbol] = None

    private[this] var _skipParens = false

    final def skipParens: Boolean = _skipParens

    def quoteIdentifier(s: String): String = self.quoteIdentifier(s)
    implicit def interpolationContext: InterpolationContext = this

    final def withSkipParens[U](b: Boolean)(f: => U): U = {
      val old = _skipParens
      _skipParens = b
      try f finally _skipParens = old
    }

    def sqlBuilder = b

    final def buildSelect(): SQLBuilder.Result = {
      expr(tree, true)
      b.build
    }

    @inline protected final def building(p: StatementPart)(f: => Unit): Unit = {
      val oldPart = currentPart
      currentPart = p
      f
      currentPart = oldPart
    }

    protected def buildComprehension(c: Comprehension.Base): Unit = {
      val limit0 = c.fetch match {
        case Some(LiteralNode(0L)) => true
        case _ => false
      }
      scanJoins(ConstArray((c.sym, c.from)))
      val (from, on) = flattenJoins(c.sym, c.from)
      val oldUniqueFrom = currentUniqueFrom
      def containsSymbolInSubquery(s: TermSymbol) =
        c.children.iterator
          .drop(1)
          .flatMap(_.collect { case c: Comprehension.Base => c }.toSeq.flatMap(_.findNode(_ == Ref(s))))
          .nonEmpty
      currentUniqueFrom = from match {
        case Seq((s, _: TableNode)) if !containsSymbolInSubquery(s) => Some(s)
        case Seq((s, _)) if !alwaysAliasSubqueries && !containsSymbolInSubquery(s) => Some(s)
        case _ => None
      }
      buildSelectClause(c)
      buildFromClause(from)
      if(limit0) b"\nwhere 1=0"
      else buildWhereClause(and(c.where.toSeq ++ on))
      buildGroupByClause(c.groupBy)
      buildHavingClause(c.having)
      buildOrderByClause(c.orderBy)
      if(!limit0) buildFetchOffsetClause(c.fetch, c.offset)
      buildForUpdateClause(c.forUpdate)
      currentUniqueFrom = oldUniqueFrom
    }

    private[this] def and(ns: Seq[Node]): Option[Node] =
      if(ns.isEmpty) None else Some(ns.reduceLeft((p1, p2) => Library.And.typed[Boolean](p1, p2)))

    protected def flattenJoins(s: TermSymbol, n: Node): (Seq[(TermSymbol, Node)], Seq[Node]) = {
      def f(s: TermSymbol, n: Node): Option[(Seq[(TermSymbol, Node)], Seq[Node])] = n match {
        case Join(ls, rs, l, r, JoinType.Inner, on) =>
          for ((defs1, on1) <- f(ls, l); (defs2, on2) <- f(rs, r))
            yield
              (defs1 ++ defs2,
                on match {
                  case LiteralNode(true) => on1 ++ on2
                  case on                => on1 ++ on2 :+ on
                })
        case _: Join                               => None
        case n => Some((Seq((s, n)), Nil))
      }
      f(s, n).getOrElse((Seq((s, n)), Nil))
    }

    protected def buildSelectClause(c: Comprehension.Base) = building(SelectPart) {
      b"select "
      buildSelectModifiers(c)
      c.select match {
        case Pure(StructNode(ch), _) =>
          b.sep(ch, ", ") { case (sym, n) =>
            buildSelectPart(n)
            b" as `$sym"
          }
          if(ch.isEmpty) b"1"
        case Pure(ProductNode(ch), _) =>
          b.sep(ch, ", ")(buildSelectPart)
          if(ch.isEmpty) b"1"
        case Pure(n, _) => buildSelectPart(n)
      }
    }

    protected def buildSelectModifiers(c: Comprehension.Base): Unit = {
      c.distinct.foreach {
        case ProductNode(ch) if ch.isEmpty => b"distinct "
        case n => b"distinct on (!$n) "
      }
    }

    protected def scanJoins(from: ConstArray[(TermSymbol, Node)]): Unit = {
      for((sym, j: Join) <- from) {
        joins += sym -> j
        scanJoins(j.generators)
      }
    }

    protected def buildFromClause(from: Seq[(TermSymbol, Node)]) = building(FromPart) {
      from match {
        case Nil | Seq((_, Pure(ProductNode(ConstArray()), _))) => scalarFrom.foreach { s => b"\nfrom $s" }
        case from =>
          b"\nfrom "
          b.sep(from, ", ") { case (sym, n) =>
            buildFrom(n, if(currentUniqueFrom.contains(sym)) None else Some(sym), false)
          }
      }
    }

    protected def buildWhereClause(where: Option[Node]) =
      building(WherePart)(where.foreach(p => b"\nwhere !$p"))

    protected def buildGroupByClause(groupBy: Option[Node]) = building(OtherPart)(groupBy.foreach { n =>
      b"\ngroup by "
      n match {
        case ProductNode(es) => b.sep(es, ", ")(buildGroupByColumn)
        case e => buildGroupByColumn(e)
      }
    })

    protected def buildGroupByColumn(by: Node) = by match {
      // Some database systems assign special meaning to literal values in GROUP BY, so we replace
      // them by a constant non-literal expression unless it is known to be safe.
      case LiteralNode(_) if !supportsLiteralGroupBy => b"0+0"
      case e => b"!$e"
    }

    protected def buildHavingClause(having: Option[Node]) =
      building(HavingPart)(having.foreach(p => b"\nhaving !$p"))

    protected def buildOrderByClause(order: ConstArray[(Node, Ordering)]) = building(OtherPart) {
      if(!order.isEmpty) {
        b"\norder by "
        b.sep(order, ", "){ case (n, o) => buildOrdering(n, o) }
      }
    }

    protected def buildFetchOffsetClause(fetch: Option[Node], offset: Option[Node]) = building(OtherPart) {
      (fetch, offset) match {
        /* SQL:2008 syntax */
        case (Some(t), Some(d)) => b"\noffset $d row fetch next $t row only"
        case (Some(t), None) => b"\nfetch next $t row only"
        case (None, Some(d)) => b"\noffset $d row"
        case _ =>
      }
    }

    protected def buildForUpdateClause(forUpdate: Boolean) = building(OtherPart) {
      if(forUpdate) {
        b"\nfor update "
      }
    }

    protected def buildSelectPart(n: Node): Unit = n match {
      case c: Comprehension.Base =>
        b"\["
        buildComprehension(c)
        b"\]"
      case n =>
        expr(n, true)
    }

    protected final def buildFrom(n: Node, alias: Option[TermSymbol], skipParens: Boolean): Unit =
      withSkipParens(skipParens)(buildFrom(n, alias))

    protected def buildFrom(n: Node, alias: Option[TermSymbol]): Unit = building(FromPart) {
      def addAlias() = alias foreach { s => b += ' ' += symbolName(s) }
      n match {
        case t: TableNode =>
          b += quoteTableName(t)
          addAlias()
        case j: Join =>
          buildJoin(j)
        case n =>
          expr(n)
          addAlias()
      }
    }

    protected def buildJoin(j: Join): Unit = {
      buildFrom(j.left, Some(j.leftGen), false)
      val op = j.on match {
        case LiteralNode(true) if j.jt == JoinType.Inner => "cross"
        case _ => j.jt.sqlName
      }
      b"\n$op join "
      if(j.right.isInstanceOf[Join] && parenthesizeNestedRHSJoin) {
        b"\["
        buildFrom(j.right, Some(j.rightGen), false)
        b"\]"
      } else buildFrom(j.right, Some(j.rightGen), false)
      if(op != "cross") j.on match {
        case LiteralNode(true) => b"\non 1=1"
        case on => b"\non !$on"
      }
    }

    final def expr(n: Node, skipParens: Boolean): Unit =
      withSkipParens(skipParens)(expr(n))

    def expr(n: Node): Unit = n match {
      case p @ Path(path) =>
        val (base, rest) = path.foldRight[(Option[TermSymbol], List[TermSymbol])]((None, Nil)) {
          case (ElementSymbol(idx), (Some(b), Nil)) => (Some(joins(b).generators(idx-1)._1), Nil)
          case (s, (None, Nil)) => (Some(s), Nil)
          case (s, (b, r)) => (b, s :: r)
        }
        if(base != currentUniqueFrom) b += symbolName(base.get) += '.'
        rest match {
          case Nil => b += '*'
          case field :: Nil => b += symbolName(field)
          case _ => throw new SlickException("Cannot resolve "+p+" as field or view")
        }
      case (n @ LiteralNode(v)) :@ JdbcType(ti, option) =>
        if(n.volatileHint || !ti.hasLiteralForm) b +?= { (p, idx, _) =>
          if(option) ti.setOption(v.asInstanceOf[Option[Any]], p, idx)
          else ti.setValue(v, p, idx)
        } else b += valueToSQLLiteral(v, n.nodeType)
      case ProductNode(ch) =>
        b"\("
        b.sep(ch, ", ")(expr(_, false))
        b"\)"
      case n: Apply => n match {
        case Library.Not(Library.==(l, LiteralNode(null))) =>
          b"\($l is not null\)"
        case Library.==(l, LiteralNode(null)) =>
          b"\($l is null\)"
        case Library.==(left: ProductNode, right: ProductNode) =>
          b"\("
          if(supportsTuples) b"$left = $right"
          else {
            val cols = left.children.zip(right.children).force
            b.sep(cols, " and "){ case (l,r) => expr(l, false); b += "="; expr(r, false) }
          }
          b"\)"
        case RewriteBooleans.ToFakeBoolean(ch) =>
          expr(RewriteBooleans.rewriteFakeBoolean(ch), skipParens)
        case RewriteBooleans.ToRealBoolean(ch) =>
          expr(Library.==.typed[Boolean](ch, LiteralNode(true).infer()), skipParens)
        case Library.Exists(c: Comprehension.Base) =>
          /* If tuples are not supported, selecting multiple individual columns
           * in exists(select ...) is probably not supported, either, so we rewrite
           * such sub-queries to "select 1". */
          b"exists\[!${(if(supportsTuples) c else c.copy(select = Pure(LiteralNode(1))).infer()): Node}\]"
        case Library.Concat(l, r) if concatOperator.isDefined =>
          b"\($l${concatOperator.get}$r\)"
        case Library.User() if !capabilities.contains(RelationalCapabilities.functionUser) =>
          b += "''"
        case Library.Database() if !capabilities.contains(RelationalCapabilities.functionDatabase) =>
          b += "''"
        case Library.Pi() if !hasPiFunction => b += pi
        case Library.Degrees(ch) if !hasRadDegConversion =>
          b"(180.0/!${Library.Pi.typed(columnTypes.bigDecimalJdbcType)}*$ch)"
        case Library.Radians(ch) if!hasRadDegConversion =>
          b"(!${Library.Pi.typed(columnTypes.bigDecimalJdbcType)}/180.0*$ch)"
        case Library.Between(left, start, end) => b"$left between $start and $end"
        case Library.CountDistinct(e) => b"count(distinct $e)"
        case Library.CountAll(e) => b"count($e)"
        case Library.Like(l, r) => b"\($l like $r\)"
        case Library.Like(l, r, LiteralNode(esc: Char)) =>
          if(esc == '\'' || esc == '%' || esc == '_')
            throw new SlickException("Illegal escape character '"+esc+"' for LIKE expression")
          // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
          b"\($l like $r escape '$esc'\)"
        case Library.StartsWith(n, LiteralNode(s: String)) =>
          b"\($n like ${valueToSQLLiteral(likeEncode(s)+'%', ScalaBaseType.stringType)} escape '^'\)"
        case Library.EndsWith(n, LiteralNode(s: String)) =>
          b"\($n like ${valueToSQLLiteral("%"+likeEncode(s), ScalaBaseType.stringType)} escape '^'\)"
        case Library.Trim(n) =>
          expr(Library.LTrim.typed[String](Library.RTrim.typed[String](n)), skipParens)
        case Library.Substring(n, start, end) =>
          val startNode = QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())
          val lengthNode = QueryParameter.constOp[Int]("-")(_ - _)(end, start)
          b"\({fn substring($n, $startNode, $lengthNode)}\)"
        case Library.Substring(n, start) =>
          val startNode = QueryParameter.constOp[Int]("+")(_ + _)(start, LiteralNode(1).infer())
          b"\({fn substring($n, $startNode)}\)"
        case Library.IndexOf(n, str) => b"\({fn locate($str, $n)} - 1\)"
        case Library.Cast(ch*)       =>
          val tn =
            if(ch.length == 2) ch(1).asInstanceOf[LiteralNode].value.asInstanceOf[String]
            else jdbcTypeFor(n.nodeType).sqlTypeName(None)
          if(supportsCast) b"cast(${ch.head} as $tn)"
          else b"{fn convert(!${ch.head},$tn)}"
        case Library.SilentCast(ch)  => b"$ch"
        case Apply(sym: Library.SqlOperator, ch) =>
          b"\("
          if(ch.length == 1) {
            b"${sym.name} ${ch.head}"
          } else b.sep(ch, " " + sym.name + " ")(expr(_, false))
          b"\)"
        case Apply(sym: Library.JdbcFunction, ch) =>
          val quote = quotedJdbcFns.forall(_.contains(sym))
          if(quote) b"{fn "
          b"${sym.name}("
          b.sep(ch, ",")(expr(_, true))
          b")"
          if(quote) b"}"
        case Apply(sym: Library.SqlFunction, ch) =>
          b"${sym.name}("
          b.sep(ch, ",")(expr(_, true))
          b")"
        case n => throw new SlickException("Unexpected function call "+n+" -- SQL prefix: "+b.build.sql)
      }
      case c: IfThenElse =>
        b"(case"
        c.ifThenClauses.foreach { case (l, r) => b" when $l then $r" }
        c.elseClause match {
          case LiteralNode(null) =>
          case n => b" else $n"
        }
        b" end)"
      case OptionApply(ch) => expr(ch, skipParens)
      case QueryParameter(extractor, JdbcType(ti, option), _) =>
        b +?= { (p, idx, param) =>
          if(option) ti.setOption(extractor(param).asInstanceOf[Option[Any]], p, idx)
          else ti.setValue(extractor(param), p, idx)
        }
      case s: SimpleFunction =>
        if(s.scalar) b"{fn "
        b"${s.name}("
        b.sep(s.children, ",")(expr(_, true))
        b")"
        if(s.scalar) b += '}'
      case RowNumber(by) =>
        b"row_number() over(order by "
        if(by.isEmpty) b"(select 1)"
        else b.sep(by, ", "){ case (n, o) => buildOrdering(n, o) }
        b")"
      case c: Comprehension.Base =>
        b"\{"
        buildComprehension(c)
        b"\}"
      case Union(left, right, all) =>
        b"\{"
        b"\["
        buildFrom(left, None, true)
        b"\]"
        if(all) b"\nunion all " else b"\nunion "
        b"\["
        buildFrom(right, None, true)
        b"\]"
        b"\}"
      case SimpleLiteral(w) => b += w
      case s: SimpleExpression => s.toSQL(this)
      case s: SimpleBinaryOperator => b"\(${s.left} ${s.name} ${s.right}\)"
      case n => throw new SlickException("Unexpected node "+n+" -- SQL prefix: "+b.build.sql)
    }

    protected def buildOrdering(n: Node, o: Ordering): Unit = {
      expr(n, false)
      if(o.direction.desc) b" desc"
      if(o.nulls.first) b" nulls first"
      else if(o.nulls.last) b" nulls last"
    }

    def buildUpdate(): SQLBuilder.Result = {
      val (gen, from, where, select) = tree match {
        case Comprehension(sym, from: TableNode, Pure(select, _), where, None, _, None, None, None, None, false) =>
          select match {
            case f @ Select(Ref(struct), _) if struct == sym                                         =>
              (sym, from, where, ConstArray(f.field))
            case ProductNode(ch)
              if ch.forall { case Select(Ref(struct), _) if struct == sym => true; case _ => false } =>
              (sym, from, where, ch.map { case Select(Ref(_), field) => field })
            case _                                                                                   =>
              throw
                new SlickException(
                  "A query for an UPDATE statement must select table columns only -- Unsupported shape: " + select
                )
          }
        case o                                                                                                   =>
          throw
            new SlickException(
              "A query for an UPDATE statement must resolve to a comprehension with a single table --" +
                " Unsupported shape: " + o
            )
      }

      val qtn = quoteTableName(from)
      symbolName(gen) = qtn // Alias table to itself because UPDATE does not support aliases
      b"update $qtn set "
      b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
      if(where.isDefined) {
        b" where "
        expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
      }
      b.build
    }

    protected def buildDeleteFrom(tableName: String): Unit = {
      b"delete from $tableName"
    }

    def buildDelete(): SQLBuilder.Result = {
      def fail(msg: String) =
        throw new SlickException("Invalid query for DELETE statement: " + msg)
      val (gen, from, where) = tree match {
        case Comprehension(sym, from, Pure(_, _), where, _, _, None, distinct, fetch, offset, forUpdate) =>
          if(fetch.isDefined || offset.isDefined || distinct.isDefined || forUpdate)
            fail(".take, .drop .forUpdate and .distinct are not supported for deleting")
          from match {
            case from: TableNode => (sym, from, where)
            case from => fail("A single source table is required, found: "+from)
          }
        case o                                                                                           =>
          fail("Unsupported shape: "+o+" -- A single SQL comprehension is required")
      }
      val qtn = quoteTableName(from)
      symbolName(gen) = qtn // Alias table to itself because DELETE does not support aliases
      buildDeleteFrom(qtn)
      if(where.isDefined) {
        b" where "
        expr(where.reduceLeft((a, b) => Library.And.typed[Boolean](a, b)), true)
      }
      b.build
    }
  }

  /** Builder for INSERT statements. */
  class InsertBuilder(val ins: Insert) {
    protected val Insert(_, table: TableNode, ProductNode(rawColumns), allFields) = ins: @unchecked
    protected val syms: ConstArray[FieldSymbol] = rawColumns.map { case Select(_, fs: FieldSymbol) => fs }
    protected lazy val allNames = syms.map(fs => quoteIdentifier(fs.name))
    protected lazy val allVars = syms.iterator.map(_ => "?").mkString("(", ",", ")")
    protected lazy val tableName = quoteTableName(table)

    def buildInsert: InsertBuilderResult = {
      val start = buildInsertStart
      if (syms.isEmpty)
        new InsertBuilderResult(table, emptyInsert, syms) {
          override def buildMultiRowInsert(size: Int): String =
            if (allFields.isEmpty)
              throw new SlickException("Multi-row insert is not supported for 'default values' phrase.")
            else {
              val valuesExpr = (1 to size).map(_ => "" + "(default)").mkString(",")
              s"insert into $tableName (${quoteIdentifier(allFields.head.name)}) values $valuesExpr"
            }
        }
      else
        new InsertBuilderResult(table, s"$start values $allVars", syms) {
          override def buildInsert(compiledQuery: Node) = {
            val (_, sbr: SQLBuilder.Result) = CodeGen.findResult(compiledQuery): @unchecked
            SQLBuilder.Result(start + sbr.sql, sbr.setter)
          }

          override def buildMultiRowInsert(size: Int): String =
            s"$buildInsertStart values ${(1 to size).map(_ => allVars).mkString(",")}"
        }
    }

    def transformMapping(n: Node) = n

    protected def buildInsertStart: String = allNames.iterator.mkString(s"insert into $tableName (", ",", ") ")

    protected def emptyInsert: String =
      if(allFields.isEmpty) s"insert into $tableName default values"
      else s"insert into $tableName (${quoteIdentifier(allFields.head.name)}) values (default)"

    /** Reorder InsertColumn indices in a mapping Node in the order of the given
      * sequence of FieldSymbols (which may contain duplicates). */
    protected def reorderColumns(n: Node, order: IndexedSeq[FieldSymbol]): Node = {
      val newIndices = order.zipWithIndex.groupBy(_._1)
      lazy val reordering: ConstArray[IndexedSeq[Int]] = syms.map(fs => newIndices(fs).map(_._2 + 1))
      n.replace({ case InsertColumn(ConstArray(Select(ref, ElementSymbol(idx))), fs, tpe) =>
        val newPaths = reordering(idx-1).map(i => Select(ref, ElementSymbol(i)))
        InsertColumn(ConstArray.from(newPaths), fs, tpe) :@ tpe
      }, keepType = true)
    }
  }

  /** Builder for upsert statements, builds standard SQL MERGE statements by default. */
  class UpsertBuilder(ins: Insert) extends InsertBuilder(ins) {
    /* NOTE: pk defined by using method `primaryKey` and pk defined with `PrimaryKey` can only have one,
             here we let table ddl to help us ensure this. */
    private lazy val funcDefinedPKs = table.profileTable.asInstanceOf[Table[?]].primaryKeys
    protected lazy val (pkSyms, softSyms) = syms.toSeq.partition { sym =>
      sym.options.contains(ColumnOption.PrimaryKey) || funcDefinedPKs.exists(pk => pk.columns.collect {
        case Select(_, f: FieldSymbol) => f
      }.exists(_.name == sym.name)) }
    protected lazy val pkNames = pkSyms.map { fs => quoteIdentifier(fs.name) }
    protected lazy val softNames = softSyms.map { fs => quoteIdentifier(fs.name) }
    protected lazy val nonAutoIncSyms = syms.filter(s => !(s.options contains ColumnOption.AutoInc))
    protected lazy val nonAutoIncNames = nonAutoIncSyms.map(fs => quoteIdentifier(fs.name))

    override def buildInsert: InsertBuilderResult = {
      val start = buildMergeStart
      val end = buildMergeEnd
      val paramSel =
        "select " + allNames.map(n => "? as "+n).iterator.mkString(",") + scalarFrom.map(n => " from "+n).getOrElse("")
      // We'd need a way to alias the column names at the top level in order to support merges from a source Query
      new InsertBuilderResult(table, start + paramSel + end, syms)
    }

    protected def buildMergeStart: String = s"merge into $tableName t using ("

    protected def buildMergeEnd: String = {
      val updateCols = softNames.toList match {
        case Nil => ""
        case list => s"when matched then update set ${list.map(n => s"t.$n=s.$n").mkString(", ")}"
      }
      val insertCols = nonAutoIncNames /*.map(n => s"t.$n")*/ .mkString(", ")
      val insertVals = nonAutoIncNames.map(n => s"s.$n").mkString(", ")
      val cond = pkNames.map(n => s"t.$n=s.$n").mkString(" and ")
      s") s on ($cond) $updateCols  when not matched then insert ($insertCols) values ($insertVals)"
    }
  }

  /** Builder for SELECT statements that can be used to check for the existing of
    * primary keys supplied to an INSERT operation. Used by the insertOrUpdate emulation
    * on databases that don't support this in a single server-side statement. */
  class CheckInsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult =
      new InsertBuilderResult(
        table = table,
        sql = pkNames.map(n => s"$n=?").mkString(s"select 1 from $tableName where ", " and ", ""),
        fields = ConstArray.from(pkSyms)
      )
  }

  /** Builder for UPDATE statements used as part of an insertOrUpdate operation
    * on databases that don't support this in a single server-side statement. */
  class UpdateInsertBuilder(ins: Insert) extends UpsertBuilder(ins) {
    override def buildInsert: InsertBuilderResult =
      new InsertBuilderResult(
        table = table,
        sql =
          "update " +
            tableName +
            " set " +
            softNames.map(n => s"$n=?").mkString(",") +
            " where " +
            pkNames.map(n => s"$n=?").mkString(" and "),
        fields = ConstArray.from(softSyms ++ pkSyms)
      )

    override def transformMapping(n: Node) = reorderColumns(n, softSyms ++ pkSyms)
  }

  /** Builder for various DDL statements. */
  class TableDDLBuilder(val table: Table[?]) { self =>
    protected val tableNode = table.toNode.asInstanceOf[TableExpansion].table.asInstanceOf[TableNode]

    /** new instance on access to avoid sharing mutable state during createPhase1 or createIfNotExistsPhase */
    protected def columns: Iterable[ColumnDDLBuilder] = table.create_*.map(fs => createColumnDDLBuilder(fs, table))

    protected val indexes: Iterable[Index] = table.indexes
    protected val foreignKeys: Iterable[ForeignKey] = table.foreignKeys
    protected val primaryKeys: Iterable[PrimaryKey] = table.primaryKeys

    def buildDDL: DDL = {
      if(primaryKeys.size > 1)
        throw new SlickException("Table "+tableNode.tableName+" defines multiple primary keys ("
          + primaryKeys.map(_.name).mkString(", ") + ")")
      DDL(createPhase1, createIfNotExistsPhase, createPhase2, dropPhase1, dropIfExistsPhase, dropPhase2 , truncatePhase)
    }

    protected def createPhase1 =
      Iterable(createTable(false)) ++ primaryKeys.map(createPrimaryKey) ++ indexes.map(createIndex)
    protected def createIfNotExistsPhase =
      Iterable(createTable(true)) ++
        primaryKeys.map(createPrimaryKey) ++
        indexes.map(createIndex) ++
        foreignKeys.map(dropForeignKeyIfExists) ++
        foreignKeys.map(createForeignKey)
    protected def createPhase2 = foreignKeys.map(createForeignKey)
    protected def dropPhase1 = foreignKeys.map(dropForeignKey)
    protected def dropIfExistsPhase = Iterable(dropTable(true))
    protected def dropPhase2 = primaryKeys.map(dropPrimaryKey) ++ Iterable(dropTable(false))
    protected def truncatePhase = Iterable(truncateTable)

    protected def createTable(checkNotExists: Boolean): String = {
      val b =
        (new StringBuilder)
          .append("create table ")
          .append(if (checkNotExists) "if not exists " else "")
          .append(quoteTableName(tableNode))
          .append(" (")
      var first = true
      for(c <- columns) {
        if(first) first = false else b append ","
        c.appendColumn(b)
      }
      addTableOptions(b)
      b append ")"
      b.toString
    }

    protected def addTableOptions(b: StringBuilder): Unit = {}

    protected def dropTable(ifExists: Boolean): String =
      "drop table "+(if(ifExists) "if exists " else "")+quoteTableName(tableNode)

    protected def truncateTable: String = "truncate table "+ quoteTableName(tableNode)

    protected def createIndex(idx: Index): String = {
      val b = new StringBuilder append "create "
      if(idx.unique) b append "unique "
      b append "index " append quoteIdentifier(idx.name) append " on " append quoteTableName(tableNode) append " ("
      addIndexColumnList(idx.on, b, idx.table.tableName)
      b append ")"
      b.toString
    }

    protected def createForeignKey(fk: ForeignKey): String = {
      val sb = new StringBuilder append "alter table " append quoteTableName(tableNode) append " add "
      addForeignKey(fk, sb)
      sb.toString
    }

    protected def addForeignKey(fk: ForeignKey, sb: StringBuilder): Unit = {
      sb append "constraint " append quoteIdentifier(fk.name) append " foreign key("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, tableNode.tableName)
      sb append ") references " append quoteTableName(fk.targetTable) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      sb append ") on update " append fk.onUpdate.action
      sb append " on delete " append fk.onDelete.action
    }

    protected def createPrimaryKey(pk: PrimaryKey): String = {
      val sb = new StringBuilder append "alter table " append quoteTableName(tableNode) append " add "
      addPrimaryKey(pk, sb)
      sb.toString
    }

    protected def addPrimaryKey(pk: PrimaryKey, sb: StringBuilder): Unit = {
      sb append "constraint " append quoteIdentifier(pk.name) append " primary key("
      addPrimaryKeyColumnList(pk.columns, sb, tableNode.tableName)
      sb append ")"
    }

    protected def dropForeignKey(fk: ForeignKey): String =
      "alter table " + quoteTableName(tableNode) + " drop constraint " + quoteIdentifier(fk.name)

    protected def dropForeignKeyIfExists(fk: ForeignKey): String =
      "alter table " + quoteTableName(tableNode) + " drop constraint if exists " + quoteIdentifier(fk.name)

    protected def dropPrimaryKey(pk: PrimaryKey): String =
      "alter table " + quoteTableName(tableNode) + " drop constraint " + quoteIdentifier(pk.name)

    protected def addIndexColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "index")

    protected def addIndexColumnList(columns: IndexedSeq[Node], requiredTableName: String) = {
      val sb = new StringBuilder
      addColumnList(columns, sb, requiredTableName, "index")
      sb.toString
    }

    protected def addForeignKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addPrimaryKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addColumnList(columns: IndexedSeq[Node],
                                sb: StringBuilder,
                                requiredTableName: String,
                                typeInfo: String): Unit = {
      var first = true
      for(c <- columns) c match {
        case Select(t: TableNode, field: FieldSymbol) =>
          if(first) first = false
          else sb append ","
          sb append quoteIdentifier(field.name)
          if(requiredTableName != t.tableName)
            throw new SlickException("All columns in "+typeInfo+" must belong to table "+requiredTableName)
        case _ => throw new SlickException("Cannot use column "+c+" in "+typeInfo+" (only named columns are allowed)")
      }
    }
  }
  object TableDDLBuilder {
    trait UniqueIndexAsConstraint extends TableDDLBuilder {
      override protected def createIndex(idx: Index) =
        if (idx.unique)
          s"""ALTER TABLE ${quoteIdentifier(table.tableName)}
             |ADD CONSTRAINT ${quoteIdentifier(idx.name)}
             |UNIQUE(${addIndexColumnList(idx.on, idx.table.tableName)})""".stripMargin
        else
          super.createIndex(idx)
    }
  }

  /** Builder for column specifications in DDL statements. */
  class ColumnDDLBuilder(column: FieldSymbol) {
    protected val JdbcType(jdbcType, isOption) = column.tpe
    protected var sqlType: String = _
    protected var varying: Boolean = false
    protected var size: Option[Int] = None
    protected var customSqlType: Boolean = false
    protected var notNull = !isOption
    protected var autoIncrement = false
    protected var primaryKey = false
    protected var unique     = false
    protected var defaultLiteral: String = _
    init()

    protected def init(): Unit = {
      for(o <- column.options) handleColumnOption(o)
      if(sqlType ne null) {
        size.foreach(l => sqlType += s"($l)")
        customSqlType = true
      } else sqlType = jdbcType.sqlTypeName(Some(column))
    }

    protected def handleColumnOption(o: ColumnOption[?]): Unit = o match {
      case SqlProfile.ColumnOption.SqlType(s) => sqlType = s
      case RelationalProfile.ColumnOption.Length(s,v) =>
        size = Some(s)
        varying = v
      case SqlProfile.ColumnOption.NotNull => notNull = true
      case SqlProfile.ColumnOption.Nullable => notNull = false
      case ColumnOption.AutoInc => autoIncrement = true
      case ColumnOption.PrimaryKey => primaryKey = true
      case ColumnOption.Unique      => unique = true
      case RelationalProfile.ColumnOption.Default(v) => defaultLiteral = valueToSQLLiteral(v, column.tpe)
    }

    def appendType(sb: StringBuilder): Unit = sb append sqlType

    def appendColumn(sb: StringBuilder): Unit = {
      sb append quoteIdentifier(column.name) append ' '
      appendType(sb)
      appendOptions(sb)
    }

    protected def appendOptions(sb: StringBuilder): Unit = {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " GENERATED BY DEFAULT AS IDENTITY(START WITH 1)"
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if( unique )   sb append " UNIQUE"
    }
  }

  /** Builder for DDL statements for sequences. */
  trait SequenceDDLBuilder {
    def buildDDL: DDL
  }
  object SequenceDDLBuilder {
    class BuiltInSupport(seq: Sequence[?]) extends SequenceDDLBuilder {
      protected def asClause = ""

      protected def incrementClause(increment: Any): String = s"increment $increment"

      protected def actualStart(start: Option[Any]): Option[Any] = start

      protected def startClause(start: Any): String = s" start $start"

      protected def cycleClause = "cycle"

      def buildDDL: DDL = {
        val createSql =
          "create sequence " +
            quoteIdentifier(seq.name) +
            asClause +
            seq._increment.fold("")(v => s" ${incrementClause(v)}") +
            seq._minValue.fold("")(v => s" minvalue $v") +
            seq._maxValue.fold("")(v => s" maxvalue $v") +
            actualStart(seq._start).fold("")(startClause) +
            (if (seq._cycle) s" $cycleClause" else "")

        val dropSql =
          s"drop sequence ${quoteIdentifier(seq.name)}"

        DDL(createSql, dropSql)
      }
    }
    object BuiltInSupport {
      class OverrideActualStart[T](seq: Sequence[T]) extends BuiltInSupport(seq) {
        override protected def actualStart(start: Option[Any]): Option[Any] =
          start.orElse {
            import seq.integral.*
            Some(if (seq._increment.exists(_ < zero)) -1 else 1)
          }
      }
      trait IncrementBy extends BuiltInSupport {
        override protected def incrementClause(increment: Any): String = s"increment by $increment"
      }
      trait StartWith extends BuiltInSupport {
        override protected def startClause(start: Any): String = s" start with $start"
      }
    }
  }
}

class InsertBuilderResult(val table: TableNode, val sql: String, val fields: ConstArray[FieldSymbol]) {
  def buildInsert(compiledQuery: Node): SQLBuilder.Result =
    throw new SlickException("Building Query-based inserts from this InsertBuilderResult is not supported")

  def buildMultiRowInsert(size: Int): String =
    throw new SlickException("Building multi-row inserts from this InsertBuilderResult is not supported")
}
