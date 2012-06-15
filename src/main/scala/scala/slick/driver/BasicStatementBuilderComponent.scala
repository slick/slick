package scala.slick.driver

import scala.language.existentials
import scala.slick.SLICKException
import scala.slick.ast._
import scala.slick.util._
import scala.slick.ql.{Join => _, _}
import scala.slick.ql.ColumnOps._
import scala.collection.mutable.HashMap

trait BasicStatementBuilderComponent { driver: BasicDriver =>

  abstract class StatementPart
  case object SelectPart extends StatementPart
  case object FromPart extends StatementPart
  case object WherePart extends StatementPart
  case object OtherPart extends StatementPart

  /** Builder for SELECT and UPDATE statements. */
  class QueryBuilder(val ast: Node, val linearizer: ValueLinearizer[_]) {

    // Immutable config options (to be overridden by subclasses)
    protected val scalarFrom: Option[String] = None
    protected val supportsTuples = true
    protected val supportsCast = true
    protected val concatOperator: Option[String] = None
    protected val useIntForBoolean = false

    // Mutable state accessible to subclasses
    protected val b = new SQLBuilder
    protected var currentPart: StatementPart = OtherPart
    protected val symbolName = new SymbolNamer
    protected val joins = new HashMap[Symbol, Join]

    def sqlBuilder = b

    final def buildSelect(): QueryBuilderResult = {
      buildComprehension(toComprehension(ast, true))
      QueryBuilderResult(b.build, linearizer)
    }

    protected final def newSym = AnonSymbol.named(symbolName.create)

    @inline protected final def building(p: StatementPart)(f: => Unit): Unit = {
      val oldPart = currentPart
      currentPart = p
      f
      currentPart = oldPart
    }

    protected def toComprehension(n: Node, liftExpression: Boolean = false): Comprehension = n match {
      case Comprehension(Seq(), Seq(), Seq(), Some(Pure(ProductNode(Seq(CountAll(q))))), None, None) =>
        Comprehension(from = Seq(newSym -> q), select = Some(Pure(CountStar)))
      case c : Comprehension => c
      case p: Pure =>
        Comprehension(select = Some(p))
      case t: TableNode =>
        Comprehension(from = Seq(t.nodeTableSymbol -> t))
      case u: Union =>
        Comprehension(from = Seq(newSym -> u))
      case n =>
        if(liftExpression) toComprehension(Pure(n))
        else throw new SLICKException("Unexpected node "+n+" -- SQL prefix: "+b.build.sql)
    }

    protected def buildComprehension(c: Comprehension): Unit = {
      scanJoins(c.from)
      buildSelectClause(c)
      buildFromClause(c.from)
      buildWhereClause(c.where)
      buildOrderByClause(c.orderBy)
      buildFetchOffsetClause(c.fetch, c.offset)
    }

    protected def buildSelectClause(c: Comprehension) = building(SelectPart) {
      b += "select "
      buildSelectModifiers(c)
      c.select match {
        case Some(Pure(StructNode(ch))) =>
          b.sep(ch, ", ") { case (sym, n) =>
            buildSelectPart(n)
            b += " as " += symbolName(sym)
          }
        case Some(Pure(ProductNode(ch))) =>
          b.sep(ch, ", ")(buildSelectPart)
        case Some(Pure(n)) => buildSelectPart(n)
        case None =>
          if(c.from.length <= 1) b += "*"
          else b += symbolName(c.from.last._1) += ".*"
      }
    }

    protected def buildSelectModifiers(c: Comprehension) {}

    protected def scanJoins(from: Seq[(Symbol, Node)]) {
      for((sym, j: Join) <- from) {
        joins += sym -> j
        scanJoins(j.nodeGenerators)
      }
    }

    protected def buildFromClause(from: Seq[(Symbol, Node)]) = building(FromPart) {
      if(from.isEmpty) scalarFrom.foreach { s => b += " from " += s }
      else {
        b += " from "
        b.sep(from, ", ") { case (sym, n) => buildFrom(n, Some(sym)) }
      }
    }

    protected def buildWhereClause(where: Seq[Node]) = building(WherePart) {
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And), true)
      }
    }

    protected def buildOrderByClause(order: Seq[(Node, Ordering)]) = building(OtherPart) {
      if(!order.isEmpty) {
        b += " order by "
        b.sep(order, ", "){ case (n, o) => buildOrdering(n, o) }
      }
    }

    protected def buildFetchOffsetClause(fetch: Option[Long], offset: Option[Long]) = building(OtherPart) {
      (fetch, offset) match {
        /* SQL:2008 syntax */
        case (Some(t), Some(d)) => b += " offset " += d += " row fetch next " += t += " row only"
        case (Some(t), None) => b += " fetch next " += t += " row only"
        case (None, Some(d)) => b += " offset " += d += " row"
        case _ =>
      }
    }

    protected def buildSelectPart(n: Node): Unit = n match {
      case c: Column[_] if useIntForBoolean && (c.typeMapper(profile) == driver.typeMapperDelegates.booleanTypeMapperDelegate) =>
        b += "case when "
        expr(n)
        b += " then 1 else 0 end"
      case n =>
        expr(n, true)
    }

    protected def buildFrom(n: Node, alias: Option[Symbol], skipParens: Boolean = false): Unit = building(FromPart) {
      def addAlias = alias foreach { s => b += ' ' += symbolName(s) }
      n match {
        case t @ TableNode(name) =>
          b += quoteIdentifier(name)
          if(alias != Some(t.nodeTableSymbol)) addAlias
        case j @ Join(leftGen, rightGen, left, right, jt, on) =>
          buildFrom(left, Some(leftGen))
          b += ' ' += jt.sqlName += " join "
          buildFrom(right, Some(rightGen))
          if(on != ConstColumn.TRUE) {
            b += " on "
            expr(on, true)
          }
        case Union(left, right, all, _, _) =>
          if(!skipParens) b += '('
          buildFrom(left, None, true)
          b += (if(all) " union all " else " union ")
          buildFrom(right, None, true)
          if(!skipParens) b += ')'
          addAlias
        case n =>
          if(!skipParens) b += '('
          buildComprehension(toComprehension(n, true))
          if(!skipParens) b += ')'
          addAlias
      }
    }

    def expr(n: Node, skipParens: Boolean = false): Unit = n match {
      case ConstColumn(true) if useIntForBoolean => b += (if(skipParens) "1=1" else "(1=1)")
      case ConstColumn(false) if useIntForBoolean => b += (if(skipParens) "1=0" else "(1=0)")
      case ConstColumn(null) => b += "null"
      case Not(Is(l, ConstColumn(null))) =>
        if(!skipParens) b += '('
        expr(l)
        b += " is not null"
        if(!skipParens) b += ')'
      case Not(e) =>
        if(!skipParens) b += '('
        b += "not "
        expr(e)
        if(!skipParens) b += ')'
      case i @ InSet(e, seq, bind) => if(seq.isEmpty) expr(ConstColumn.FALSE) else {
        if(!skipParens) b += '('
        expr(e); b += " in ("
        if(bind) b.sep(seq, ",")(x => b +?= { (p, param) => i.tm(driver).setValue(x, p) })
        else b += seq.map(i.tm(driver).valueToSQLLiteral).mkString(",")
        b += ')'
        if(!skipParens) b += ')'
      }
      case Is(l, ConstColumn(null)) =>
        if(!skipParens) b += '('
        expr(l)
        b += " is null"
        if(!skipParens) b += ')'
      case Is(left: ProductNode, right: ProductNode) =>
        if(!skipParens) b += '('
        if(supportsTuples) {
          expr(left)
          b += " = "
          expr(right)
        } else {
          val cols = left.nodeChildren zip right.nodeChildren
          b.sep(cols, " and "){ case (l,r) => expr(l); b += "="; expr(r) }
        }
        if(!skipParens) b += ')'
      case ProductNode(ch) =>
        if(!skipParens) b += '('
        b.sep(ch, ", ")(expr(_))
        if(!skipParens) b += ')'
      case Is(l, r) =>
        if(!skipParens) b += '('
        expr(l)
        b += '='
        expr(r)
        if(!skipParens) b += ')'
      case StdFunction("exists", c: Comprehension) if(!supportsTuples) =>
        /* If tuples are not supported, selecting multiple individial columns
         * in exists(select ...) is probably not supported, either, so we rewrite
         * such sub-queries to "select *". */
        b += "exists("
        expr(c.copy(select = None), true)
        b += ')'
      case EscFunction("concat", l, r) if concatOperator.isDefined =>
        if(!skipParens) b += '('
        expr(l)
        b += concatOperator.get
        expr(r)
        if(!skipParens) b += ')'
      case s: SimpleFunction =>
        if(s.scalar) b += "{fn "
        b += s.name += '('
        b.sep(s.nodeChildren, ",")(expr(_, true))
        b += ')'
        if(s.scalar) b += '}'
      case SimpleLiteral(w) => b += w
      case s: SimpleExpression => s.toSQL(this)
      case Between(left, start, end) => expr(left); b += " between "; expr(start); b += " and "; expr(end)
      case CountDistinct(e) => b += "count(distinct "; expr(e); b += ')'
      case Like(l, r, esc) =>
        if(!skipParens) b += '('
        expr(l)
        b += " like "
        expr(r)
        esc.foreach { ch =>
          if(ch == '\'' || ch == '%' || ch == '_') throw new SLICKException("Illegal escape character '"+ch+"' for LIKE expression")
          // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
          b += " escape '" += ch += "'"
        }
        if(!skipParens) b += ')'
      case a @ AsColumnOf(ch, name) =>
        val tn = name.getOrElse(mapTypeName(a.typeMapper(driver)))
        if(supportsCast) {
          b += "cast("
          expr(ch)
          b += " as " += tn += ")"
        } else {
          b += "{fn convert("
          expr(ch, true)
          b += ',' += tn += ")}"
        }
      case s: SimpleBinaryOperator =>
        if(!skipParens) b += '('
        expr(s.left)
        b += ' ' += s.name += ' '
        expr(s.right)
        if(!skipParens) b += ')'
      case c @ ConstColumn(v) => b += c.typeMapper(driver).valueToSQLLiteral(v)
      case c @ BindColumn(v) => b +?= { (p, param) => c.typeMapper(driver).setValue(v, p) }
      case pc @ ParameterColumn(_, extractor) => b +?= { (p, param) =>
        pc.typeMapper(driver).setValue(extractor.asInstanceOf[(Any => Any)](param), p)
      }
      case c: Case.CaseNode =>
        b += "(case"
        c.clauses.foldRight(()) { (w,_) =>
          b += " when "
          expr(w.asInstanceOf[Case.WhenNode].left)
          b += " then "
          expr(w.asInstanceOf[Case.WhenNode].right)
        }
        c.elseClause match {
          case ConstColumn(null) =>
          case n =>
            b += " else "
            expr(n)
        }
        b += " end)"
      case Path(psyms) =>
        val field = psyms.head
        val struct = psyms.tail.reduceRight[Symbol] {
          case (ElementSymbol(idx), z) => joins(z).nodeGenerators(idx-1)._1
        }
        b += symbolName(struct) += '.' += symbolName(field)
      case CountStar => b += "count(*)"
      case n => // try to build a sub-query
        if(!skipParens) b += '('
        buildComprehension(toComprehension(n))
        if(!skipParens) b += ')'
    }

    protected def buildOrdering(n: Node, o: Ordering) {
      expr(n)
      if(o.direction.desc) b += " desc"
      if(o.nulls.first) b += " nulls first"
      else if(o.nulls.last) b += " nulls last"
    }

    def buildUpdate: QueryBuilderResult = {
      val (gen, from, where, select) = ast match {
        case Comprehension(Seq((sym, from: TableNode)), where, _, Some(Pure(select)), None, None) => select match {
          case f @ Select(Ref(struct), _) if struct == sym => (sym, from, where, Seq(f.field))
          case ProductNode(ch) if ch.forall{ case Select(Ref(struct), _) if struct == sym => true; case _ => false} =>
            (sym, from, where, ch.map{ case Select(Ref(_), field) => field })
          case _ => throw new SLICKException("A query for an UPDATE statement must select table columns only -- Unsupported shape: "+select)
        }
        case _ => throw new SLICKException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
      }

      val qtn = quoteIdentifier(from.tableName)
      symbolName(gen) = qtn // Alias table to itself because UPDATE does not support aliases
      b += "update " += qtn += " set "
      b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And), true)
      }
      QueryBuilderResult(b.build, linearizer)
    }

    def buildDelete: QueryBuilderResult = {
      val (gen, from, where) = ast match {
        case Comprehension(Seq((sym, from: TableNode)), where, _, Some(Pure(select)), None, None) => (sym, from, where)
        case _ => throw new SLICKException("A query for a DELETE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
      }
      val qtn = quoteIdentifier(from.tableName)
      symbolName(gen) = qtn // Alias table to itself because DELETE does not support aliases
      b += "delete from " += qtn
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And), true)
      }
      QueryBuilderResult(b.build, linearizer)
    }

    /*TODO
    final protected def appendGroupClause(): Unit = query.typedModifiers[Grouping] match {
      case Nil =>
      case xs => b += " group by "; b.sep(xs, ",")(x => expr(x.by))
    }
    */
  }

  /** Builder for INSERT statements. */
  class InsertBuilder(val column: Any) {

    def buildInsert: String = {
      val (table, cols, vals) = buildParts
      "INSERT INTO " + quoteIdentifier(table) + " (" + cols + ") VALUES (" + vals + ")"
    }

    def buildInsert(query: Query[_, _]): QueryBuilderResult = {
      val (table, cols, _) = buildParts
      val qb = driver.createQueryBuilder(query)
      qb.sqlBuilder += "INSERT INTO " += quoteIdentifier(table) += " (" += cols.toString += ") "
      qb.buildSelect()
    }

    protected def buildParts: (String, StringBuilder, StringBuilder) = {
      val cols = new StringBuilder
      val vals = new StringBuilder
      var table:String = null
      def f(c: Any): Unit = c match {
        case p:Projection[_] =>
          for(i <- 0 until p.productArity)
            f(Node(p.productElement(i)))
        case t:TableNode => f(Node(t.nodeShaped_*.value))
        case n:NamedColumn[_] =>
          if(table eq null) table = n.table.asInstanceOf[TableNode].tableName
          else if(table != n.table.asInstanceOf[TableNode].tableName) throw new SLICKException("Inserts must all be to the same table")
          appendNamedColumn(n.raw, cols, vals)
        case Select(Ref(GlobalSymbol(_, t: TableNode)), field: FieldSymbol) =>
          val n = field.column.get
          if(table eq null) table = t.tableName
          else if(table != t.tableName) throw new SLICKException("Inserts must all be to the same table")
          appendNamedColumn(n, cols, vals)
        case _ => throw new SLICKException("Cannot use column "+c+" in INSERT statement")
      }
      f(Node(column))
      if(table eq null) throw new SLICKException("No table to insert into")
      (table, cols, vals)
    }

    protected def appendNamedColumn(n: RawNamedColumn, cols: StringBuilder, vals: StringBuilder) {
      if(!cols.isEmpty) {
        cols append ","
        vals append ","
      }
      cols append quoteIdentifier(n.name)
      vals append '?'
    }
  }

  /** Builder for various DDL statements. */
  class TableDDLBuilder(val table: Table[_]) {
    protected val indexes: Iterable[Index] = table.indexes
    protected val foreignKeys: Iterable[ForeignKey[_ <: TableNode, _]] = table.foreignKeys
    protected val primaryKeys: Iterable[PrimaryKey] = table.primaryKeys

    def buildDDL: DDL = {
      if(primaryKeys.size > 1)
        throw new SLICKException("Table "+table.tableName+" defines multiple primary keys ("
          + primaryKeys.map(_.name).mkString(", ") + ")")
      new DDL {
        val createPhase1 = Iterable(createTable) ++ primaryKeys.map(createPrimaryKey) ++ indexes.map(createIndex)
        val createPhase2 = foreignKeys.map(createForeignKey)
        val dropPhase1 = foreignKeys.map(dropForeignKey)
        val dropPhase2 = primaryKeys.map(dropPrimaryKey) ++ Iterable(dropTable)
      }
    }

    protected def createTable: String = {
      val b = new StringBuilder append "create table " append quoteIdentifier(table.tableName) append " ("
      var first = true
      for(n <- table.create_*) {
        if(first) first = false
        else b append ","
        createColumnDDLBuilder(n, table).appendColumn(b)
      }
      addTableOptions(b)
      b append ")"
      b.toString
    }

    protected def addTableOptions(b: StringBuilder) {}

    protected def dropTable: String =
      "drop table " + quoteIdentifier(table.tableName)

    protected def createIndex(idx: Index): String = {
      val b = new StringBuilder append "create "
      if(idx.unique) b append "unique "
      b append "index " append quoteIdentifier(idx.name) append " on " append quoteIdentifier(table.tableName) append "("
      addIndexColumnList(idx.on, b, idx.table.tableName)
      b append ")"
      b.toString
    }

    protected def createForeignKey(fk: ForeignKey[_ <: TableNode, _]): String = {
      val sb = new StringBuilder append "alter table " append quoteIdentifier(table.tableName) append " add "
      addForeignKey(fk, sb)
      sb.toString
    }

    protected def addForeignKey(fk: ForeignKey[_ <: TableNode, _], sb: StringBuilder) {
      sb append "constraint " append quoteIdentifier(fk.name) append " foreign key("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
      sb append ") references " append quoteIdentifier(fk.targetTable.tableName) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      sb append ") on update " append fk.onUpdate.action
      sb append " on delete " append fk.onDelete.action
    }

    protected def createPrimaryKey(pk: PrimaryKey): String = {
      val sb = new StringBuilder append "alter table " append quoteIdentifier(table.tableName) append " add "
      addPrimaryKey(pk, sb)
      sb.toString
    }

    protected def addPrimaryKey(pk: PrimaryKey, sb: StringBuilder) {
      sb append "constraint " append quoteIdentifier(pk.name) append " primary key("
      addPrimaryKeyColumnList(pk.columns, sb, table.tableName)
      sb append ")"
    }

    protected def dropForeignKey(fk: ForeignKey[_ <: TableNode, _]): String =
      "alter table " + quoteIdentifier(table.tableName) + " drop constraint " + quoteIdentifier(fk.name)

    protected def dropPrimaryKey(pk: PrimaryKey): String =
      "alter table " + quoteIdentifier(table.tableName) + " drop constraint " + quoteIdentifier(pk.name)

    protected def addIndexColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "index")

    protected def addForeignKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addPrimaryKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String, typeInfo: String) {
      var first = true
      for(c <- columns) c match {
        case Select(Ref(GlobalSymbol(_, t: TableNode)), field: FieldSymbol) =>
          val n = field.column.get
          if(first) first = false
          else sb append ","
          sb append quoteIdentifier(n.name)
          if(requiredTableName != t.tableName)
            throw new SLICKException("All columns in "+typeInfo+" must belong to table "+requiredTableName)
        case _ => throw new SLICKException("Cannot use column "+c+" in "+typeInfo+" (only named columns are allowed)")
      }
    }
  }

  /** Builder for column specifications in DDL statements. */
  class ColumnDDLBuilder(column: RawNamedColumn) {
    protected val tmDelegate = column.typeMapper(driver)
    protected var sqlType: String = null
    protected var notNull = !tmDelegate.nullable
    protected var autoIncrement = false
    protected var primaryKey = false
    protected var defaultLiteral: String = null
    init()

    protected def init() {
      for(o <- column.options) handleColumnOption(o)
      if(sqlType eq null) sqlType = mapTypeName(tmDelegate)
    }

    protected def handleColumnOption(o: ColumnOption[_]): Unit = o match {
      case ColumnOption.DBType(s) => sqlType = s
      case ColumnOption.NotNull => notNull = true
      case ColumnOption.Nullable => notNull = false
      case ColumnOption.AutoInc => autoIncrement = true
      case ColumnOption.PrimaryKey => primaryKey = true
      case ColumnOption.Default(v) => defaultLiteral =
        column.asInstanceOf[RawNamedColumn].typeMapper(driver).asInstanceOf[TypeMapperDelegate[Any]].valueToSQLLiteral(v)
    }

    def appendColumn(sb: StringBuilder) {
      sb append quoteIdentifier(column.name) append ' '
      sb append sqlType
      appendOptions(sb)
    }

    protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(autoIncrement) sb append " GENERATED BY DEFAULT AS IDENTITY(START WITH 1)"
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
    }
  }

  /** Builder for DDL statements for sequences. */
  class SequenceDDLBuilder(seq: Sequence[_]) {
    def buildDDL: DDL = {
      val b = new StringBuilder append "create sequence " append quoteIdentifier(seq.name)
      seq._increment.foreach { b append " increment " append _ }
      seq._minValue.foreach { b append " minvalue " append _ }
      seq._maxValue.foreach { b append " maxvalue " append _ }
      seq._start.foreach { b append " start " append _ }
      if(seq._cycle) b append " cycle"
      new DDL {
        val createPhase1 = Iterable(b.toString)
        val createPhase2 = Nil
        val dropPhase1 = Nil
        val dropPhase2 = Iterable("drop sequence " + quoteIdentifier(seq.name))
      }
    }
  }
}

case class QueryBuilderResult(sbr: SQLBuilder.Result, linearizer: ValueLinearizer[_]) {
  def sql = sbr.sql
  def setter = sbr.setter
}
