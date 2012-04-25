package scala.slick.driver

import scala.slick.SLICKException
import scala.slick.ast._
import scala.slick.util._
import scala.slick.ql._
import scala.slick.ql.ColumnOps._

trait BasicStatementBuilderComponent { driver: BasicDriver =>

  /** Builder for SELECT and UPDATE statements. */
  class QueryBuilder(val ast: Node, val linearizer: ValueLinearizer[_]) {
    protected final val b = new SQLBuilder

    protected val mayLimit0 = true
    protected val scalarFrom: Option[String] = None
    protected val supportsTuples = true
    protected val supportsCast = true
    protected val concatOperator: Option[String] = None

    def sqlBuilder = b

    final def buildSelect(): QueryBuilderResult = {
      buildComprehension(ast)
      QueryBuilderResult(b.build, linearizer)
    }

    protected def buildComprehension(n: Node): Unit = n match {
      case Comprehension(from, where, orderBy, select) =>
        b += "select "
        select match {
          case Some(n) => buildSelectClause(n)
          case None =>
            if(from.length <= 1) b += "*"
            else b += symbolName(from.last._1) += ".*"
        }
        if(from.isEmpty) buildScalarFrom
        else {
          b += " from "
          b.sep(from, ", ") { case (sym, n) =>
            buildFrom(n, Some(sym))
          }
        }
        if(!where.isEmpty) {
          b += " where "
          expr(where.reduceLeft(And))
        }
        if(!orderBy.isEmpty) appendOrderClause(orderBy)
      case Pure(CountAll(q)) =>
        b += "select count(*) from "
        buildFrom(q, None)
      case p @ Pure(_) =>
        b += "select "
        buildSelectClause(p)
        buildScalarFrom
      case TableNode(name) =>
        b += "select * from " += quoteIdentifier(name)
      case TakeDrop(from, take, drop) => buildTakeDrop(from, take, drop)
      case Union(left, right, all, _, _) =>
        b += "select * from "
        buildFrom(left, None)
        b += (if(all) " union all " else " union ")
        buildFrom(right, None)
      case n => throw new SLICKException("Unexpected node "+n+" -- SQL prefix: "+b.build.sql)
    }

    protected def buildScalarFrom: Unit = scalarFrom.foreach { s => b += " from " += s }

    protected def buildTakeDrop(from: Node, take: Option[Int], drop: Option[Int]) {
      if(take == Some(0)) {
        b += "select * from "
        buildFrom(from, None)
        b += " where 1=0"
      } else {
        buildComprehension(from)
        appendTakeDropClause(take, drop)
      }
    }

    protected def appendTakeDropClause(take: Option[Int], drop: Option[Int]) = (take, drop) match {
      /* SQL:2008 syntax */
      case (Some(t), Some(d)) => b += " offset " += d += " row fetch next " += t += " row only"
      case (Some(t), None) => b += " fetch next " += t += " row only"
      case (None, Some(d)) => b += " offset " += d += " row"
      case _ =>
    }

    protected def buildSelectClause(n: Node): Unit = n match {
      case Pure(StructNode(ch)) =>
        b.sep(ch, ", ") { case (sym, n) =>
          expr(n)
          b += " as " += symbolName(sym)
        }
      case Pure(ProductNode(ch @ _*)) =>
        b.sep(ch, ", ")(expr)
      case Pure(n) => expr(n)
    }

    protected def buildFrom(n: Node, alias: Option[Symbol]){
      def addAlias = alias foreach { s => b += ' ' += symbolName(s) }
      n match {
        case TableNode(name) =>
          b += quoteIdentifier(name)
          addAlias
        case BaseJoin(leftGen, rightGen, left, right, jt) =>
          buildFrom(left, Some(leftGen))
          b += ' ' += jt.sqlName += " join "
          buildFrom(right, Some(rightGen))
        case FilteredJoin(leftGen, rightGen, left, right, jt, on) =>
          buildFrom(left, Some(leftGen))
          b += ' ' += jt.sqlName += " join "
          buildFrom(right, Some(rightGen))
          b += " on "
          expr(on)
        case n =>
          b += '('
          buildComprehension(n)
          b += ')'
          addAlias
      }
    }

    protected def symbolName(s: Symbol): String = s match {
      case AnonSymbol(name) => name
      case s => quoteIdentifier(s.name)
    }

    def expr(n: Node): Unit = n match {
      case ConstColumn(null) => b += "null"
      case Not(Is(l, ConstColumn(null))) => b += '('; expr(l); b += " is not null)"
      case Not(e) => b += "(not "; expr(e); b+= ')'
      case i @ InSet(e, seq, bind) => if(seq.isEmpty) expr(ConstColumn.FALSE) else {
        b += '('; expr(e); b += " in ("
        if(bind) b.sep(seq, ",")(x => b +?= { (p, param) => i.tm(driver).setValue(x, p) })
        else b += seq.map(i.tm(driver).valueToSQLLiteral).mkString(",")
        b += "))"
      }
      case Is(l, ConstColumn(null)) => b += '('; expr(l); b += " is null)"
      case Is(left: ProductNode, right: ProductNode) =>
        if(supportsTuples) {
          b += "("
          expr(left)
          b += " = "
          expr(right)
          b += ")"
        } else {
          val cols = left.nodeChildren zip right.nodeChildren
          b += "("
          b.sep(cols, " and "){ case (l,r) => expr(l); b += "="; expr(r) }
          b += ")"
        }
      case ProductNode(ch @ _*) =>
        b += "("
        b.sep(ch, ", ")(expr)
        b += ")"
      case Is(l, r) => b += '('; expr(l); b += '='; expr(r); b += ')'
      case EscFunction("concat", l, r) if concatOperator.isDefined =>
        b += '('; expr(l); b += concatOperator.get; expr(r); b += ')'
      case s: SimpleFunction =>
        if(s.scalar) b += "{fn "
        b += s.name += '('
        b.sep(s.nodeChildren, ",")(expr)
        b += ')'
        if(s.scalar) b += '}'
      case SimpleLiteral(w) => b += w
      case s: SimpleExpression => s.toSQL(this)
      case Between(left, start, end) => expr(left); b += " between "; expr(start); b += " and "; expr(end)
      case CountDistinct(e) => b += "count(distinct "; expr(e); b += ')'
      case Like(l, r, esc) =>
        b += '('; expr(l); b += " like "; expr(r);
        esc.foreach { ch =>
          if(ch == '\'' || ch == '%' || ch == '_') throw new SLICKException("Illegal escape character '"+ch+"' for LIKE expression")
          // JDBC defines an {escape } syntax but the unescaped version is understood by more DBs/drivers
          b += " escape '" += ch += "'"
        }
        b += ')'
      case a @ AsColumnOf(ch, name) =>
        val tn = name.getOrElse(mapTypeName(a.typeMapper(driver)))
        if(supportsCast) {
          b += "cast("
          expr(ch)
          b += " as " += tn += ")"
        } else {
          b += "{fn convert("
          expr(ch)
          b += ',' += tn += ")}"
        }
      case s: SimpleBinaryOperator => b += '('; expr(s.left); b += ' ' += s.name += ' '; expr(s.right); b += ')'
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
      case FieldRef(struct, field) => b += symbolName(struct) += '.' += symbolName(field)
      //TODO case CountAll(q) => b += "count(*)"; localTableName(q)
      //TODO case query:Query[_, _] => b += "("; subQueryBuilderFor(query).innerBuildSelect(b, false); b += ")"
      //TODO case sq @ Subquery(_, _) => b += quoteIdentifier(localTableName(sq)) += ".*"
      case n => // try to build a sub-query
        b += '('
        buildComprehension(n)
        b += ')'
      //case _ => throw new SLICKException("Don't know what to do with node "+n+" in an expression")
    }

    protected def appendOrderClause(order: Seq[(Node, Ordering)]) {
      b += " order by "
      b.sep(order, ", "){ case (n, o) => appendOrdering(n, o) }
    }

    protected def appendOrdering(n: Node, o: Ordering) {
      expr(n)
      if(o.direction.desc) b += " desc"
      if(o.nulls.first) b += " nulls first"
      else if(o.nulls.last) b += " nulls last"
    }

    def buildUpdate: QueryBuilderResult = {
      val (gen, from, where, select) = ast match {
        case Comprehension(Seq((sym, from: TableNode)), where, _, Some(Pure(select))) => select match {
          case f @ FieldRef(struct, _) if struct == sym => (sym, from, where, Seq(f.field))
          case ProductNode(ch @ _*) if ch.forall{ case FieldRef(struct, _) if struct == sym => true; case _ => false} =>
            (sym, from, where, ch.map{ case FieldRef(_, field) => field })
          case _ => throw new SLICKException("A query for an UPDATE statement must select table columns only -- Unsupported shape: "+select)
        }
        case _ => throw new SLICKException("A query for an UPDATE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
      }

      b += "update " += quoteIdentifier(from.tableName) += ' ' += symbolName(gen) += " set "
      b.sep(select, ", ")(field => b += symbolName(field) += " = ?")
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And))
      }
      //TODO nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
      QueryBuilderResult(b.build, linearizer)
    }

    def buildDelete: QueryBuilderResult = {
      val (gen, from, where) = ast match {
        case Comprehension(Seq((sym, from: TableNode)), where, _, Some(Pure(select))) => (sym, from, where)
        case _ => throw new SLICKException("A query for a DELETE statement must resolve to a comprehension with a single table -- Unsupported shape: "+ast)
      }

      b += "delete from " += quoteIdentifier(from.tableName) += ' ' += symbolName(gen)
      if(!where.isEmpty) {
        b += " where "
        expr(where.reduceLeft(And))
      }
      //TODO nc = nc.overrideName(table, tableName) // Alias table to itself because UPDATE does not support aliases
      QueryBuilderResult(b.build, linearizer)
    }





    protected def rewriteCountStarQuery(q: Query[_, _]) =
      /*q.modifiers.isEmpty &&*/ (q.packed match {
        case _: TableNode => true
        case _ => false
      })

    protected def innerBuildSelectNoRewrite(rename: Boolean): Unit = sys.error("obsolete")

    protected def appendClauses(): Unit = sys.error("obsolete")

    /*TODO
    final protected def appendGroupClause(): Unit = query.typedModifiers[Grouping] match {
      case Nil =>
      case xs => b += " group by "; b.sep(xs, ",")(x => expr(x.by))
    }
    */

    protected def innerExpr(c: Node): Unit = sys.error("obsolete")

    final protected def appendConditions(): Unit = sys.error("obsolete")
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
        case Wrapped(t: TableNode, n: RawNamedColumn) =>
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
  class DDLBuilder(val table: AbstractBasicTable[_]) {
    //TODO: Move AutoInc handling to extended profile

    protected def createColumnDDLBuilder(c: RawNamedColumn) = new ColumnDDLBuilder(c)

    protected class ColumnDDLBuilder(protected val column: RawNamedColumn) {
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

      protected def handleColumnOption(o: ColumnOption[_,_]): Unit = o match {
        case BasicColumnOption.DBType(s) => sqlType = s
        case BasicColumnOption.NotNull => notNull = true
        case BasicColumnOption.Nullable => notNull = false
        case ExtendedColumnOption.AutoInc => autoIncrement = true
        case BasicColumnOption.PrimaryKey => primaryKey = true
        case BasicColumnOption.Default(v) => defaultLiteral =
          column.asInstanceOf[RawNamedColumn].typeMapper(driver).asInstanceOf[TypeMapperDelegate[Any]].valueToSQLLiteral(v)
      }

      def appendColumn(sb: StringBuilder) {
        sb append quoteIdentifier(column.name) append ' '
        sb append sqlType
        appendOptions(sb)
      }

      protected def appendOptions(sb: StringBuilder) {
        if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
        if(notNull) sb append " NOT NULL"
        if(autoIncrement) sb append " AUTO_INCREMENT"
        if(primaryKey) sb append " PRIMARY KEY"
      }
    }

    def buildDDL: DDL = {
      val createTable = {
        val b = new StringBuilder append "CREATE TABLE " append quoteIdentifier(table.tableName) append " ("
        var first = true
        for(n <- table.create_*) {
          if(first) first = false
          else b append ","
          createColumnDDLBuilder(n).appendColumn(b)
        }
        b append ")"
        b.toString
      }
      val createIndexes = table.indexes.map(createIndex)
      val foreignKeys = table.foreignKeys
      val primaryKeys = table.primaryKeys
      if(primaryKeys.size > 1)
        throw new SLICKException("Table "+table.tableName+" defines multiple primary keys")
      new DDL {
        val createPhase1 = Iterable(createTable) ++ primaryKeys.map(createPrimaryKey) ++ createIndexes
        val createPhase2 = foreignKeys.map(createForeignKey)
        val dropPhase1 = foreignKeys.map(dropForeignKey)
        val dropPhase2 = primaryKeys.map(dropPrimaryKey) ++ Iterable("DROP TABLE " + quoteIdentifier(table.tableName))
      }
    }

    protected def createIndex(idx: Index) = {
      val b = new StringBuilder append "CREATE "
      if(idx.unique) b append "UNIQUE "
      b append "INDEX " append quoteIdentifier(idx.name) append " ON " append quoteIdentifier(table.tableName) append "("
      addIndexColumnList(idx.on, b, idx.table.tableName)
      b append ")"
      b.toString
    }

    protected def createForeignKey(fk: ForeignKey[_ <: TableNode, _]) = {
      val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
      addForeignKey(fk, sb)
      sb.toString
    }

    protected def addForeignKey(fk: ForeignKey[_ <: TableNode, _], sb: StringBuilder) {
      sb append "CONSTRAINT " append quoteIdentifier(fk.name) append " FOREIGN KEY("
      addForeignKeyColumnList(fk.linearizedSourceColumns, sb, table.tableName)
      sb append ") REFERENCES " append quoteIdentifier(fk.targetTable.tableName) append "("
      addForeignKeyColumnList(fk.linearizedTargetColumnsForOriginalTargetTable, sb, fk.targetTable.tableName)
      sb append ") ON UPDATE " append fk.onUpdate.action
      sb append " ON DELETE " append fk.onDelete.action
    }

    protected def createPrimaryKey(pk: PrimaryKey) = {
      val sb = new StringBuilder append "ALTER TABLE " append quoteIdentifier(table.tableName) append " ADD "
      addPrimaryKey(pk, sb)
      sb.toString
    }

    protected def addPrimaryKey(pk: PrimaryKey, sb: StringBuilder) {
      sb append "CONSTRAINT " append quoteIdentifier(pk.name) append " PRIMARY KEY("
      addPrimaryKeyColumnList(pk.columns, sb, table.tableName)
      sb append ")"
    }

    protected def dropForeignKey(fk: ForeignKey[_ <: TableNode, _]) = {
      "ALTER TABLE " + quoteIdentifier(table.tableName) + " DROP CONSTRAINT " + quoteIdentifier(fk.name)
    }

    protected def dropPrimaryKey(pk: PrimaryKey) = {
      "ALTER TABLE " + quoteIdentifier(table.tableName) + " DROP CONSTRAINT " + quoteIdentifier(pk.name)
    }

    protected def addIndexColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "index")

    protected def addForeignKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addPrimaryKeyColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String) =
      addColumnList(columns, sb, requiredTableName, "foreign key constraint")

    protected def addColumnList(columns: IndexedSeq[Node], sb: StringBuilder, requiredTableName: String, typeInfo: String) = {
      var first = true
      for(c <- columns) c match {
        case Wrapped(t: TableNode, n: RawNamedColumn) =>
          if(first) first = false
          else sb append ","
          sb append quoteIdentifier(n.name)
          if(requiredTableName != t.tableName)
            throw new SLICKException("All columns in "+typeInfo+" must belong to table "+requiredTableName)
        case _ => throw new SLICKException("Cannot use column "+c+
          " in "+typeInfo+" (only named columns are allowed)")
      }
    }
  }

  /** Builder for DDL statements for sequences. */
  class SequenceDDLBuilder(seq: Sequence[_]) {
    def buildDDL: DDL = {
      val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
      seq._increment.foreach { b append " INCREMENT " append _ }
      seq._minValue.foreach { b append " MINVALUE " append _ }
      seq._maxValue.foreach { b append " MAXVALUE " append _ }
      seq._start.foreach { b append " START " append _ }
      if(seq._cycle) b append " CYCLE"
      new DDL {
        val createPhase1 = Iterable(b.toString)
        val createPhase2 = Nil
        val dropPhase1 = Nil
        val dropPhase2 = Iterable("DROP SEQUENCE " + quoteIdentifier(seq.name))
      }
    }
  }
}

case class QueryBuilderResult(sbr: SQLBuilder.Result, linearizer: ValueLinearizer[_]) {
  def sql = sbr.sql
  def setter = sbr.setter
}
