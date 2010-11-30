package org.scalaquery.ql.extended

import org.scalaquery.SQueryException
import org.scalaquery.ql._
import org.scalaquery.ql.basic._
import org.scalaquery.util._

class DerbyDriver extends ExtendedProfile { self =>

  type ImplicitT = ExtendedImplicitConversions[DerbyDriver]
  type TypeMapperDelegatesT = BasicTypeMapperDelegates

  val Implicit = new ExtendedImplicitConversions[DerbyDriver] {
    implicit val scalaQueryDriver = self
  }

  val typeMapperDelegates = new DerbyTypeMapperDelegates

  override def createQueryBuilder(query: Query[_], nc: NamingContext) = new DerbyQueryBuilder(query, nc, None, this)
  override def buildTableDDL(table: AbstractBasicTable[_]): DDL = new DerbyDDLBuilder(table, this).buildDDL
  override def buildSequenceDDL(seq: Sequence[_]): DDL = new DerbySequenceDDLBuilder(seq, this).buildDDL
}

object DerbyDriver extends DerbyDriver

class DerbyTypeMapperDelegates extends BasicTypeMapperDelegates {
  import DerbyTypeMapperDelegates._
  override val booleanTypeMapperDelegate = new BooleanTypeMapperDelegate
}

object DerbyTypeMapperDelegates {
  /* Derby does not have a proper BOOLEAN type. The suggested workaround is
   * SMALLINT with constants 1 and 0 for TRUE and FALSE. */
  class BooleanTypeMapperDelegate extends BasicTypeMapperDelegates.BooleanTypeMapperDelegate {
    override def sqlTypeName = "SMALLINT"
    override def valueToSQLLiteral(value: Boolean) = if(value) "1" else "0"
  }
}

class DerbyQueryBuilder(_query: Query[_], _nc: NamingContext, parent: Option[BasicQueryBuilder], profile: DerbyDriver)
extends BasicQueryBuilder(_query, _nc, parent, profile) {

  import ExtendedQueryOps._
  import profile.sqlUtils._

  override type Self = DerbyQueryBuilder

  protected def createSubQueryBuilder(query: Query[_], nc: NamingContext) =
    new DerbyQueryBuilder(query, nc, Some(this), profile)

  override protected def innerBuildSelectNoRewrite(b: SQLBuilder, rename: Boolean) {
    query.typedModifiers[TakeDrop] match {
      case TakeDrop(Some(0), _) :: _ =>
        /* Derby does not allow fetching 0 rows, so we use this workaround to
         * force the query to return no results */
        b += "SELECT * FROM ("
        super.innerBuildSelectNoRewrite(b, rename)
        b += ") t0 WHERE 1=0"
      case _ =>
        super.innerBuildSelectNoRewrite(b, rename)
    }
  }

  override protected def expr(c: Node, b: SQLBuilder, rename: Boolean, topLevel: Boolean): Unit = {
    c match {
      /* Convert proper BOOLEANs which should be returned from a SELECT
       * statement into pseudo-boolean SMALLINT values 1 and 0 */
      case c: Column[_] if topLevel && !rename && b == selectSlot && c.typeMapper(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
        b += "case when "
        innerExpr(c, b)
        b += " then 1 else 0 end"
      case _ => super.expr(c, b, rename, topLevel)
    }
  }

  override protected def innerExpr(c: Node, b: SQLBuilder): Unit = c match {
    /* Create TRUE and FALSE values because Derby lacks boolean literals */
    case c @ ConstColumn(true) => b += "(1=1)"
    case c @ ConstColumn(false) => b += "(1=0)"

    /* Convert pseudo-booleans from tables and subqueries to real booleans */
    case n: NamedColumn[_] if n.typeMapper(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
      b += "("; super.innerExpr(c, b); b += " != 0)"
    case c @ SubqueryColumn(pos, sq, tm) if tm(profile) == profile.typeMapperDelegates.booleanTypeMapperDelegate =>
      b += "("; super.innerExpr(c, b); b += " != 0)"

    case a @ ColumnOps.AsColumnOf(ch, name) =>
      /* Derby does not support {fn convert}, so we use the SQL CAST syntax */
      b += "cast("; expr(ch, b); b += " as " += name.getOrElse(mapTypeName(a.typeMapper(profile))) += ")"

    case ColumnOps.IfNull(l, r) => r match {
      /* Derby does not support IFNULL so we use COALESCE instead,
       * and it requires NULLs to be casted to a suitable type */
      case c: Column[_] =>
        b += "coalesce(cast("
        expr(l, b)
        b += " as " += mapTypeName(c.typeMapper(profile)) += "),"
        expr(r, b); b += ")"
      case _ => throw new SQueryException("Cannot determine type of right-hand side for ifNull")
    }

    case fk: ForeignKey[_] =>
      /* Derby does not support row value constructor syntax (tuple syntax),
       * so we need to untuple and compare the individual columns (which
       * should be safe because they may not contain NULLs). */
      val cols = untupleColumn(fk.left) zip untupleColumn(fk.right)
      b += "("
      for((l,r) <- b.sep(cols, " and ")) {
        expr(l, b); b += "="; expr(r, b);
      }
      b += ")"

    case c @ BindColumn(v) if b == selectSlot =>
      /* The Derby embedded driver has a bug (DERBY-4671) which results in a
       * NullPointerException when using bind variables in a SELECT clause.
       * This should be fixed in Derby 10.6.1.1. The workaround is to add an
       * explicit type annotation (in the form of a CAST expression). */
      val tmd = c.typeMapper(profile)
      b += "cast("
      b +?= { (p, param) => tmd.setValue(v, p) }
      b += " as " += mapTypeName(tmd) += ")"

    /* I guess NEXTVAL was too short */
    case Sequence.Nextval(seq) => b += "(next value for " += quoteIdentifier(seq.name) += ")"

    case Sequence.Currval(seq) => throw new SQueryException("Derby does not support CURRVAL")

    case _ => super.innerExpr(c, b)
  }

  override protected def insertFromClauses() {
    super.insertFromClauses()
    /* At least it makes more sense than calling it "DUAL"... */
    if(fromSlot.isEmpty) fromSlot += " FROM sysibm.sysdummy1"
  }

  override protected def appendClauses(b: SQLBuilder): Unit = {
    super.appendClauses(b)
    appendLimitClause(b)
  }

  protected def appendLimitClause(b: SQLBuilder): Unit = query.typedModifiers[TakeDrop].lastOption.foreach {
    /* Derby uses the SQL:2008 syntax for skip/limit */
    case TakeDrop(Some(0), _) => // handled above in innerBuildSelectNoRewrite
    case TakeDrop(Some(t), Some(d)) => b += " OFFSET " += d += " ROW FETCH NEXT " += t += " ROW ONLY"
    case TakeDrop(Some(t), None) => b += " FETCH NEXT " += t += " ROW ONLY"
    case TakeDrop(None, Some(d)) => b += " OFFSET " += d += " ROW"
    case _ =>
  }

  override protected def table(t: Node, name: String, b: SQLBuilder): Unit = t match {
    /* Derby requires columns of UNION parts to have the same names. If my
     * understanding of SQL:2008 is correct, this is a bug. This behavior
     * would be correct if the CORRESPONDING keyword was used for a UNION.
     * The workaround is to rename all parts with the same auto-generated
     * column names. */
    case Subquery(Union(all, sqs), rename) =>
      b += "("
      for(sq <- b.sep(sqs, (if(all) " UNION ALL " else " UNION ")))
        subQueryBuilderFor(sq).innerBuildSelect(b, rename)
      b += ") " += quoteIdentifier(name)
    case _ => super.table(t, name, b)
  }
}

class DerbyDDLBuilder(table: AbstractBasicTable[_], profile: DerbyDriver) extends BasicDDLBuilder(table, profile) {
  import profile.sqlUtils._

  protected class DerbyColumnDDLBuilder(column: NamedColumn[_]) extends BasicColumnDDLBuilder(column) {
    override protected def appendOptions(sb: StringBuilder) {
      if(defaultLiteral ne null) sb append " DEFAULT " append defaultLiteral
      if(notNull) sb append " NOT NULL"
      if(primaryKey) sb append " PRIMARY KEY"
      if(autoIncrement) sb append " GENERATED BY DEFAULT AS IDENTITY"
    }
  }

  override protected def createColumnDDLBuilder(c: NamedColumn[_]) = new DerbyColumnDDLBuilder(c)

  override protected def createIndex(idx: Index) = {
    if(idx.unique) {
      /* Create a UNIQUE CONSTRAINT (with an automatically generated backing
       * index) because Derby does not allow a FOREIGN KEY CONSTRAINT to
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

class DerbySequenceDDLBuilder[T](seq: Sequence[T], profile: DerbyDriver) extends BasicSequenceDDLBuilder(seq, profile) {
  import profile.sqlUtils._

  override def buildDDL: DDL = {
    import seq.integral._
    val increment = seq._increment.getOrElse(one)
    val desc = increment < zero
    val b = new StringBuilder append "CREATE SEQUENCE " append quoteIdentifier(seq.name)
    /* Set the START value explicitly because it defaults to the data type's
     * min/max value instead of the more conventional 1/-1. */
    b append " START WITH " append seq._start.getOrElse(if(desc) -1 else 1)
    seq._increment.foreach { b append " INCREMENT BY " append _ }
    seq._maxValue.foreach { b append " MAXVALUE " append _ }
    seq._minValue.foreach { b append " MINVALUE " append _ }
    /* Cycling is supported but does not conform to SQL:2008 semantics. Derby
     * cycles back to the START value instead of MINVALUE or MAXVALUE. No good
     * workaround available AFAICT. */
    if(seq._cycle) b append " CYCLE"
    new DDL {
      val createPhase1 = Iterable(b.toString)
      val createPhase2 = Nil
      val dropPhase1 = Nil
      val dropPhase2 = Iterable("DROP SEQUENCE " + quoteIdentifier(seq.name))
    }
  }
}
