package scala.slick.meta
import scala.slick.ast.ColumnOption

/** Qualified name of a database table */
case class QualifiedName(table: String, schema: Option[String]=None, catalog: Option[String]=None){
  override def toString = catalog.map(_ + ".").getOrElse("") + schema.map(_ + ".").getOrElse("") + table
}

/** A database table including its related entities */
case class Table(
  name: QualifiedName,
  columns: Seq[Column],
  primaryKey: Option[PrimaryKey],
  foreignKeys: Seq[ForeignKey],
  indices: Seq[Index]
)

case class Column(
  name: String,
  table: QualifiedName,
  jdbcType: Int,
  nullable: Boolean,
  options: Set[ColumnOption[_]]
)

case class PrimaryKey(
  name: Option[String],
  table: QualifiedName,
  columns: Seq[Column]
)

case class ForeignKey(
  name: Option[String],
  referencingTable: QualifiedName,
  referencingColumns: Seq[Column],
  referencedTable: QualifiedName,
  referencedColumns: Seq[Column],
  onUpdate: ForeignKeyAction,
  onDelete: ForeignKeyAction
)

sealed abstract class ForeignKeyAction(val action: String)

object ForeignKeyAction {
  case object Cascade    extends ForeignKeyAction("CASCADE")
  case object Restrict   extends ForeignKeyAction("RESTRICT")
  case object NoAction   extends ForeignKeyAction("NO ACTION")
  case object SetNull    extends ForeignKeyAction("SET NULL")
  case object SetDefault extends ForeignKeyAction("SET DEFAULT")
}

case class Index(
  name: Option[String],
  table: QualifiedName,
  columns: Seq[Column],
  unique: Boolean
)

/**
 * A container class for Slick's meta model
 * The model can have cirular dependencies (e.g. a table has a foreign key which points back to the table).
 * The circular dependencies are broken apart by using names instead of object references for back references.
 */
case class Model(
  tables: Seq[Table]
){
  lazy val tablesByName = tables.map(t => t.name->t).toMap
  /**
   * Verifies consistency of the model by checking for duplicate names and references to non-existing entities.
   * In case such things are found, throws an AssertionError. 
   */
  def assertConsistency{
    assert(tables.size == tables.map(_.name).distinct.size, "duplicate tables names detected")
    tables.foreach{ table =>
      import table._
      assert(columns.size == columns.map(_.name).distinct.size, "duplicate column names detected")
      def msg( what: String, where: String ) = s"Reference to non-existent $what in $where of table $table."
      primaryKey.foreach{ pk =>
        assert( tablesByName.isDefinedAt(pk.table), msg("table "+pk.table,"primary key "+pk) )
        pk.columns.foreach{ column =>
          assert( table.columns.contains(column), msg("column "+column,"primary key "+pk) )
        }
      }
      assert(foreignKeys.filter(_.name!="").size == foreignKeys.filter(_.name!="").map(_.name).distinct.size, "duplicate foreign key names detected")
      foreignKeys.foreach{ fk =>
        assert( tablesByName.isDefinedAt(fk.referencedTable), msg("table "+fk.referencedTable,"foreign key "+fk) )
        assert( tablesByName.isDefinedAt(fk.referencingTable), msg("table "+fk.referencingTable,"foreign key "+fk) )
        val pkTable = tablesByName(fk.referencedTable)
        val fkTable = tablesByName(fk.referencingTable)
        assert( table == fkTable, "Referencing table $fkTable does not match table $table the foreign key $fk is contained in." )
        fk.referencedColumns.foreach{ pkColumn =>
          assert( pkTable.columns.contains(pkColumn), msg("column "+pkColumn+" of table "+pkTable,"foreign key "+fk) )
        }
        fk.referencingColumns.foreach{ fkColumn =>
          assert( fkTable.columns.contains(fkColumn), msg("column "+fkColumn+" of table "+fkTable,"foreign key "+fk) )
        }
      }
      assert(indices.filter(_.name!="").size == indices.filter(_.name!="").map(_.name).distinct.size, "duplicate index names detected")
      indices.foreach{ idx =>
        assert( tablesByName.isDefinedAt(idx.table), msg("table "+idx.table,"index "+idx) )
        idx.columns.foreach{ column =>
          assert( table.columns.contains(column), msg("column "+column,"index "+idx) )
        }
      }
    }
  }
}
