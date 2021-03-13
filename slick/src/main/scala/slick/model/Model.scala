package slick.model
import slick.ast.ColumnOption

trait ModelOption[T]
trait TableOption[T]
trait PrimaryKeyOption[T]
trait ForeignKeyOption[T]
trait IndexOption[T]

/** Qualified name of a database table */
case class QualifiedName(table: String, schema: Option[String]=None, catalog: Option[String]=None){
  /** human readable String representation */
  def asString = catalog.map(_+".").getOrElse("") + 
                  schema.map(_+".").getOrElse("") + 
                  table
}

case class Table(
  name: QualifiedName,
  columns: Seq[Column],
  primaryKey: Option[PrimaryKey],
  foreignKeys: Seq[ForeignKey],
  indices: Seq[Index],
  options: Set[TableOption[_]] = Set()
){
  require( name.table != "", "name cannot be empty string" )
}

/** @param tpe qualified Scala type, e.g. java.sql.Date */
case class Column(
  name: String,
  table: QualifiedName,
  tpe: String,
  nullable: Boolean,
  options: Set[ColumnOption[_]] = Set()
){
  require( name != "", "name cannot be empty string" )
}

case class PrimaryKey(
  name: Option[String],
  table: QualifiedName,
  columns: Seq[Column],
  options: Set[PrimaryKeyOption[_]] = Set()
){
  require( !name.contains(""), "name cannot be empty string" )
}

case class ForeignKey(
  name: Option[String],
  referencingTable: QualifiedName,
  referencingColumns: Seq[Column],
  referencedTable: QualifiedName,
  referencedColumns: Seq[Column],
  onUpdate: ForeignKeyAction,
  onDelete: ForeignKeyAction,
  options: Set[ForeignKeyOption[_]] = Set()
){
  require( !name.contains(""), "name cannot be empty string" )
}


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
  unique: Boolean,
  options: Set[IndexOption[_]] = Set()
){
  require( !name.contains(""), "name cannot be empty string" )
}


/**
 * A container class for Slick's data model
 * The model can have circular references (e.g. a table has a foreign key which points back to the table).
 * The references are broken apart by using names instead of object references for back references.
 */
case class Model(
  tables: Seq[Table],
  options: Set[ModelOption[_]] = Set()
){
  lazy val tablesByName = tables.map(t => t.name->t).toMap
  /**
   * Verifies consistency of the model by checking for duplicate names and references to non-existing entities.
   * In case such things are found, throws an AssertionError.
   */
  def assertConsistency(): Unit = {
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
      assert(foreignKeys.count(_.name.isDefined) == foreignKeys.filter(_.name.isDefined).map(_.name).distinct.size, "duplicate foreign key names detected")
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
      assert(indices.count(_.name.isDefined) == indices.filter(_.name.isDefined).map(_.name).distinct.size, "duplicate index names detected")
      indices.foreach{ idx =>
        assert( tablesByName.isDefinedAt(idx.table), msg("table "+idx.table,"index "+idx) )
        idx.columns.foreach{ column =>
          assert( table.columns.contains(column), msg("column "+column,"index "+idx) )
        }
      }
    }
  }
}
