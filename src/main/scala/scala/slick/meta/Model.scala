package scala.slick.meta
import scala.slick.lifted.ForeignKeyAction
import scala.slick.ast.ColumnOption
import scala.slick.ast.ColumnOption._

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

/** Workaround to have helper methods in Columns's secondary constructor */
private object ColumnHelpers{
  val Type =  "(a-zA-Z_)*(\\(0-9\\))?.*".r
  val extractTypeNameAndSize: PartialFunction[ColumnOption[_],Option[(String,Option[Int])]] = {
    case DBType(Type(typeName, size)) => Some((typeName, if(size != "") Some(size.toInt) else None))
  }
  val extractDefault: PartialFunction[ColumnOption[_],Option[Any]] = {
    case Default(defaultValue) => Some(defaultValue)
    case _ => None
  }
}
import ColumnHelpers._
case class Column(
  name: String,
  table: QualifiedName,
  tpe: Int,
  typeName: String,
  size: Option[Int],
  nullable: Boolean,
  autoInc: Boolean,
  default: Option[Option[Any]]
){
  /** Alternative constructor consuming ColumnOptions (untested at this point). */
  def this(name: String, table: QualifiedName, columnOptions: Seq[ColumnOption[_]], tpe: Int) = {
    this(
      name,
      table,
      tpe,
      typeName = columnOptions.collect(extractTypeNameAndSize).head.get._1,
      size = columnOptions.collect(extractTypeNameAndSize).head.get._2,
      nullable = columnOptions.contains(Nullable),
      autoInc = columnOptions.contains(AutoInc),
      default = columnOptions.collect(extractDefault).headOption
    )
  }
  def columnOptions: Set[ColumnOption[_]] = {
    Set(
      if(nullable) Nullable else NotNull,
      DBType( typeName + size.map("("+_+")").getOrElse("") )
    ) ++ (if(autoInc) Some(AutoInc) else None) ++ default.map(Default(_))
  }
}

case class PrimaryKey(
  name: String,
  table: QualifiedName,
  columns: Seq[Column]
)

case class ForeignKey(
  name: String,
  referencingTable: QualifiedName,
  referencingColumns: Seq[Column],
  referencedTable: QualifiedName,
  referencedColumns: Seq[Column],
  onUpdate: ForeignKeyAction,
  onDelete: ForeignKeyAction
)

case class Index(
  name: String,
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