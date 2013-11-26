package scala.slick.meta.codegen

import scala.slick.{meta => m}
import scala.slick.lifted.ForeignKeyAction
import scala.slick.util.StringExtensionMethods

/**
 * Slick code generator providing the base structure and facilities.
 * It contains a subclass as a generator for Tables, which again contains
 * subclasses for Column, etc.
 * The implementation follows the virtual class pattern, which allows flexible
 * customization by overriding the inner classes (following the pattern).
 * @see http://lampwww.epfl.ch/~odersky/papers/ScalableComponent.html
*/
abstract class AbstractGenerator[Code](model: m.Model)
                   extends GeneratorHelpers[Code]{
  model.assertConsistency

  // virtual class pattern
  /** Table code generator "virtual" class. */
  type Table <: TableDef
  /** Creates a Table code generator. Override for customization. */
  def Table: m.Table => Table

  /** Table code generators. */
  final lazy val tables: Seq[Table] = model.tables.map(Table)
  /** Table code generators indexed by db table name. */
  final lazy val tablesByName: Map[m.QualifiedName,Table] = tables.map(t => t.meta.name -> t).toMap

  // pulled out here to make this common use case simpler
  /** Maps database table name to Table class and value name */
  def tableName = (dbName: String) => dbName.toCamelCase
  /** Maps database table name to entity case class name */
  def entityName = (dbName: String) => dbName.toCamelCase+"Row"

  // -----------------------------------------------------
  // Code generators for the different meta model entities
  /**
   * Code generator for table related code
   * @param meta Jdbc meta data
  */
  abstract case class TableDef(val meta: m.Table){
    table =>

    // virtual class pattern
    /** Column code generator */
    type Column     <: ColumnDef
    /** Primary key code generator */
    type PrimaryKey <: PrimaryKeyDef
    /** Foreign key code generator */
    type ForeignKey <: ForeignKeyDef
    /** Index code generator */
    type Index      <: IndexDef
    /** Creates a column      code generator. Override for customization. */
    def Column    : m.Column     => Column
    /** Creates a primary key code generator. Override for customization. */
    def PrimaryKey: m.PrimaryKey => PrimaryKey
    /** Creates a foreign key code generator. Override for customization. */
    def ForeignKey: m.ForeignKey => ForeignKey
    /** Creates an index      code generator. Override for customization. */
    def Index     : m.Index      => Index

    // component generators
    /** Column code generators. */
    final lazy val columns: Seq[Column] = meta.columns.map(Column)
    /** Column code generators indexed by db column name */
    final lazy val columnsByName: Map[String,Column] = columns.map(c => c.meta.name -> c).toMap
    /** Primary key code generator, if this table has one */
    final lazy val primaryKey: Option[PrimaryKey] = meta.primaryKey.map(PrimaryKey)
    /** Foreign key code generators */
    final lazy val foreignKeys: Seq[ForeignKey] = meta.foreignKeys.map(ForeignKey)
    /** Index code generators */
    final lazy val indices: Seq[Index] = meta.indices.map(Index)

    /** Generates the complete code for this table and its subordinate generators. */
    def code: Seq[Code] = {
      (
        if(entityClassEnabled)
          Seq(docWithCode(entityClassDoc,entityClassCode))
        else
          Seq()
      ) ++ Seq(
        docWithCode(tableClassDoc,tableClassCode),
        docWithCode(tableValueDoc,tableValueCode)
      )
    }

    // * projection and types
    /** The * projection that accumulates all columns and map them if mappingEnabled is true*/
    def star: Code
    /** Type of the * projection in case it mappingEnabled is false. */
    def types: Code = compound(columns.map(_.tpe))
    /** The type of the elements this table yields. */
    final def tpe: Code = if(mappingEnabled) mappedType else types

    // mapping
    /** Indicates if this table should be mapped using <> to a factory and extractor or not. (Consider overriding entityClassEnabled instead, which affects this, too.) */
    def mappingEnabled = entityClassEnabled
    /** The type to which elements of this table are mapped (in case mappingEnabled is true). */
    def mappedType: Code
    /** Function that constructs an entity object from the unmapped values */
    def factory: Code
    /** Function that extracts the unmapped values from an entity object */
    def extractor: Code

    // entity class (mapped case class)
    /** Indicates if an entity case class should be generated for this table. (This also set mappingEnabled to false unless you override it.) */
    def entityClassEnabled = true
    /** Scala doc for entity case class */
    def entityClassDoc: Option[String] = Some(s"Entity class storing rows of table $tableValueName")
    /** Name used for entity case class */
    def entityClassName: String = entityName(meta.name.table)
    /** Generates the entity case class (holding a complete row of data of this table).*/
    def entityClassCode: Code

    // Table class
    /** Scala doc for the Table class */
    def tableClassDoc: Option[String] = Some(s"Table description of table ${meta.name.table}. Objects of this class serves as prototypes for rows in queries.")
    /** Name for the Table class */
    def tableClassName: String = tableName(meta.name.table)
    /** Generates the Table class code. */
    def tableClassCode: Code
    /** Generates the body of the Table class as individual statements grouped into logical groups. */
    final def tableClassBody: Seq[Seq[Code]] = Seq(
      Seq(star),
      columns    .map(x => docWithCode(x.doc,x.code)),
      // H2 apparently needs primary key and autoinc to be specified together, so we place single primary keys as column options
      primaryKey.filter(_.columns.size > 1).map(x => docWithCode(x.doc,x.code)).toSeq,
      foreignKeys.map(x => docWithCode(x.doc,x.code)),
      indices    .map(x => docWithCode(x.doc,x.code))
    )

    // Table value (TableQuery)
    /** Scala doc for the Table/TableQuery value */
    def tableValueDoc: Option[String] = Some(s"Collection-like TableQuery object for table $tableValueName")
    /** Name used for the Table/TableQuery value */
    def tableValueName: String = tableName(meta.name.table)
    /** Generates the definition of the Table/TableQuery value (a collection-like value representing this database table). */
    def tableValueCode: Code

    // generator classes
    /**
     * Code generator for column related code.
     * @param meta Jdbc meta data
     */
    abstract case class ColumnDef(val meta: m.Column){
      /**
       * Underlying Scala type of this column.
       * Override this to just affect the data type but preserve potential Option-wrapping.
       * Override tpe for taking control of Option.wrapping.
       * Override GeneratorHelpers#sqlTypeToClass for generic adjustments.
       */
      def rawType: Code = sqlTypeToScala(meta.jdbcType)
      /** Possibly Option-wrapped Scala type of this column. @see rawType */
      def tpe: Code = if(meta.nullable) toOption(rawType) else rawType

      /** ColumnOptions */
      final def options: Iterable[Code] = (
        Seq(dbTypeColumnOption) ++
          (if(meta.autoInc) Some(autoIncrementColumnOption) else None) ++
          defaultValueColumnOption ++
          // H2 apparently needs primary key and autoinc to be specified together, so we place single column primary keys as column options.
          primaryKey.filter(_.columns.size==1/* && meta.autoInc*/).filter(_.columns.head.meta.name == meta.name).map(_ => primaryKeyColumnOption)
      )
      /** Generates code for a PrimaryKey column option. */
      def primaryKeyColumnOption: Code
      /** Generates code for an AutoInc column option. */
      def autoIncrementColumnOption: Code
      /** Generates code for a DefaultValue column option (if this table has a default value). */
      def defaultValueColumnOption: Option[Code]
      /** Generates literal Scala code representation of the default value (if this table has one). */
      def default: Option[Code]
      /** Column option representing the db type. */
      def dbTypeColumnOption: Code

      /** Name for the column definition used in Scala code */
      def name: String = meta.name.toCamelCase.uncapitalize
      /** Scala doc comment for the column definition */
      def doc: Option[String] = Some({
        def dbTypeComment = ": "+meta.dbType
        s"""Database column ${meta.name}$dbTypeComment${if(meta.autoInc) ", AutoInc" else ""}"""
      })
      /** Scala code defining the column */
      def code: Code
    }

    /**
     * Code generator for primary key related code.
     * (Currently only used for composite primary keys.)
     * @param meta Jdbc meta data
     */
    abstract case class PrimaryKeyDef(val meta: m.PrimaryKey){
      /** Columns code generators in correct order */
      final lazy val columns: Seq[Column] = meta.columns.map(_.name).map(table.columnsByName)
      /** Name for the primary key definition used in Scala code */
      def name = "pk"+meta.name.toCamelCase
      /** Scala doc comment for the definition */
      def doc: Option[String] = Some(s"Primary key of ${table.tableValueName}")
      /** Scala code defining this primary key */
      def code: Code
    }

    /**
     * Code generator for foreign key related code.
     * @param meta Jdbc meta data
     */
    abstract case class ForeignKeyDef(val meta: m.ForeignKey){
      /** Referencing columns code generators */
      final lazy val referencingColumns: Seq[Column] = meta.referencingColumns.map(_.name).map(columnsByName)
      /** Referenced Table code generator */
      final lazy val referencedTable: Table = tablesByName(meta.referencedTable)
      /** Referenced Columns code generators */
      final lazy val referencedColumns: Seq[TableDef#Column] = meta.referencedColumns.map(_.name).map(referencedTable.columnsByName)
      /** Generates the ForeignKeyAction code for the ON UPDATE behavior rule. */
      def onUpdate: Code
      /** Generates the ForeignKeyAction code for the ON Delete behavior rule. */
      def onDelete: Code
      /** Name for the foreign key definition used in Scala code. (Default: if no name conflict, name of referenced table, else database name.) */
      def name: String = {
        val preferredName = referencedTable.tableValueName.uncapitalize
        if(
          // multiple foreign keys to the same table
          table.foreignKeys.exists(_.referencedTable == referencedTable)
          // column name conflicts with referenced table name
          || table.columns.exists(_.name == preferredName)
        )
          meta.name.toCamelCase.uncapitalize
        else
          preferredName
      }
      /** Scala doc comment for the definition */
      def doc: Option[String] = Some(s"Foreign key ${meta.name} referencing ${referencedTable.tableValueName}")
      /** Scala code defining this foreign key */
      def code: Code
    }

    /**
     * Code generator for index related code
     * @param meta Jdbc meta data
     */
    abstract case class IndexDef(val meta: m.Index){
      /** Columns code generators */
      final lazy val columns: Seq[Column] = meta.columns.map(_.name).map(table.columnsByName)
      /** The name used in Scala code */
      def name = "index"+meta.name.toCamelCase
      /** Name for the index definition used in Scala code */
      def doc: Option[String] = Some(
        (if(meta.unique)"Uniqueness " else "")+
        s"""Index over ${columns.map(_.name).mkString("(",",",")")}"""
      )
      /** Scala code defining this index */
      def code: Code
    }
  }
}

/** Helper methods for code generation */
trait GeneratorHelpers[Code]{
  /** Assemble doc comment with scala code */
  def docWithCode(comment: Option[String], code:Code): Code

  /** Indents all but the first line of the given string */
  def indent(code: String): String = code.split("\n").mkString("\n"+"  ")

  /** Wrap the given type into an Option type */
  def toOption(tpe: Code): Code

  /**
   * Creates a compound type or value from a given sequence of types or values.
   * Defaults to a tuple. Can be used to switch to HLists.
   */
  def compound(valuesOrTypes: Seq[Code]): Code

  /** Generates code for the Scala type corresponding to the given java.sql.Types type. (Uses sqlTypeToClass.) */
  def sqlTypeToScala(dbType: Int): Code

  /** Converts from java.sql.Types to the corresponding Java class name (with fully qualified path). */
  def sqlTypeToClass(dbType: Int): String = {
    import java.sql.Types._
    dbType match {
      case BIT | BOOLEAN => "Boolean"
      case TINYINT => "Byte"
      case SMALLINT => "Short"
      case INTEGER => "Int"
      case BIGINT => "Long" //"java.math.BigInteger"
      case FLOAT => "Float"
      case REAL | DOUBLE => "Double"
      case NUMERIC | DECIMAL => "java.math.BigDecimal"
      case CHAR | VARCHAR | LONGVARCHAR => "String"
      case DATE => "java.sql.Date"
      case TIME => "java.sql.Time"
      case TIMESTAMP => "java.sql.Timestamp"
      case BINARY | VARBINARY | LONGVARBINARY | BLOB => "java.sql.Blob"
      case NULL => "Null"
      case CLOB => "java.sql.Clob"
      case _ => "AnyRef"
    }
  }
}
