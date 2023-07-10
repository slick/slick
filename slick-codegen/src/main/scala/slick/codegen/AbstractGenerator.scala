package slick.codegen

import slick.model as m
import slick.ast.ColumnOption
import slick.model.ForeignKeyAction
import slick.relational.RelationalProfile
import slick.sql.SqlProfile

/**
 * Slick code generator providing the base structure and facilities.
 * It contains a subclass as a generator for Tables, which again contains
 * subclasses for Column, etc.
 * The implementation follows the virtual class pattern, which allows flexible
 * customization by overriding the inner classes (following the pattern).
 * @see http://lampwww.epfl.ch/~odersky/papers/ScalableComponent.html
 */
abstract class AbstractGenerator[Code,TermName,TypeName](model: m.Model)
  extends GeneratorHelpers[Code,TermName,TypeName]{ codegen =>
  model.assertConsistency()

  /** Enables DDL Generation. */
  val ddlEnabled = true
  /** Table code generators. */
  final lazy val tables: Seq[Table] = model.tables.map(Table).sortBy(_.TableClass.rawName.toLowerCase)
  /** Table code generators indexed by db table name. */
  final lazy val tablesByName: Map[m.QualifiedName,Table] = tables.map(t => t.model.name -> t).toMap

  // pulled out here to make this common use case simpler
  /** Maps database table name to Table class and value name
      @group Basic customization overrides */
  def tableName = (dbName: String) => dbName.toCamelCase
  /** Maps database table name to entity case class name
      @group Basic customization overrides */
  def entityName = (dbName: String) => dbName.toCamelCase+"Row"

  /** Table generator virtual class */
  type Table <: AbstractTableDef
  /** Table generator factory. Override for customization.
      @group Basic customization overrides */
  def Table: m.Table => Table
  /**
   * Code generator for table related code
   *
   * @group Basic customization overrides
   * @param model corresponding Slick meta model component
   */
  abstract case class AbstractTableDef(model: m.Table) { table =>
    /** Column code generators in the order they appear in the model. */
    final lazy val columnsPositional: IndexedSeq[Column] = model.columns.map(Column).toIndexedSeq
    /** Database column positions in the desired user-facing order. Currently just moves the positions of AutoInc columns to the end if autoIncLastAsOption is enabled. */
    lazy val desiredColumnOrder: Seq[Int] = {
      val withIndex = columnsPositional.zipWithIndex
      if(autoIncLast)
        // put auto inc column last
        (withIndex.filterNot( _._1.autoInc ) ++ withIndex.filter( _._1.autoInc )).map(_._2)
      else
        withIndex.map(_._2)
    }
    /** Column code generators in the desired user-facing order. */
    final lazy val columns: Seq[Column] = desiredColumnOrder.map(columnsPositional)
    /** Column code generators indexed by db column name */
    final lazy val columnsByName: Map[String,Column] = columns.map(c => c.model.name -> c).toMap
    /** Primary key code generator, if this table has one */
    final lazy val primaryKey: Option[PrimaryKey] = model.primaryKey.map(PrimaryKey)
    /** Foreign key code generators */
    final lazy val foreignKeys: Seq[ForeignKey] = model.foreignKeys.map(ForeignKey)
    /** Index code generators */
    final lazy val indices: Seq[Index] = model.indices.map(Index)
/* currently not needed
    /** All raw term names in this table (before escaping or disambiguating columns) */
    final lazy val allRawTermNames: Seq[String] = (definitions ++ TableClass.definitions.flatten).collect{ case d:TermDef => d}.map(_.rawName)
*/
    /** Definitions to be generated for this table
        @group Basic customization overrides */
    def definitions = Seq[AbstractDef]( EntityType, PlainSqlMapper, TableClass, TableValue )
    /** Generates the complete code for this table and its subordinate generators.
        @group Basic customization overrides */
    def code: Seq[Code] = definitions.flatMap(_.getEnabled).map(_.docWithCode)

    /** Creates a compound type from a given sequence of types.
     *  Uses HList if hlistEnabled else tuple.
     */
    def compoundType(types: Seq[Code]): Code
    /** Creates a compound value from a given sequence of values.
     *  Uses HList if hlistEnabled else tuple.
     */
    def compoundValue(values: Seq[Code]): Code
    /** If HList should be used as a compound type instead of tuples. Only if hugeClassEnabled is false.
        @group Basic customization now overrides */
    def hlistEnabled = !hugeClassEnabled && columns.size > 22
    /**
       Default is true, i.e. a case class will be generated even if column.size > 22.
       Override to false to get the code as before Slick 3.3, i.e. a HList based type will be generated instead.
       @group Basic customization now overrides */
    def hugeClassEnabled = true
    /** Indicates whether auto increment columns should be put last.
        Please set to !hlistEnabled for switching this on.
        @group Basic customization overrides */
    def autoIncLast = false
    /** Indicates if this table should be mapped using factory and extractor or not, in which case tuples are used. (Consider overriding EntityType.enabled instead, which affects this, too.) Disabled by default when using hlists.
        @group Basic customization overrides */
    def mappingEnabled = !hlistEnabled
    /** Indicates if table has more than 22 columns but still has to be mapped to a case class.
      */
    final def isMappedToHugeClass = hugeClassEnabled && mappingEnabled && EntityType.classEnabled && columns.size > 22
    /** Function that constructs an entity object from the unmapped values
        @group Basic customization overrides */
    def factory: Code

    /** Entity case class or type alias generator virtual class */
    type EntityType <: AbstractEntityTypeDef
    /** Entity case class or type alias generator factory. Override for customization.
        @group Basic customization overrides */
    def EntityType: EntityType
    /** Entity case class or type alias generator definition (Mapped case class holding a complete row of data of this table).
        @group Basic customization overrides */
    trait AbstractEntityTypeDef extends AbstractTypeDef {
      /** Column types */
      def types: Code = compoundType(columns.map(_.exposedType))
      /** Indicates whether a case class should be generated. Otherwise a type alias. */
      def classEnabled = mappingEnabled
      /** Indicates whether a generated case class should be final. */
      def caseClassFinal = false
      def doc =
        if(classEnabled){
          s"Entity class storing rows of table ${TableValue.name}\n" +
            columns.map(c => "@param "+c.name+" "+c.doc).mkString("\n")
        } else {
          s"Row type of table ${TableValue.name}\n"
        }
      def rawName: String = entityName(model.name.table)
    }

    /** Plain SQL GetResult mapper generator virtual class */
    type PlainSqlMapper <: AbstractPlainSqlMapperDef
    /** Plain SQL GetResult mapper generator factory. Override for customization.
        @group Basic customization overrides */
    def PlainSqlMapper: PlainSqlMapper
    /** Plain SQL GetResult mapper generator definition
        @group Basic customization overrides */
    trait AbstractPlainSqlMapperDef extends AbstractTermDef {
      def doc = s"GetResult implicit for fetching ${EntityType.name} objects using plain SQL queries"
      def rawName: String = "GetResult"+EntityType.rawName
    }

    /** Table class generator virtual class */
    type TableClass <: AbstractTableClassDef
    /** Table class generator factory. Override for customization.
        @group Basic customization overrides */
    def TableClass: TableClass
    /** Table class generator definition
        @group Basic customization overrides */
    trait AbstractTableClassDef extends AbstractTypeDef {
      /** The type of the elements this table yields. */
      def elementType: TypeName = EntityType.name
      /** The * projection that accumulates all columns and map them if mappingEnabled is true*/
      def star: Code
      /** Indicates whether a ? projection should be generated. */
      def optionEnabled = mappingEnabled && columns.exists(c => !c.model.nullable)
      /** The ? projection to produce an Option row. Useful for outer joins. */
      def option: Code
      /** Function that constructs an Option of an entity object from the unmapped Option values */
      def optionFactory: Code
      def doc = s"Table description of table ${model.name.table}. Objects of this class serve as prototypes for rows in queries." +
                { val collidingTerms = columns.map(_.rawName) intersect scalaKeywords
                  if(collidingTerms.nonEmpty) "\nNOTE: The following names collided with Scala keywords and were escaped: "+collidingTerms.mkString(", ") else "" }+
                { val collidingTerms = columns.map(_.rawName) intersect slickTableTermMembersNoArgs
                  if(collidingTerms.nonEmpty) "\nNOTE: The following names collided with Scala method names and were disambiguated: "+collidingTerms.mkString(", ") else "" }
      def rawName: String = tableName(model.name.table)
      def code: Code

      /** All definitions in this table class including disabled ones grouped into logical groups. */
      def definitions = {
        def OptionDef = new AbstractDef {
          def doc  = "Maps whole row to an option. Useful for outer joins."
          override def enabled = optionEnabled
          def code = option
          def rawName = ???
        }
        def StarDef = new AbstractDef {
          def doc  = ""
          def code = star
          def rawName = ???
        }

        Seq[Seq[AbstractDef]](
          Seq(StarDef,OptionDef), columns, primaryKey.toSeq, foreignKeys, indices
        )
      }
      /** Code for enabled definitions in this table class grouped into logical groups. */
      def body: Seq[Seq[Code]] = definitions.map(_.flatMap(_.getEnabled).map(_.docWithCode)).filter(_.nonEmpty)
    }

    /** Table value generator virtual class */
    type TableValue <: AbstractTableValueDef
    /** Table value generator factory. Override for customization.
        @group Basic customization overrides */
    def TableValue: TableValue
    /** Table value generator definition (generates a collection-like value representing this database table).
        @group Basic customization overrides */
    trait AbstractTableValueDef extends AbstractTermDef {
      def doc = s"Collection-like TableQuery object for table ${TableValue.name}"
      def rawName: String = tableName(model.name.table)
      def code: Code
    }

    /** Column generator virtual class */
    type Column  <: AbstractColumnDef
    /** Column generator factory. Override for customization.
        @group Basic customization overrides */
    def Column    : m.Column     => Column
    /**
     * Column related generator definition
     * @group Basic customization overrides
     * @param model corresponding Slick meta model component
     */
    abstract case class AbstractColumnDef(model: m.Column) extends AbstractTermDef {
      /**
       * Underlying Scala type of this column.
       * Override this to just affect the data type but preserve potential Option-wrapping.
       * @group Basic customization overrides
       */
      def rawType: Code = parseType(model.tpe)
      /**
       * Indicates whether the exposed type of this column should be wrapped in an Option.
       * Useful for autoInc and automatically created columns.
       * Set to autoInc to expose autoInc columns as Option.
       * @group Basic customization overrides
       */
      def asOption = autoIncLast && autoInc
      /** Possibly Option-wrapped Scala type of this column. @see rawType and @see exposedType */
      final def actualType: Code      = if(model.nullable) optionType(rawType) else rawType
      /** Option of actualType if fakeNullable else actualType. */
      final def exposedType: Code  = if(asOption) optionType(actualType) else actualType

      assert(!(model.nullable && asOption),s"Cannot enable 'fakeNullable' for a 'nullable' column. $model")

      /** Indicates whether this is an auto increment column */
      final def autoInc = model.options.contains(ColumnOption.AutoInc)
      /** Generates code for a columnOption */
      def columnOptionCode: ColumnOption[_] => Option[Code]
      /** Generates code for the ColumnOptions (DBType, AutoInc, etc.) */
      def options: Iterable[Code] = model.options.filter{
        case _: SqlProfile.ColumnOption.SqlType => dbType
        case _                                  => true
      }.flatMap(columnOptionCode(_).toSeq)
      /** Indicates if a (non-portable) DBType ColumnOption should be generated */
      def dbType: Boolean = false
      /** Returns a function, that maps a value to its literal representation as code */
      def defaultCode: Any => Code
      /** Generates a literal representation of the default value or None in case of an Option-typed autoInc column */
      def default: Option[Code] = model.options.collect{
        case RelationalProfile.ColumnOption.Default(value) => value
        case _ if asOption => None
      }.map(defaultCode).headOption

      def rawName: String = model.name.toCamelCase.uncapitalize
      def doc: String = "Database column "+model.name+" "+model.options.map(_.toString).mkString(", ")
    }

    /** Primary key generator virtual class */
    type PrimaryKey <: AbstractPrimaryKeyDef
    /** PrimaryKey generator factory. Override for customization.
        @group Basic customization overrides */
    def PrimaryKey: m.PrimaryKey => PrimaryKey
    /**
     * PrimaryKey related generator definition
     * (Currently only used for composite primary keys.)
     * @group Basic customization overrides
     * @param model corresponding Slick meta model component
     */
    abstract case class AbstractPrimaryKeyDef(model: m.PrimaryKey) extends AbstractTermDef {
      /** Columns code generators in correct order */
      final lazy val columns: Seq[Column] = model.columns.map(_.name).map(columnsByName)
      /** Name used in the db or a default name */
      def dbName = model.name.getOrElse(table.model.name.table+"_PK")
      def rawName = disambiguateTerm("pk")
      def doc = "Primary key of "+TableValue.name+s" (database name $dbName)"
    }

    private var _freshFkId = 0
    private def freshFkId = {
      _freshFkId+=1
      _freshFkId
    }
    /** Foreign key generator virtual class */
    type ForeignKey <: AbstractForeignKeyDef
    /** ForeignKey generator factory. Override for customization.
        @group Basic customization overrides */
    def ForeignKey: m.ForeignKey => ForeignKey
    /**
     * ForeignKey related generator definition
     * @group Basic customization overrides
     * @param model corresponding Slick meta model component
     */
    abstract case class AbstractForeignKeyDef(model: m.ForeignKey) extends AbstractTermDef {
      private val id = freshFkId
      /** Referencing Table code generator */
      final lazy val referencingTable = table
      /** Referencing columns code generators */
      final lazy val referencingColumns: Seq[Column] = model.referencingColumns.map(_.name).map(columnsByName)
      /** Referenced Table code generator */
      final lazy val referencedTable: Table = tablesByName(model.referencedTable)
      /** Referenced Columns code generators */
      final lazy val referencedColumns: Seq[AbstractTableDef#Column] =
        model.referencedColumns.map(_.name).map(referencedTable.columnsByName)
      /** Name used in the db or a default name */
      def dbName = model.name.getOrElse( referencedTable.model.name.table + "_FK_" + id )
      def actionCode(action: ForeignKeyAction): Code
      final def onUpdate: Code = actionCode(model.onUpdate)
      final def onDelete: Code = actionCode(model.onDelete)
      def rawName: String = disambiguateTerm({
        val fksToSameTable = foreignKeys.filter(_.referencedTable == referencedTable)
        require(
          fksToSameTable.count(_.model.name.isEmpty) <= 1,
          s"Found multiple unnamed foreign keys to same table, please manually provide names using overrides." +
            s" ${referencingTable.model.name.table} -> ${referencedTable.model.name.table}"
        )
        val baseName = referencedTable.TableClass.rawName.uncapitalize + "Fk"
        disambiguateTerm(if(fksToSameTable.size > 1) baseName + id else baseName)
      })
      def doc = s"Foreign key referencing ${referencedTable.TableValue.name} (database name $dbName)"
    }

    private var _freshIdxId = 0
    private def freshIdxId = {
      _freshIdxId+=1
      _freshIdxId
    }
    /** Index generator virtual class */
    type Index   <: AbstractIndexDef
    /** Index generator factory. Override for customization.
        @group Basic customization overrides */
    def Index     : m.Index      => Index
    /**
     * Index related generator definition
     * @group Basic customization overrides
     * @param model corresponding Slick meta model component
     */
    abstract case class AbstractIndexDef(model: m.Index) extends AbstractTermDef {
      private val id = freshIdxId
      /** Columns code generators */
      final lazy val columns: Seq[Column] = model.columns.map(_.name).map(columnsByName)
      /** Name used in the db or a default name */
      val dbName = model.name.getOrElse(table.model.name.table+"_INDEX_"+id)
      def rawName = disambiguateTerm("index"+id)
      def doc: String =
        (if(model.unique)"Uniqueness " else "")+
          "Index over "+columns.map(_.name).mkString("(",",",")")+s" (database name $dbName)"
    }

    /** Common interface for any kind of definition within the generated code */
    trait AbstractDef {
      /** Indicates whether this will be included in the generated code
        @group Basic customization overrides */
      def enabled = true
      /** Returns Some(this) if enabled else None */
      final def getEnabled = if(enabled) Some(this) else None
      /** Scala doc comment with code */
      def docWithCode: Code = codegen.docWithCode(doc, code)
      /** Scala doc comment
        @group Basic customization overrides */
      def doc: String
      /** Scala code
        @group Basic customization overrides */
      def code: Code
      /** Name as desired in Scala Code. (Allowed to collide with Scala keywords. Will be automatically escaped.)
        @group Basic customization overrides */
      def rawName: String
    }
    /** Common interface for definitions that define a term (val, def, ...) within the generated code */
    trait AbstractTermDef extends AbstractDef {
      override def docWithCode: Code = {
        val newDoc =
          doc +
            (if (scalaKeywords.contains(rawName))
              s"\nNOTE: The name was escaped because it collided with a Scala keyword."
            else
              "") +
            (if (slickTableTermMembersNoArgs.contains(rawName))
              s"\nNOTE: The name was disambiguated because it collided with Slick's method Table#$rawName."
            else
              "")
        codegen.docWithCode(newDoc, this.code)
      }
      /** Name (escaped if colliding with Scala keyword). */
      final def name: TermName = termName{
        if(slickTableTermMembersNoArgs.contains(rawName)){
          disambiguateTerm(rawName)
        } else rawName
      }
      /** Adds one or more X to the end of the given string to avoid collisions with column names. */
      def disambiguateTerm(name:String, postfix:String="X"): String =
        if((columns.map(_.rawName) ++ slickTableTermMembersNoArgs).contains(name)) disambiguateTerm(name+postfix)
        else name
    }
    /** Common interface for definitions that define a type (class, case class, ...) within the generated code */
    trait AbstractTypeDef extends AbstractDef {
      /** Name (escaped if colliding with Scala keyword). */
      final def name: TypeName = typeName( rawName )
      /** Inherited traits.
        @group Basic customization overrides */
      def parents: Seq[Code] = Seq()
    }
  }
}

/** Helper methods for code generation */
trait GeneratorHelpers[Code, TermName, TypeName] {
  def indent(code: String): String = {
    val lines = code.split("\n")
    lines.tail.foldLeft(lines.head) { (out, line) =>
      out + '\n' +
        (if (line.isEmpty) line else "  " + line)
    }
  }

  /** Assemble doc comment with scala code */
  def docWithCode(comment: String, code:Code): Code

  /** Words that are reserved keywords in Scala */
  val scalaKeywords = Seq("abstract","case","catch","class","def","do","else","extends","false","final","finally","for","forSome","if","implicit","import","lazy","match","new","null","object","override","package","private","protected","return","sealed","super","this","throw","trait","try","true","type","val","var","while","with","yield",":","=","=>","<-","<:","<%",">:","#","@")

  /** Existing term member names in Table[_] that do not take parameters */
  val slickTableTermMembersNoArgs =
    Seq(
      // Rep
      Seq("toNode"),
      // AbstractTable
      Seq("*","tableIdentitySymbol","create_*","foreignKeys","indexes","primaryKeys","schemaName","tableConstraints","tableName","tableNode","tableTag"),
      // Table
      Seq("O","tableIdentitySymbol","tableProvider"),
      // generated code
      Seq("_tableTag")
    ).flatten
/* currently disambiguated using overloading
  /** Existing term member names in Table[_] that take parameters */
  val slickTableTermMembersHasArgs =
    Seq(
      // Rep
      Seq("encodeRef"),
      // Table
      Seq("column"),
      // AbstractTable
      Seq("collectFieldSymbols","foreignKey","index","primaryKey")
    ).flatten
  /** Existing type member names in Table[_] */
  val slickTableTypeMembers = Seq("TableElementType")
*/

  /** Marks a String as a TermName (e.g. for escaping scala keywords) */
  protected def termName( name:String ): TermName
  /** Marks a String as a TypeName (e.g. for escaping scala keywords) */
  protected def typeName( name:String ): TypeName

  /** Wrap the given type into an Option type */
  def optionType(tpe: Code): Code

  /** Generates code for a qualified Scala type */
  def parseType(tpe: String): Code

  /** Slick code generator string extension methods. (Warning: Not unicode-safe, uses String#apply) */
  implicit class StringExtensions(val str: String){
    /** Lowercases the first (16 bit) character. (Warning: Not unicode-safe, uses String#apply) */
    final def uncapitalize: String = str(0).toString.toLowerCase + str.tail

    /**
     * Capitalizes the first (16 bit) character of each word separated by one or more '_'. Lower cases all other characters.
     * Removes one '_' from each sequence of one or more subsequent '_' (to avoid collision).
     * (Warning: Not unicode-safe, uses String#apply)
     */
    final def toCamelCase: String
      = str.toLowerCase
           .split("_")
           .map{ case "" => "_" case s => s } // avoid possible collisions caused by multiple '_'
           .map(_.capitalize)
           .mkString("")
  }
}
