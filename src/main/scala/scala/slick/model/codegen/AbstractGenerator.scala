package scala.slick.model.codegen

import scala.slick.{model => m}
import scala.slick.model.ForeignKeyAction
import scala.slick.ast.ColumnOption

/** Workaround for 2.0.x binary compatibility */
private[codegen] object GlobalVariables{
  var compoundTypeEnabled = false
}

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
  model.assertConsistency

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
  type Table <: TableDef
  /** Table generator factory. Override for customization.
      @group Basic customization overrides */
  def Table: m.Table => Table
  /**
   * Code generator for table related code
   * @group Basic customization overrides
   * @param model corresponding Slick meta model component
  */
  abstract case class TableDef(val model: m.Table){
    table =>
    /** Column code generators in the order they appear in the model. */
    final lazy val columnsPositional: IndexedSeq[Column] = model.columns.map(Column).toIndexedSeq
    /** Database column positions in the desired user-facing order. Currently just moves the positions of AutoInc columns to the end if autoIncLastAsOption is enabled. */
    final lazy val desiredColumnOrder: Seq[Int] = {
      val withIndex = columnsPositional.zipWithIndex
      if(autoIncLastAsOption)
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
    def definitions = Seq[Def]( EntityType, PlainSqlMapper, TableClass, TableValue )
    /** Generates the complete code for this table and its subordinate generators.
        @group Basic customization overrides */
    def code: Seq[Code] = definitions.flatMap(_.getEnabled).map(_.docWithCode)

    /** Creates a compound type or value from a given sequence of types or values.
     *  Uses HList if hlistEnabled else tuple.
     */
    def compound(valuesOrTypes: Seq[Code]): Code
    /** If HList should be used as a compound type instead of tuples. Default to true for > 22 columns.
        @group Basic customization overrides */
    def hlistEnabled = columns.size > 22
    /** Indicates whether auto increment columns should be put last and made an Option with a None default.
        Please set to !hlistEnabled for switching this on.
        @group Basic customization overrides */
    def autoIncLastAsOption = false
    /** Indicates if this table should be mapped using factory and extractor or not, in which case tuples are used. (Consider overriding EntityType.enabled instead, which affects this, too.) Disabled by default when using hlists.
        @group Basic customization overrides */
    def mappingEnabled = !hlistEnabled
    /** Function that constructs an entity object from the unmapped values
        @group Basic customization overrides */
    def factory: Code
    /** Function that extracts the unmapped values from an entity object
        @group Basic customization overrides */
    def extractor: Code

    /** Entity case class or type alias generator virtual class */
    type EntityType <: EntityTypeDef
    /** Entity case class or type alias generator factory. Override for customization.
        @group Basic customization overrides */
    def EntityType: EntityType
    /** Entity case class or type alias generator definition (Mapped case class holding a complete row of data of this table).
        @group Basic customization overrides */
    trait EntityTypeDef extends TypeDef{
      /** Column types */
      def types: Code = {
        GlobalVariables.compoundTypeEnabled = true
        val code = compound(columns.map(_.exposedType))
        GlobalVariables.compoundTypeEnabled = false
        code
      }
      /** Indicated whether a case class should be generated. Otherwise a type alias. */
      def classEnabled = mappingEnabled
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
    type PlainSqlMapper <: PlainSqlMapperDef
    /** Plain SQL GetResult mapper generator factory. Override for customization.
        @group Basic customization overrides */
    def PlainSqlMapper: PlainSqlMapper
    /** Plain SQL GetResult mapper generator definition 
        @group Basic customization overrides */
    trait PlainSqlMapperDef extends TermDef{
      def doc = s"GetResult implicit for fetching ${EntityType.name} objects using plain SQL queries"
      def rawName: String = "GetResult"+EntityType.rawName
    }

    /** Table class generator virtual class */
    type TableClass <: TableClassDef
    /** Table class generator factory. Override for customization.
        @group Basic customization overrides */
    def TableClass: TableClass
    /** Table class generator definition 
        @group Basic customization overrides */
    trait TableClassDef extends TypeDef{
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
        def OptionDef = new Def{
          def doc  = "Maps whole row to an option. Useful for outer joins."
          override def enabled = optionEnabled
          def code = option
          def rawName = ???
        }
        def StarDef = new Def{
          def doc  = ""
          def code = star
          def rawName = ???
        }

        Seq[Seq[Def]](
          Seq(StarDef,OptionDef), columns, primaryKey.toSeq, foreignKeys, indices
        )
      }
      /** Code for enabled definitions in this table class grouped into logical groups. */
      def body: Seq[Seq[Code]] = definitions.map(_.flatMap(_.getEnabled).map(_.docWithCode)).filter(_.nonEmpty)
    }

    /** Table value generator virtual class */
    type TableValue <: TableValueDef
    /** Table value generator factory. Override for customization.
        @group Basic customization overrides */
    def TableValue: TableValue
    /** Table value generator definition (generates a collection-like value representing this database table). 
        @group Basic customization overrides */
    trait TableValueDef extends TermDef{
      def doc = s"Collection-like TableQuery object for table ${TableValue.name}"
      def rawName: String = tableName(model.name.table)
      def code: Code
    }

    /** Column generator virtual class */
    type Column     <: ColumnDef
    /** Column generator factory. Override for customization.
        @group Basic customization overrides */
    def Column    : m.Column     => Column
    /**
     * Column related generator definition
     * @group Basic customization overrides 
     * @param model corresponding Slick meta model component
     */
    abstract case class ColumnDef(val model: m.Column) extends TermDef{
      /**
       * Underlying Scala type of this column.
       * Override this to just affect the data type but preserve potential Option-wrapping.
       * Override GeneratorHelpers#mapJdbcTypeString for generic adjustments.
       * @group Basic customization overrides
       */
      def rawType: Code = parseType(model.tpe)
      /** Possibly Option-wrapped Scala type of this column. @see rawType and @see exposedType */
      final def actualType: Code      = if(model.nullable) optionType(rawType) else rawType
      /** Option of actualType if fakeNullable else actualType. Useful to expose autoInc columns as nullable. */
      final def exposedType: Code  = if(fakeNullable) optionType(actualType) else actualType
      /** Indicates whether this column should be user facing as a nullable column with default None even though it is not. Useful for autoInc columns. */
      final def fakeNullable = autoIncLastAsOption && autoInc

      assert(!(model.nullable && fakeNullable),s"Cannot enable 'fakeNullable' for a 'nullable' column. $model")

      /** Indicates whether this is an auto increment column */
      final def autoInc = model.options.contains(ColumnOption.AutoInc)
      /** Generates code for a columnOption */
      def columnOptionCode: ColumnOption[_] => Option[Code]
      /** Generates code for the ColumnOptions (DBType, AutoInc, etc.) */
      def options: Iterable[Code] = model.options.flatMap(columnOptionCode(_).toSeq)
      /** Returns a function, that maps a value to its literal representation as code */
      def defaultCode: Any => Code
      /** Generates a literal represenation of the default value or None in case of an Option-typed autoinc column */
      def default: Option[Code] = model.options.collect{
        case ColumnOption.Default(value) => value
        case _ if fakeNullable => None
      }.map(defaultCode).headOption

      def rawName: String = model.name.toCamelCase.uncapitalize
      def doc: String = "Database column "+model.name+" "+model.options.map(_.toString).mkString(", ")
    }

    /** Primary key generator virtual class */
    type PrimaryKey <: PrimaryKeyDef
    /** PrimaryKey generator factory. Override for customization.
        @group Basic customization overrides */
    def PrimaryKey: m.PrimaryKey => PrimaryKey
    /**
     * PrimaryKey related generator definition
     * (Currently only used for composite primary keys.)
     * @group Basic customization overrides 
     * @param model corresponding Slick meta model component
     */
    abstract case class PrimaryKeyDef(val model: m.PrimaryKey) extends TermDef{
      /** Columns code generators in correct order */
      final lazy val columns: Seq[Column] = model.columns.map(_.name).map(columnsByName)
      /** Name used in the db or a default name */
      def dbName = model.name.getOrElse(table.model.name.table+"_PK")
      def rawName = disambiguateTerm("pk")
      def doc = "Primary key of "+TableValue.name+s" (database name ${dbName})"
    }

    private var _freshFkId = 0
    private def freshFkId = {
      _freshFkId+=1
      _freshFkId
    }
    /** Foreign key generator virtual class */
    type ForeignKey <: ForeignKeyDef
    /** ForeignKey generator factory. Override for customization.
        @group Basic customization overrides */
    def ForeignKey: m.ForeignKey => ForeignKey
    /**
     * ForeignKey related generator definition
     * @group Basic customization overrides 
     * @param model corresponding Slick meta model component
     */
    abstract case class ForeignKeyDef(val model: m.ForeignKey) extends TermDef{
      private val id = freshFkId
      /** Referencing Table code generator */
      final lazy val referencingTable = table
      /** Referencing columns code generators */
      final lazy val referencingColumns: Seq[Column] = model.referencingColumns.map(_.name).map(columnsByName)
      /** Referenced Table code generator */
      final lazy val referencedTable: Table = tablesByName(model.referencedTable)
      /** Referenced Columns code generators */
      final lazy val referencedColumns: Seq[TableDef#Column] = model.referencedColumns.map(_.name).map(referencedTable.columnsByName)
      /** Name used in the db or a default name */
      def dbName = model.name.getOrElse( referencedTable.model.name.table + "_FK_" + id )
      def actionCode(action: ForeignKeyAction): Code
      final def onUpdate: Code = actionCode(model.onUpdate)
      final def onDelete: Code = actionCode(model.onDelete)
      def rawName: String = disambiguateTerm({
        val fksToSameTable = foreignKeys.filter(_.referencedTable == referencedTable)
        require(
          fksToSameTable.filter(_.model.name.isEmpty).size <= 1,
          s"Found multiple unnamed foreign keys to same table, please manually provide names using overrides. ${referencingTable.model.name.table} -> ${referencedTable.model.name.table}"
        )
        val baseName = referencedTable.TableClass.rawName.uncapitalize + "Fk"
        disambiguateTerm(if(fksToSameTable.size > 1) baseName + id else baseName)
      })
      def doc = s"Foreign key referencing ${referencedTable.TableValue.name} (database name ${dbName})"
    }

    private var _freshIdxId = 0
    private def freshIdxId = {
      _freshIdxId+=1
      _freshIdxId
    }
    /** Index generator virtual class */
    type Index      <: IndexDef
    /** Index generator factory. Override for customization.
        @group Basic customization overrides */
    def Index     : m.Index      => Index
    /**
     * Index related generator definition
     * @group Basic customization overrides 
     * @param model corresponding Slick meta model component
     */
    abstract case class IndexDef(val model: m.Index) extends TermDef{
      private val id = freshIdxId
      /** Columns code generators */
      final lazy val columns: Seq[Column] = model.columns.map(_.name).map(columnsByName)
      /** Name used in the db or a default name */
      val dbName = model.name.getOrElse(table.model.name.table+"_INDEX_"+id)
      def rawName = disambiguateTerm("index"+id)
      def doc: String = 
        (if(model.unique)"Uniqueness " else "")+
        "Index over "+columns.map(_.name).mkString("(",",",")")+s" (database name ${dbName})"
    }

    /** Common interface for any kind of definition within the generated code */
    trait Def{
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
    trait TermDef extends Def{
      override def docWithCode: Code = {
        val newdoc = doc +
          (if(scalaKeywords.contains(rawName)) s"\nNOTE: The name was escaped because it collided with a Scala keyword." else "")+
          (if(slickTableTermMembersNoArgs.contains(rawName)) s"\nNOTE: The name was disambiguated because it collided with Slick's method Table#$rawName." else "")
        codegen.docWithCode(newdoc, code)
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
    trait TypeDef extends Def{
      /** Name (escaped if colliding with Scala keyword). */
      final def name: TypeName = typeName( rawName )
      /** Inherited traits.
        @group Basic customization overrides */
      def parents: Seq[Code] = Seq()
    }
  }
}

/** Helper methods for code generation */
trait GeneratorHelpers[Code,TermName,TypeName]{
  def indent(code: String): String = code.split("\n").mkString("\n"+"  ")

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
      Seq("O","tableIdentitySymbol","tableProvider")
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
