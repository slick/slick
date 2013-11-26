package scala.slick.meta.codegen

import scala.slick.{meta => m}
import scala.slick.meta.ForeignKeyAction

/** Base implementation for a Source code String generator */
abstract class AbstractSourceCodeGenerator(model: m.Model)
                   extends AbstractGenerator[String](model)
                   with StringGeneratorHelpers{
  // virtual class pattern
  type Table <: TableDef
  def Table: m.Table => Table

  /** Generates code for the complete model (not wrapped in a package yet) */
  def code = {
    s"import scala.slick.meta.ForeignKeyAction" +
      "\n" +
      tables.map(_.code.mkString("\n")).mkString("\n\n")
  }

  // Code generators for the different meta model entities
  abstract class TableDef(meta: m.Table) extends super.TableDef(meta){
    // virtual class pattern
    type Column     <: ColumnDef
    type PrimaryKey <: PrimaryKeyDef
    type ForeignKey <: ForeignKeyDef
    type Index      <: IndexDef
    def Column    : m.Column     => Column
    def PrimaryKey: m.PrimaryKey => PrimaryKey
    def ForeignKey: m.ForeignKey => ForeignKey
    def Index     : m.Index      => Index

    def star = "def * = " + compound(columns.map(_.name)) + (if(mappingEnabled) s" <> (${factory}, ${extractor})" else "") // TODO: encode this check in the parent class
    
    def mappedType        = entityClassName
    def factory: String   = mappedType+".tupled"
    def extractor: String = mappedType+".unapply"
    
    def entityClassCode = s"""case class ${entityClassName}(${columns.map(c=>c.name+": "+c.tpe+(c.default.map("="+_).getOrElse(""))).mkString(", ")})"""

    def tableClassCode = s"""
class ${tableClassName}(tag: Tag) extends Table[${tpe}](tag,"${meta.name.table}"){
  ${indent(tableClassBody.map(_.mkString("\n")).mkString("\n\n"))}
}
    """.trim()

    def tableValueCode = s"""lazy val ${tableValueName} = TableQuery[${tableClassName}]"""

    class ColumnDef(meta: m.Column) extends super.ColumnDef(meta){
      final def primaryKeyColumnOption = "O.PrimaryKey"
      final def dbTypeColumnOption: String = s"""O.DBType("${dbTypeWithSize}")"""
      final def autoIncrementColumnOption = "O.AutoInc"
      final def defaultValueColumnOption = default.map("O.Default("+_+")")

      def default : Option[String] = {
        meta.default.collect{
          case Some(s:String) => "\""+s+"\""
          case Some(v:Int) => v
          case Some(v:Double) => v
          case None => "None"
        }.map(_.toString)
      }

      def code = s"""val ${name} = column[${tpe}]("${meta.name}"${options.map(", "+_).mkString("")})"""
    }

    class PrimaryKeyDef(meta: m.PrimaryKey) extends super.PrimaryKeyDef(meta){
      def code = s"""val $name = primaryKey("${meta.name}", ${compound(columns.map(_.name))})"""
    }

    class ForeignKeyDef(meta: m.ForeignKey) extends super.ForeignKeyDef(meta){
      def ruleString(action: ForeignKeyAction) = action match{
        case ForeignKeyAction.Cascade    => "ForeignKeyAction.Cascade"
        case ForeignKeyAction.Restrict   => "ForeignKeyAction.Restrict"
        case ForeignKeyAction.NoAction   => "ForeignKeyAction.NoAction"
        case ForeignKeyAction.SetNull    => "ForeignKeyAction.SetNull"
        case ForeignKeyAction.SetDefault => "ForeignKeyAction.SetDefault"
      }
      
      final def onUpdate: String = ruleString(meta.onUpdate)
      final def onDelete: String = ruleString(meta.onDelete)
      def code = s"""val $name = foreignKey("${meta.name}", ${compound(referencingColumns.map(_.name))}, ${referencedTable.tableValueName})(t => ${compound(referencedColumns.map(_.name).map("t."+_))}, onUpdate=${onUpdate}, onDelete=${onDelete})"""
    }

    class IndexDef(meta: m.Index) extends super.IndexDef(meta){
      def code = s"""val $name = index("${meta.name}", ${compound(columns.map(_.name))}${if(meta.unique) ", unique=true" else ""})"""
    }
  }
  // Default Scala tree generators
  /** Implements Column code generator using ColumnDef */
  trait DefaultColumn extends TableDef{
    type Column = ColumnDef
    def Column = new ColumnDef(_)
  }
  /** Implements PrimaryKey code generator using PrimaryKeyDef */
  trait DefaultPrimaryKey extends TableDef{
    type PrimaryKey = PrimaryKeyDef
    def PrimaryKey = new PrimaryKeyDef(_)
  }
  /** Implements ForeignKey code generator using ForeignKeyDef */
  trait DefaultForeignKey extends TableDef{
    type ForeignKey = ForeignKeyDef  
    def ForeignKey = new ForeignKeyDef(_)
  }
  /** Implements Index code generator using IndexDef */
  trait DefaultIndex extends TableDef{
    type Index = IndexDef  
    def Index = new IndexDef(_)
  }
}

trait StringGeneratorHelpers extends scala.slick.meta.codegen.GeneratorHelpers[String]{
  def docWithCode(comment: Option[String], code:String): String = comment.map("/** "+_+" */") ++ Seq(code) mkString "\n"
  def toOption(t: String) = s"Option[$t]"
  def compound(valuesOrTypes: Seq[String]): String = if (valuesOrTypes.size == 1) valuesOrTypes.head else valuesOrTypes.mkString("(",", ",")")
  def sqlTypeToScala(dbType: Int): String = sqlTypeToClass(dbType)
}
