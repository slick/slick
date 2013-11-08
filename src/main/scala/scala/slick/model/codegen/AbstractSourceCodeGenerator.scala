package scala.slick.model.codegen

import scala.slick.{model => m}
import scala.slick.model.ForeignKeyAction
import scala.slick.ast.ColumnOption
import scala.slick.SlickException

/** Base implementation for a Source code String generator */
abstract class AbstractSourceCodeGenerator(model: m.Model)
                   extends AbstractGenerator[String,String,String](model)
                   with StringGeneratorHelpers{
  /** Generates code for the complete model (not wrapped in a package yet)
      @group Basic customization overrides */
  def code = {
    "import scala.slick.model.ForeignKeyAction\n" +
    ( if(tables.exists(_.hlistEnabled)){
        "import scala.slick.collection.heterogenous._\n"+
        "import scala.slick.collection.heterogenous.syntax._\n"
      } else ""
    ) +
    ( if(tables.exists(_.PlainSqlMapper.enabled)){
        "import scala.slick.jdbc.{GetResult => GR}\n"+
        "// NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.\n"
      } else ""
    ) + "\n" +
    tables.map(_.code.mkString("\n")).mkString("\n\n")
  }

  private def tuple(i: Int) = termName(s"_${i+1}")
  
  abstract class TableDef(model: m.Table) extends super.TableDef(model){

    def compound(valuesOrTypes: Seq[String]): String = {
      if(hlistEnabled) valuesOrTypes.mkString(" :: ") + " :: HNil"
      else if (valuesOrTypes.size == 1) valuesOrTypes.head
      else if(valuesOrTypes.size <= 22) s"""(${valuesOrTypes.mkString(", ")})"""
      else throw new Exception("Cannot generate tuple for > 22 columns, please set hlistEnable=true or override compound.")
    }

    def factory   = if(columns.size == 1) TableClass.elementType else s"${TableClass.elementType}.tupled"
    def extractor = s"${TableClass.elementType}.unapply"

    trait EntityTypeDef extends super.EntityTypeDef{
      def code = 
        if(classEnabled){
          val args = columns.map(c=>
            c.default.map( v =>
              s"${c.name}: ${c.exposedType}=$v"
            ).getOrElse(
              s"${c.name}: ${c.exposedType}"
            )
          ).mkString(", ")
          val prns = (parents.take(1).map(" extends "+_) ++ parents.drop(1).map(" with "+_)).mkString("")
          s"""case class $name($args)$prns"""

        } else {
          s"type $name = $types"
        }
    }

    trait PlainSqlMapperDef extends super.PlainSqlMapperDef{
      def code = {
        val positional = compound(columnsPositional.map(c => (if(c.fakeNullable || c.model.nullable)s"<<?[${c.rawType}]"else s"<<[${c.rawType}]")))
        val dependencies = columns.map(_.rawType).toSet.toList.zipWithIndex.map{ case (t,i) => s"""e$i: GR[$t]"""}.mkString(", ")
        val rearranged = compound(desiredColumnOrder.map(i => if(hlistEnabled) s"r($i)" else tuple(i)))
        def result(args: String) = if(mappingEnabled) s"$factory($args)" else args
        val body =
          if(autoIncLastAsOption && columns.size > 1){
            s"""
val positional = $positional
import positional._
${result(rearranged)} // putting AutoInc last
            """.trim
          } else
           result(positional)
        s"""
implicit def ${name}(implicit $dependencies): GR[${TableClass.elementType}] = GR{
  prs => import prs._
  ${indent(body)}
}
        """.trim
      }
    }

    trait TableClassDef extends super.TableClassDef{
      def star = {
        val struct = compound(columns.map(c=>if(c.fakeNullable)s"${c.name}.?" else s"${c.name}"))
        val rhs = if(mappingEnabled) s"$struct <> ($factory, $extractor)" else struct
        s"def * = $rhs"
      }
      def option = {
        val struct = compound(columns.map(c=>if(c.model.nullable)s"${c.name}" else s"${c.name}.?"))
        val rhs = if(mappingEnabled) s"""$struct.shaped.<>($optionFactory, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))""" else struct
        s"def ? = $rhs"
      }
      def optionFactory = {
        val positionalAccessors = columns.zipWithIndex.map{ case(c,i) =>
          if(c.fakeNullable || c.model.nullable) tuple(i) else s"${tuple(i)}.get"
        }
        val fac = s"$factory(${compound(positionalAccessors)})"
        val discriminator = columns.zipWithIndex.collect{ case (c,i) if !c.model.nullable => s"${tuple(i)}" }.headOption
        val expr = discriminator.map(d => s"$d.map(_=> $fac)").getOrElse(s"None")
        s"{r=>import r._; $expr}"
      }
      def code = {
        val prns = parents.map(" with " + _).mkString("")
        val args = model.name.schema.map(n => s"""Some("$n")""") ++ Seq("\""+model.name.table+"\"")
        s"""
class $name(tag: Tag) extends Table[$elementType](tag, ${args.mkString(", ")})$prns {
  ${indent(body.map(_.mkString("\n")).mkString("\n\n"))}
}
        """.trim()
      }
    }

    trait TableValueDef extends super.TableValueDef{
      def code = s"lazy val $name = new TableQuery(tag => new ${TableClass.name}(tag))"
    }

    class ColumnDef(model: m.Column) extends super.ColumnDef(model){
      import ColumnOption._
      val PK1 = TableDef.this.PrimaryKey
      val PK2 = ColumnOption.PrimaryKey
      def columnOptionCode = {
        case PK2            => Some(s"O.PrimaryKey")
        case Default(value) => Some(s"O.Default(${default.get})") // .get is safe here
        case DBType(dbType) => Some(s"O.DBType($dbType)")
        case AutoInc        => Some(s"O.AutoInc")
        case NotNull|Nullable => throw new SlickException( s"Please don't use Nullable or NotNull column options. Use an Option type, respectively the nullable flag in Slick's model model Column." )
        case o => throw new SlickException( s"Don't know how to generate code for unexpected ColumnOption $o." )
      }
      def defaultCode = {
        case Some(v) => s"Some(${defaultCode(v)})"
        case s:String  => "\""+s+"\""
        case None      => s"None"
        case v:Int     => s"$v"
        case v:Long    => s"${v}L"
        case v:Float   => s"${v}F"
        case v:Double  => s"$v"
        case v:Boolean => s"$v"
        case v:Short   => s"$v"
        case v => throw new SlickException( s"Dont' know how to generate code for default value $v of ${v.getClass}" )
      }
      // Explicit type to allow overloading existing Slick method names.
      // Explicit type argument for better error message when implicit type mapper not found.
      def code = s"""val $name: Column[$actualType] = column[$actualType]("${model.name}"${options.map(", "+_).mkString("")})"""
    }

    class PrimaryKeyDef(model: m.PrimaryKey) extends super.PrimaryKeyDef(model){
      def code = s"""val $name = primaryKey("$dbName", ${compound(columns.map(_.name))})"""
    }

    class ForeignKeyDef(model: m.ForeignKey) extends super.ForeignKeyDef(model){
      def actionCode(action: ForeignKeyAction) = action match{
        case ForeignKeyAction.Cascade    => "ForeignKeyAction.Cascade"
        case ForeignKeyAction.Restrict   => "ForeignKeyAction.Restrict"
        case ForeignKeyAction.NoAction   => "ForeignKeyAction.NoAction"
        case ForeignKeyAction.SetNull    => "ForeignKeyAction.SetNull"
        case ForeignKeyAction.SetDefault => "ForeignKeyAction.SetDefault"
      }
      def code = {
        val fkColumns = compound(referencingColumns.map(_.name))
        val pkTable = referencedTable.TableValue.name
        val pkColumns = compound(referencedColumns.map(c => s"r.${c.name}"))
        s"""val $name = foreignKey("$dbName", $fkColumns, $pkTable)(r => $pkColumns, onUpdate=${onUpdate}, onDelete=${onDelete})"""
      }
    }

    class IndexDef(model: m.Index) extends super.IndexDef(model){
      def code = {
        val unique = if(model.unique) s", unique=true" else ""
        s"""val $name = index("$dbName", ${compound(columns.map(_.name))}$unique)"""
      }
    }
  }
}

trait StringGeneratorHelpers extends scala.slick.model.codegen.GeneratorHelpers[String,String,String]{
  def docWithCode(doc: String, code:String): String = (if(doc != "") "/** "+doc.split("\n").mkString("\n *  ")+" */\n" else "") + code
  final def optionType(t: String) = s"Option[$t]"
  def parseType(tpe: String): String = tpe
  def termName( name: String ) = if(scalaKeywords.contains(name)) "`"+name+"`" else name
  def typeName( name: String ) = if(scalaKeywords.contains(name)) "`"+name+"`" else name
}
