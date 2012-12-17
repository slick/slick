package scala.slick.jdbc.codegen

import scala.slick.jdbc.reflect
import scala.slick.session.Session
import scala.slick.jdbc.meta.CodeGen
import sys.process._
import java.io.File


class Schema( driver:String, schema:reflect.Schema, package_ :String )(implicit session:Session) extends GeneratorBase{
  val s = schema
  def table(t:reflect.Table) = new Table(this,t) 
  def tables : List[Table] = s.tables.map(table _)
  def driverImport = "import scala.slick.driver."+driver+"Driver.simple._" 
  def render = driverImport + lineBreak + s"""
package ${baseTablePackage}{
${indent(tables.map(_.renderBaseTable).mkString(lineBreak))}
}
package ${concreteTablePackage}{
${indent(tables.map(_.renderConcreteTable).mkString(lineBreak))}
}
package ${entitiesPackage}{
${indent(tables.map(_.renderEntity).mkString(lineBreak))}
}
""".trim()//+lineBreak+("-"*80)+lineBreak+tables.map(t => CodeGen.output2(t.table.t)).mkString(lineBreak+lineBreak)
  def renderBaseTables = tables.map(_.renderBaseTable).mkString(lineBreak)
  def renderConcreteTables = tables.map(_.renderConcreteTable).mkString(lineBreak)
  def renderEntities = tables.map(_.renderEntity).mkString(lineBreak)
  def entitiesPackage = package_ + ".entities"
  def baseTablePackage = package_ +".tables"+".base"
  def concreteTablePackage = package_ +".tables"
  private def dump( code:String, srcFolder:String, package_ :String, fileName:String ) {
    assert( new File(srcFolder).exists() )
    val folder : String = srcFolder + "/" + (package_.replace(".","/")) + "/"
    new File(folder).mkdirs()
    code.#>(new File( folder+fileName )).!
  } 
  def singleFile( srcFolder:String, fileName:String="" ) {
    dump( render, srcFolder, package_, if(fileName != "") fileName else "schema.scala" )
  }
  def fewFiles( srcFolder:String, concreteTablesFileName:String="", baseTablesFileName:String="", entitiesFileName:String="" ) {
    dump( driverImport + lineBreak + renderBaseTables, srcFolder, concreteTablePackage, if(concreteTablesFileName != "") concreteTablesFileName else "tables.scala" )
    dump( renderConcreteTables, srcFolder, baseTablePackage, if(baseTablesFileName != "") baseTablesFileName else "tables.scala" )
    dump( renderEntities, srcFolder, entitiesPackage, if(entitiesFileName != "") entitiesFileName else "entities.scala" )
  }
  def manyFiles( srcFolder:String ) {
    tables.foreach{ table =>
      dump( table.renderEntity, srcFolder, entitiesPackage, table.scalaName+".scala" )
      dump( table.renderConcreteTable, srcFolder, concreteTablePackage, table.scalaName+".scala" )
      dump( driverImport + lineBreak + table.renderBaseTable, srcFolder, baseTablePackage, table.entityName+".scala" )
    }
  }
}

protected trait GeneratorBase{
  val lineBreak = "\n"
  def indent( lines:Traversable[GeneratorBase] ) : String = render(lines).map(x=>s"  ${x}").mkString(lineBreak)
  def indent( s:String ) = s.split(lineBreak).map("  "+_).mkString(lineBreak)
  def commas( v:Traversable[String] ) = v.mkString(", ")
  def scalaName(name:String,cap:Boolean=true) = CodeGen.mkScalaName(name,cap) // can we do better than the cap parameter?
  def scalaType( sqlType : Int ) = CodeGen.scalaTypeFor(sqlType)
  def render : String
  def render(renderers:Traversable[GeneratorBase]) : List[String] = renderers.map(_.render).toList
}

class Column( table:Table,column:reflect.Column ) extends GeneratorBase{
  val t = table
  val c = column
  def _scalaType = scalaType(c.sqlType)
  def scalaType : String = c.nullable.filter(x=>x).map( _=>s"Option[${_scalaType}]" ).getOrElse(_scalaType)
  def scalaName : String = scalaName(name,false)
  def name = c.name
  def columnSize = c.columnSize.map(s=>s"(${s})").getOrElse("")
  def tpe(name:String) = s"""
    DBType "${name}${columnSize}"
   """.trim()
  def types = c.sqlTypeName.map( tpe _ )
  def autoIncrement = c.autoInc.filter(x=>x).map(_=>"AutoInc")
  def primaryKey = if(c.primaryKey) Some("PrimaryKey") else None
  def flags = (autoIncrement ++ primaryKey)
  def options = commas( (types ++ flags).map("O "+_) )
  def render = s"""
    def ${scalaName} = column[${scalaType}]("${name}", ${options})
  """.trim()
}

class Table (val schema:Schema,val table:reflect.Table)(implicit session:Session) extends GeneratorBase{
  val s = schema
  val t = table
  def name = t.name
  def scalaName : String = scalaName(name)
  def entityName : String = scalaName(name)
  def entityFullyQualifiedName : String = s.entitiesPackage+"."+entityName
  def column(c:reflect.Column) = new Column(this,c)
  def columns = t.columns.map(column _)
  def star = "def * = " + columns.map(_.scalaName).mkString(" ~ ")
  def mappedToFullyQualified = Some(entityFullyQualifiedName)
  def importEntity = mappedToFullyQualified.map("import "+_)
  def mappedToClassName = Some(entityName)
  def factory = mappedToClassName.get
  def extractor = mappedToClassName.get+".unapply _"
  def mapping = mappedToClassName.map(_=>s"<> (${factory}, ${extractor})").getOrElse("")
  def types = mappedToClassName.getOrElse(
    if(columns.length == 1) columns(0).scalaType
    else "("+columns.map(_.scalaType).mkString(",")+")" 
  )
  def foreignKeys = "" //many(foreignKey(key))
  def constraints = ""
  def render = s"""
package ${s.entitiesPackage}{
${indent(renderEntity)}
}
package ${s.baseTablePackage}{
${indent(renderBaseTable)}
}
package ${s.concreteTablePackage}{
${indent(renderConcreteTable)}
}
"""
  def renderBaseTable = importEntity.map(_+lineBreak).getOrElse("")+s"""
abstract class ${scalaName} extends Table[${types}]("${name}"){
  // columns
${indent(columns)}

  ${star} ${mapping}
  
  // foreign keys
  ${foreignKeys}
  
  // constraints
  ${constraints}
  
  
}
    """.trim()
  def renderConcreteTable = s"""
object ${scalaName} extends ${s.baseTablePackage}.${scalaName}
    """.trim()
  def renderEntity = s"""
case class ${entityName}( ${columns.map(c=>c.name+" :"+c.scalaType).mkString(", ")} )
    """.trim()
}
