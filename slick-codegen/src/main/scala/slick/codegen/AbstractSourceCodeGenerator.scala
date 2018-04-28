package slick.codegen

import slick.SlickException
import slick.ast.ColumnOption
import slick.{model => m}
import slick.model.ForeignKeyAction
import slick.relational.RelationalProfile
import slick.sql.SqlProfile

/** Base implementation for a Source code String generator */
abstract class AbstractSourceCodeGenerator(model: m.Model)
                   extends AbstractGenerator[String,String,String](model)
                   with StringGeneratorHelpers{
  /** Generates code for the complete model (not wrapped in a package yet)
      @group Basic customization overrides */
  def code = {
    "import slick.model.ForeignKeyAction\n" +
    ( if(tables.exists(_.hlistEnabled)){
        "import slick.collection.heterogeneous._\n"+
        "import slick.collection.heterogeneous.syntax._\n"
      } else ""
    ) +
    ( if(tables.exists(_.PlainSqlMapper.enabled)){
        "// NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.\n"+
        "import slick.jdbc.{GetResult => GR}\n"
      } else ""
    ) +
    codeForDDL +
    tables.map(_.code.mkString("\n")).mkString("\n\n")
  }

  /** Generates code for the container class (not wrapped in a package yet)
     @group Basic customization overrides */
  def codeForContainer = {
    "import slick.model.ForeignKeyAction\n" +
      ( if(tables.exists(_.hlistEnabled)){
            "import slick.collection.heterogeneous._\n"+
              "import slick.collection.heterogeneous.syntax._\n"
          } else ""
        ) +
      ( if(tables.exists(_.PlainSqlMapper.enabled)){
            "// NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.\n"+
              "import slick.jdbc.{GetResult => GR}\n"
          } else ""
        ) +
      codeForDDL
  }

  /**
   * Generates code for the DDL statement.
   * @group Basic customization overrides
   */
  def codeForDDL: String = {
    (if(ddlEnabled){
      "\n/** DDL for all tables. Call .create to execute. */" +
      (
        if(tables.length > 5)
          "\nlazy val schema: profile.SchemaDescription = Array(" + tables.map(_.TableValue.name + ".schema").mkString(", ") + ").reduceLeft(_ ++ _)"
        else if(tables.nonEmpty)
          "\nlazy val schema: profile.SchemaDescription = " + tables.map(_.TableValue.name + ".schema").mkString(" ++ ")
        else
          "\nlazy val schema: profile.SchemaDescription = profile.DDL(Nil, Nil)"
      ) +
      "\n@deprecated(\"Use .schema instead of .ddl\", \"3.0\")"+
      "\ndef ddl = schema" +
      "\n\n"
    } else "")
  }

  /** Generates a map that associates the table name with its generated code (not wrapped in a package yet).
   *  @group Basic customization overrides
   */
  def codePerTable: Map[String,String] = {
    tables.map(table => {
      val before="import slick.model.ForeignKeyAction\n" +
        (if (table.hlistEnabled) {
          "import slick.collection.heterogeneous._\n" +
            "import slick.collection.heterogeneous.syntax._\n"
        }
        else "") +
        (if (table.PlainSqlMapper.enabled) {
          "// NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.\n" +
            "import slick.jdbc.{GetResult => GR}\n"
        }
        else "")

      (table.TableValue.name,table.code.mkString(before,"\n",""))
    }).toMap
  }


  protected def tuple(i: Int) = termName(s"_${i+1}")

  abstract class TableDef(model: m.Table) extends super.TableDef(model){

    def compoundType(types: Seq[String]): String = {
      if(hlistEnabled){
        def mkHList(types: List[String]): String = types match {
          case Nil => "HNil"
          case e :: tail => s"HCons[$e," + mkHList(tail) + "]"
        }
        mkHList(types.toList)
      }
      else compoundValue(types)
    }

    def compoundValue(values: Seq[String]): String = {
      if(hlistEnabled) values.mkString(" :: ") + " :: HNil"
      else if (values.size == 1) values.head
      else if(values.size <= 22) s"""(${values.mkString(", ")})"""
      else throw new Exception("Cannot generate tuple for > 22 columns, please set hlistEnable=true or override compound.")
    }

    def factory   = if(columns.size == 1) TableClass.elementType else s"${TableClass.elementType}.tupled"
    def extractor = s"${TableClass.elementType}.unapply"

    trait EntityTypeDef extends super.EntityTypeDef{
      def code = {
        val args = columns.map(c=>
          c.default.map( v =>
            s"${c.name}: ${c.exposedType} = $v"
          ).getOrElse(
            s"${c.name}: ${c.exposedType}"
          )
        ).mkString(", ")
        if(classEnabled){
          val prns = (parents.take(1).map(" extends "+_) ++ parents.drop(1).map(" with "+_)).mkString("")
          (if(caseClassFinal) "final " else "") +
          s"""case class $name($args)$prns"""
        } else {
          if(columns.size > 254)
            s"type $name = $types" // constructor method would exceed JVM parameter limit
          else s"""
type $name = $types
/** Constructor for $name providing default values if available in the database schema. */
def $name($args): $name = {
  ${compoundValue(columns.map(_.name))}
}
          """.trim
        }
      }
    }

    trait PlainSqlMapperDef extends super.PlainSqlMapperDef{
      def code = {
        val positional = compoundValue(columnsPositional.map(c => (if(c.asOption || c.model.nullable)s"<<?[${c.rawType}]"else s"<<[${c.rawType}]")))
        val dependencies = columns.map(_.exposedType).distinct.zipWithIndex.map{ case (t,i) => s"""e$i: GR[$t]"""}.mkString(", ")
        val rearranged = compoundValue(desiredColumnOrder.map(i => if(hlistEnabled) s"r($i)" else tuple(i)))
        def result(args: String) = if(mappingEnabled) s"$factory($args)" else args
        val body =
          if(autoIncLast && columns.size > 1){
            s"""
val r = $positional
import r._
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
        val struct = compoundValue(columns.map(c=>if(c.asOption)s"Rep.Some(${c.name})" else s"${c.name}"))
        val rhs = if(mappingEnabled) s"$struct <> ($factory, $extractor)" else struct
        s"def * = $rhs"
      }
      def option = {
        val struct = compoundValue(columns.map(c=>if(c.model.nullable)s"${c.name}" else s"Rep.Some(${c.name})"))
        val rhs = if(mappingEnabled) s"""$struct.shaped.<>($optionFactory, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))""" else struct
        s"def ? = $rhs"
      }
      def optionFactory = {
        val accessors = columns.zipWithIndex.map{ case(c,i) =>
          val accessor = if(columns.size > 1) tuple(i) else "r"
          if(c.asOption || c.model.nullable) accessor else s"$accessor.get"
        }
        val fac = s"$factory(${compoundValue(accessors)})"
        val discriminator = columns.zipWithIndex.collect{ case (c,i) if !c.model.nullable => if(columns.size > 1) tuple(i) else "r" }.headOption
        val expr = discriminator.map(d => s"$d.map(_=> $fac)").getOrElse(s"None")
        if(columns.size > 1)
          s"{r=>import r._; $expr}"
        else
          s"r => $expr"
      }
      def code = {
        val prns = parents.map(" with " + _).mkString("")
        val args = model.name.schema.map(n => s"""Some("$n")""") ++ Seq("\""+model.name.table+"\"")
        s"""
class $name(_tableTag: Tag) extends profile.api.Table[$elementType](_tableTag, ${args.mkString(", ")})$prns {
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
      import RelationalProfile.ColumnOption._
      import SqlProfile.ColumnOption._
      def columnOptionCode = {
        case ColumnOption.PrimaryKey => Some(s"O.PrimaryKey")
        case Default(value)     => Some(s"O.Default(${default.get})") // .get is safe here
        case SqlType(dbType)    => Some(s"""O.SqlType("$dbType")""")
        case Length(length,varying) => Some(s"O.Length($length,varying=$varying)")
        case AutoInc            => Some(s"O.AutoInc")
        case Unique             => Some(s"O.Unique")
        case NotNull|Nullable   => throw new SlickException( s"Please don't use Nullable or NotNull column options. Use an Option type, respectively the nullable flag in Slick's model model Column." )
        case o => None // throw new SlickException( s"Don't know how to generate code for unexpected ColumnOption $o." )
      }
      def defaultCode = {
        case Some(v) => s"Some(${defaultCode(v)})"
        case s: String if rawType == "java.sql.Timestamp" => s
        case s:String  => "\""+s.replaceAll("\"", """\\"""")+"\""
        case None      => s"None"
        case v:Byte    => s"$v"
        case v:Int     => s"$v"
        case v:Long    => s"${v}L"
        case v:Float   => s"${v}F"
        case v:Double  => s"$v"
        case v:Boolean => s"$v"
        case v:Short   => s"$v"
        case v:Char   => s"'$v'"
        case v:BigDecimal => s"""scala.math.BigDecimal(\"$v\")"""
	case v: java.sql.Timestamp => s"""java.sql.Timestamp.valueOf("${v}")"""
        case v => throw new SlickException( s"Dont' know how to generate code for default value $v of ${v.getClass}. Override def defaultCode to render the value." )
      }
      // Explicit type to allow overloading existing Slick method names.
      // Explicit type argument for better error message when implicit type mapper not found.
      def code = s"""val $name: Rep[$actualType] = column[$actualType]("${model.name}"${options.map(", "+_).mkString("")})"""
    }

    class PrimaryKeyDef(model: m.PrimaryKey) extends super.PrimaryKeyDef(model){
      def code = s"""val $name = primaryKey("$dbName", ${compoundValue(columns.map(_.name))})"""
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
        val pkTable = referencedTable.TableValue.name
        val (pkColumns, fkColumns) = (referencedColumns, referencingColumns).zipped.map { (p, f) =>
          val pk = s"r.${p.name}"
          val fk = f.name
          if(p.model.nullable && !f.model.nullable) (pk, s"Rep.Some($fk)")
          else if(!p.model.nullable && f.model.nullable) (s"Rep.Some($pk)", fk)
          else (pk, fk)
        }.unzip
        s"""lazy val $name = foreignKey("$dbName", ${compoundValue(fkColumns)}, $pkTable)(r => ${compoundValue(pkColumns)}, onUpdate=${onUpdate}, onDelete=${onDelete})"""
      }
    }

    class IndexDef(model: m.Index) extends super.IndexDef(model){
      def code = {
        val unique = if(model.unique) s", unique=true" else ""
        s"""val $name = index("$dbName", ${compoundValue(columns.map(_.name))}$unique)"""
      }
    }
  }
}

trait StringGeneratorHelpers extends slick.codegen.GeneratorHelpers[String,String,String]{
  def docWithCode(doc: String, code:String): String = (if(doc != "") "/** "+doc.split("\n").mkString("\n *  ")+" */\n" else "") + code
  final def optionType(t: String) = s"Option[$t]"
  def parseType(tpe: String): String = tpe
  def shouldQuoteIdentifier(s: String) = {
    def isIdent =
      if(s.isEmpty) false
      else Character.isJavaIdentifierStart(s.head) && s.tail.forall(Character.isJavaIdentifierPart)
    scalaKeywords.contains(s) || !isIdent
  }
  def termName( name: String ) = if(shouldQuoteIdentifier(name)) "`"+name+"`" else name
  def typeName( name: String ) = if(shouldQuoteIdentifier(name)) "`"+name+"`" else name
}
